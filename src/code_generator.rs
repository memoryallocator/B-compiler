use std::*;
use rc::{Rc, Weak};
use collections::{HashMap, HashSet};

use crate::lexical_analyzer::token;
use token::{Constant, ConstantType, TokenPos};
use crate::config;
use config::*;
use crate::parser;
use parser::ast::*;

#[derive(Debug, Copy, Clone)]
pub(crate) enum GlobalDefinition {
    Variable,
    Function { parameter_list_length: Option<usize> },
}

#[derive(Debug, Clone)]
enum NameIsImportedBecause {
    FnCall(Weak<FunctionCallNode>),
    ExplicitImport(Weak<ExternDeclarationNode>),
}

#[derive(Debug, Clone)]
struct GlobalDefinitionInfo {
    definition: GlobalDefinition,
    import: Option<NameIsImportedBecause>,
    defined_here: Option<Weak<DefinitionNode>>,
}

#[derive(Debug, Clone)]
enum Name {
    GlobalDefinition {},
    Local((Weak<AutoDeclarationNode>, usize)),
    Parameter,
    Label,
}

#[derive(Debug, Default, Clone)]
pub(crate) struct Scope {
    curr_fn: Weak<FunctionDefinitionNode>,
    globals: HashMap<Rc<String>, GlobalDefinitionInfo>,
    locals: HashMap<Rc<String>, (Weak<AutoDeclaration>, usize)>,
    labels: HashMap<Rc<String>, Weak<LabelDeclarationNode>>,
    breakable_statement: Option<BreakableStatement>,
}

pub(crate) struct CodeGenerator<'a> {
    pub(crate) compiler_options: CompilerOptions,
    pub(crate) standard_library_names: &'a HashSet<StandardLibraryName>,
    pub(crate) source_code: Option<&'a str>,
}

impl CodeGenerator<'_> {
    pub(crate) fn run(&self, program_node: &ProgramNode) -> (Vec<Warning>, Result<String, Error>) {
        struct StringPool {
            data: HashMap<Rc<String>, usize>,
            prefix: &'static str,
        }

        let mut string_pool = StringPool {
            data: Default::default(),
            prefix: "..@STR_POOL",
        };

        let mut scope = Scope {
            globals: self.standard_library_names
                .into_iter()
                .map(|name| {
                    let defined_here = None;

                    match name {
                        StandardLibraryName::Function {
                            fn_name,
                            parameter_list_length
                        } =>
                            (fn_name.clone(), GlobalDefinitionInfo {
                                definition: GlobalDefinition::Function {
                                    parameter_list_length: *parameter_list_length
                                },
                                import: None,
                                defined_here,
                            }),

                        StandardLibraryName::Variable { var_name } => {
                            (var_name.clone(), GlobalDefinitionInfo {
                                definition: GlobalDefinition::Variable,
                                import: None,
                                defined_here,
                            })
                        }
                    }
                }).collect(),
            ..Default::default()
        };

        let mut warnings = Vec::<Warning>::new();

        fn reserve_word(compiler_options: CompilerOptions) -> &'static str {
            match compiler_options.target_platform.arch {
                Arch::x86_32 => "resd",
                Arch::x86_64 => "resq"
            }
        }

        fn decl_word(compiler_options: CompilerOptions) -> &'static str {
            match compiler_options.target_platform.arch {
                Arch::x86_32 => "dd",
                Arch::x86_64 => "dq"
            }
        }

        fn mangle_name(s: &str) -> String {
            let s = s.replace('.', "@");
            "$".to_owned() + &*s
        }

        trait GenerateCode {
            fn generate_code(
                self: &Rc<Self>,
                scope: &mut Scope,
                compiler_options: CompilerOptions,
                str_pool: &mut StringPool,
                warnings: &mut Vec<Warning>,
            ) -> Result<String, Error>;
        }

        impl GenerateCode for Constant {
            fn generate_code(
                self: &Rc<Self>,
                scope: &mut Scope,
                compiler_options: CompilerOptions,
                str_pool: &mut StringPool,
                warnings: &mut Vec<Warning>,
            ) -> Result<String, Error> {
                match self.constant_type {
                    ConstantType::Octal => {
                        Ok("0q".to_owned() + &**self.value)
                    }
                    ConstantType::Decimal => {
                        Ok(self.value.to_string())
                    }
                    ConstantType::Char => {
                        todo!()
                    }
                    ConstantType::String => {
                        let value = &self.value;
                        let data_len = str_pool.data.len();

                        let idx =
                            if let Some(idx) = str_pool.data.get(value) {
                                *idx
                            } else {
                                str_pool.data.insert(value.clone(), data_len);
                                data_len
                            };

                        Ok(str_pool.prefix.to_owned() + &idx.to_string())
                    }
                }
            }
        }

        impl GenerateCode for ConstantNode {
            fn generate_code(
                self: &Rc<Self>,
                scope: &mut Scope,
                compiler_options: CompilerOptions,
                str_pool: &mut StringPool,
                warnings: &mut Vec<Warning>,
            ) -> Result<String, Error> {
                self.constant.generate_code(scope, compiler_options, str_pool, warnings)
            }
        }

        impl GenerateCode for Ival {
            fn generate_code(
                self: &Rc<Self>,
                scope: &mut Scope,
                compiler_options: CompilerOptions,
                str_pool: &mut StringPool,
                warnings: &mut Vec<Warning>,
            ) -> Result<String, Error> {
                match self.as_ref() {
                    Ival::Constant(constant) =>
                        constant.generate_code(scope, compiler_options, str_pool, warnings),

                    Ival::Name(name, pos) => {
                        if let Some(_) = scope.globals.get(name) {
                            Ok(mangle_name(name.as_str()))
                        } else {
                            return Err(Error::NameNotDefined {
                                name: name.clone(),
                                pos: *pos,
                            });
                        }
                    }
                }
            }
        }

        impl GenerateCode for VariableDefinitionNode {
            fn generate_code(
                self: &Rc<Self>,
                scope: &mut Scope,
                compiler_options: CompilerOptions,
                str_pool: &mut StringPool,
                warnings: &mut Vec<Warning>,
            ) -> Result<String, Error> {
                let mut res = String::new();

                res.push_str("section .data\n");
                res.push_str(&mangle_name(self.name.as_str()));
                res.push_str(": ");

                res.push_str(decl_word(compiler_options));
                res.push(' ');

                if let Some(ival) = &self.initial_value {
                    res.push_str(&*ival.generate_code(scope, compiler_options,
                                                      str_pool, warnings)?)
                } else {
                    res.push('0');
                }

                res.push('\n');
                Ok(res)
            }
        }

        impl GenerateCode for VectorDefinitionNode {
            fn generate_code(
                self: &Rc<Self>,
                scope: &mut Scope,
                compiler_options: CompilerOptions,
                str_pool: &mut StringPool,
                warnings: &mut Vec<Warning>,
            ) -> Result<String, Error> {
                if let Some(const_node) = &self.specified_size {
                    if let Constant {
                        constant_type: ConstantType::String, ..
                    } = const_node.as_ref().constant.as_ref() {
                        return Err(Error::VecSizeIsString(VecDeclOrDef::Def(self.clone())));
                    }
                }

                let mut res = String::new();

                res.push_str("section .data\n");
                res.push_str(&mangle_name(self.name.as_str()));
                res.push_str(": ");

                let decl_word = decl_word(compiler_options);

                let ivals_len =
                    if let Some(ivals) = &self.initial_values {
                        for ival in ivals {
                            res.push_str(decl_word);
                            res.push(' ');

                            res.push_str(&*ival.generate_code(scope, compiler_options,
                                                              str_pool, warnings)?);
                            res.push('\n');
                        }

                        ivals.len()
                    } else {
                        0
                    };

                let act_size =
                    if let Some(sp_size) = &self.specified_size {
                        sp_size.generate_code(scope, compiler_options,
                                              str_pool, warnings)? + " + 1"
                    } else {
                        if ivals_len == 0 {
                            warnings.push(Warning::VecWithNoSizeAndInits(self.clone()));
                        }
                        "1".to_owned()
                    };

                let values_left = format!("{} - {}", act_size, ivals_len);
                let init_code = format!(
                    "%if ({}) >= 0
times({}) {}
%endif",
                    values_left, values_left, decl_word);

                res.push_str(&*init_code);
                res.push('\n');
                Ok(res)
            }
        }

        impl GenerateCode for CompoundStatementNode {
            fn generate_code(
                self: &Rc<Self>,
                scope: &mut Scope,
                compiler_options: CompilerOptions,
                str_pool: &mut StringPool,
                warnings: &mut Vec<Warning>,
            ) -> Result<String, Error> {
                let mut res = String::new();

                for stmt in &self.statement_list {
                    res += &*stmt.generate_code(scope, compiler_options, str_pool, warnings)?;
                }

                Ok(res)
            }
        }

        fn generate_code_for_name(
            name: &Rc<String>,
            pos: TokenPos,
            scope: &Scope,
            output_reg: &str,
        ) -> Result<String, Error> {
            if let Some(
                GlobalDefinitionInfo { import: Some(_), .. }
            ) = scope.globals.get(name) {
                return Ok(format!("mov {}, {}\n", output_reg, mangle_name(name)));
            }

            if let Some((_, offset)) = scope.locals.get(name) {
                return Ok(format!("mov {}, rbp-{}\n", output_reg, offset));
            }

            if let Some(_) = scope.labels.get(name) {
                let full_name =
                    mangle_name(&**scope.curr_fn.upgrade().unwrap().name)
                        + "."
                        + &*mangle_name(name);

                return Ok(format!("mov {}, {}\n", output_reg, full_name));
            }

            Err(Error::NameNotDefined { name: name.clone(), pos })
        }

        fn generate_code_for_rvalue_node(
            rvalue_node: &Rc<RvalueNode>,
            scope: &mut Scope,
            compiler_options: CompilerOptions,
            str_pool: &mut StringPool,
            warnings: &mut Vec<Warning>,
            output_reg: &str,
        ) -> Result<String, Error> {
            match rvalue_node.rvalue.as_ref() {
                Rvalue::Constant(constant) => {
                    let constant = constant.generate_code(scope, compiler_options,
                                                          str_pool, warnings)?;
                    Ok(format!("mov {}, {}\n", output_reg, constant))
                }

                Rvalue::Lvalue(lv) => {
                    let address_code = generate_code_for_lvalue_node(lv,
                                                                     scope,
                                                                     compiler_options,
                                                                     str_pool,
                                                                     warnings,
                                                                     output_reg)?;

                    Ok(address_code + &*format!("mov {}, [{}]\n", output_reg, output_reg))
                }

                Rvalue::Assign { lhs, assign, rhs } => {
                    let lhs_output_reg = "r9";
                    let rhs_output_reg = "rcx";

                    let lhs_code = generate_code_for_lvalue_node(lhs,
                                                                 scope,
                                                                 compiler_options,
                                                                 str_pool,
                                                                 warnings,
                                                                 lhs_output_reg)?;

                    let rhs_code = generate_code_for_rvalue_node(rhs,
                                                                 scope,
                                                                 compiler_options,
                                                                 str_pool,
                                                                 warnings,
                                                                 rhs_output_reg)?;
                    match assign.bin_op {
                        None => {
                            Ok(rhs_code + "\n"
                                + &*lhs_code + "\n"
                                + &*format!("mov [{}], {}\n", lhs_output_reg, rhs_output_reg)
                                + &*format!("mov {}, [{}]\n", output_reg, lhs_output_reg))
                        }
                        Some(bin_op) => {
                            todo!()
                        }
                    }
                }
                _ => todo!()
            }
        }

        fn generate_code_for_lvalue_node(
            lvalue_node: &Rc<LvalueNode>,
            scope: &mut Scope,
            compiler_options: CompilerOptions,
            str_pool: &mut StringPool,
            warnings: &mut Vec<Warning>,
            output_reg: &str,
        ) -> Result<String, Error> {
            let pos = lvalue_node.position;

            match lvalue_node.lvalue.as_ref() {
                Lvalue::Name(name) => {
                    Ok(generate_code_for_name(&name, pos, scope, output_reg)?
                        + &*format!("mov {}, {}\n",
                                    output_reg, output_reg))
                }

                Lvalue::DerefRvalue(rv) => {
                    let res = generate_code_for_rvalue_node(rv,
                                                            scope,
                                                            compiler_options,
                                                            str_pool,
                                                            warnings,
                                                            output_reg)?;

                    Ok(res + &*format!("mov {}, [{}]\n", output_reg, output_reg))
                }

                Lvalue::Indexing { vector, index } => {
                    todo!()
                }
            }
        }

        impl GenerateCode for ReturnNode {
            fn generate_code(
                self: &Rc<Self>,
                scope: &mut Scope,
                compiler_options: CompilerOptions,
                str_pool: &mut StringPool,
                warnings: &mut Vec<Warning>,
            ) -> Result<String, Error> {
                let mut res = String::new();

                if let Some(rv) = &self.rvalue {
                    res += &*generate_code_for_rvalue_node(rv,
                                                           scope,
                                                           compiler_options,
                                                           str_pool,
                                                           warnings,
                                                           "rax")?;
                }

                res += "leave\n";
                res += "ret\n";

                Ok(res)
            }
        }

        impl GenerateCode for Statement {
            fn generate_code(
                self: &Rc<Self>,
                scope: &mut Scope,
                compiler_options: CompilerOptions,
                str_pool: &mut StringPool,
                warnings: &mut Vec<Warning>,
            ) -> Result<String, Error> {
                fn check_name_was_not_declared(
                    name: &Rc<String>,
                    pos: TokenPos,
                    scope: &Scope,
                ) -> Result<(), Error> {
                    if let Some((prev, _)) = scope.locals.get(name) {
                        return Err(Error::NameRedefined {
                            name: name.clone(),
                            curr_def_pos: pos,
                            prev_def_pos: Some(prev.upgrade().unwrap().position),
                        });
                    }

                    if let Some(prev) = scope.labels.get(&*name) {
                        return Err(Error::NameRedefined {
                            name: name.clone(),
                            curr_def_pos: pos,
                            prev_def_pos: Some(prev.upgrade().unwrap().position),
                        });
                    }

                    if let Some(
                        GlobalDefinitionInfo {
                            import,
                            defined_here, ..
                        }) = scope.globals.get(name) {
                        if import.is_some() {
                            return Err(Error::NameRedefined {
                                name: name.clone(),
                                curr_def_pos: pos,
                                prev_def_pos: if let Some(def) = defined_here {
                                    Some(def.upgrade().unwrap().get_position())
                                } else {
                                    None
                                },
                            });
                        }
                    }

                    Ok(())
                }

                Ok(match self.as_ref() {
                    Statement::NullStatement => "nop\n".to_string(),
                    Statement::Compound(comp_stmt) => {
                        comp_stmt.generate_code(scope, compiler_options, str_pool, warnings)?
                    }

                    Statement::Declaration(decl_node) => {
                        let curr_stmt =
                            match decl_node.as_ref() {
                                DeclarationNode::AutoDeclaration(auto_decl) => {
                                    for (
                                        decl, (pos, _)
                                    ) in &auto_decl.declarations {
                                        check_name_was_not_declared(&decl.name, *pos, scope)?;

                                        scope.locals.insert(decl.name.clone(),
                                                            (Rc::downgrade(&decl),
                                                             scope.locals.len()));
                                    }

                                    String::new()
                                }

                                DeclarationNode::ExternDeclaration(extern_decl) => {
                                    for (name, (pos, _)) in &extern_decl.names {
                                        check_name_was_not_declared(&name, *pos, scope)?;

                                        if let Some(
                                            GlobalDefinitionInfo {
                                                import,
                                                defined_here, ..
                                            }) = scope.globals.get_mut(name) {
                                            *import = Some(NameIsImportedBecause::ExplicitImport(
                                                Rc::downgrade(extern_decl)));
                                        }
                                    }

                                    String::new()
                                }

                                DeclarationNode::LabelDeclaration(label) => {
                                    assert!(scope.labels.contains_key(&*label.label_name));

                                    format!(".{}:\n", mangle_name(&**label.label_name))
                                }
                            };

                        let next_stmt = decl_node.get_next_statement();
                        let next_stmt = next_stmt.generate_code(scope,
                                                                compiler_options,
                                                                str_pool,
                                                                warnings)?;
                        curr_stmt + &next_stmt
                    }

                    Statement::Return(ret) => {
                        ret.generate_code(scope, compiler_options, str_pool, warnings)?
                    }

                    Statement::RvalueAndSemicolon(rv) => {
                        generate_code_for_rvalue_node(&rv.rvalue,
                                                      scope,
                                                      compiler_options,
                                                      str_pool,
                                                      warnings,
                                                      "rax")?
                    }

                    _ => todo!()
                })
            }
        }

        impl GenerateCode for StatementNode {
            fn generate_code(
                self: &Rc<Self>,
                scope: &mut Scope,
                compiler_options: CompilerOptions,
                str_pool: &mut StringPool,
                warnings: &mut Vec<Warning>,
            ) -> Result<String, Error> {
                self.statement.generate_code(scope, compiler_options, str_pool, warnings)
            }
        }

        impl GenerateCode for FunctionDefinitionNode {
            fn generate_code(
                self: &Rc<Self>,
                scope: &mut Scope,
                compiler_options: CompilerOptions,
                str_pool: &mut StringPool,
                warnings: &mut Vec<Warning>,
            ) -> Result<String, Error> {
                let mut scope = scope.clone();
                scope.curr_fn = Rc::downgrade(self);

                let body_code = self.body.generate_code(&mut scope, compiler_options,
                                                        str_pool, warnings)?;

                let mut res = format!("{}:\n", mangle_name(&**self.name));
                res.push_str("push rbp\n");
                res.push_str("mov rbp, rsp\n");

                let target = compiler_options.target_platform;
                let stack_size = format!("{} * {}", scope.locals.len(),
                                         match target.arch {
                                             Arch::x86_32 => 4,
                                             Arch::x86_64 => 8,
                                         });
                res.push_str(&format!("sub rsp, {}\n", stack_size));

                if target.platform_name == PlatformName::Linux {
                    for reg in ["rdi", "rsi", "rdx", "rcx", "r8", "r9"].iter().rev() {
                        res.push_str(&format!("push {}\n", reg));
                    }
                } else {
                    todo!()
                }

                res.push_str(&body_code);

                res.push_str(&*Rc::new(ReturnNode {
                    rvalue: None
                }).generate_code(&mut scope, compiler_options, str_pool, warnings)?);

                Ok(res)
            }
        }

        trait FindLabels {
            fn find_labels(
                self: &Rc<Self>,
                scope: &mut Scope,
                labels: &mut HashMap<Rc<String>, Weak<LabelDeclarationNode>>,
            ) -> Result<(), Error>;
        }

        impl FindLabels for CompoundStatementNode {
            fn find_labels(
                self: &Rc<Self>,
                scope: &mut Scope,
                labels: &mut HashMap<Rc<String>, Weak<LabelDeclarationNode>>,
            ) -> Result<(), Error> {
                for stmt in &self.statement_list {
                    stmt.find_labels(scope, labels)?;
                }

                Ok(())
            }
        }

        impl FindLabels for DeclarationNode {
            fn find_labels(
                self: &Rc<Self>,
                scope: &mut Scope,
                labels: &mut HashMap<Rc<String>, Weak<LabelDeclarationNode>>,
            ) -> Result<(), Error> {
                match self.as_ref() {
                    DeclarationNode::AutoDeclaration(auto_decl) => {
                        auto_decl.next_statement.find_labels(scope, labels)
                    }

                    DeclarationNode::ExternDeclaration(extern_decl) => {
                        extern_decl.next_statement.find_labels(scope, labels)
                    }

                    DeclarationNode::LabelDeclaration(label_decl) => {
                        let name = &label_decl.label_name;
                        let mut scope = scope.clone();

                        if let Some(prev) = scope.globals.get(name) {
                            if let Some(import) = &prev.import {
                                return Err(Error::NameRedefined {
                                    name: name.clone(),
                                    curr_def_pos: label_decl.position,
                                    prev_def_pos: Some(match import {
                                        NameIsImportedBecause::FnCall(fn_call) => {
                                            fn_call.upgrade().unwrap().fn_name.position
                                        }

                                        NameIsImportedBecause::ExplicitImport(extern_decl) => {
                                            extern_decl.upgrade().unwrap().position
                                        }
                                    }),
                                });
                            }
                        }

                        if let Some((auto_decl, _)) = scope.locals.get(name) {
                            return Err(Error::NameRedefined {
                                name: name.clone(),
                                curr_def_pos: label_decl.position,
                                prev_def_pos: Some(auto_decl.upgrade().unwrap().position),
                            });
                        }

                        if let Some(
                            prev
                        ) = scope.curr_fn.upgrade().unwrap().parameters.get(name) {
                            return Err(Error::NameRedefined {
                                name: name.clone(),
                                curr_def_pos: label_decl.position,
                                prev_def_pos: Some(scope.curr_fn.upgrade().unwrap()
                                    .parameters.get(name).unwrap().0),
                            });
                        }

                        assert!(labels.insert(name.clone(),
                                              Rc::downgrade(label_decl)).is_none());
                        Ok(())
                    }
                }
            }
        }

        impl FindLabels for SwitchNode {
            fn find_labels(
                self: &Rc<Self>,
                scope: &mut Scope,
                labels: &mut HashMap<Rc<String>, Weak<LabelDeclarationNode>>,
            ) -> Result<(), Error> {
                self.body.find_labels(scope, labels)
            }
        }

        impl FindLabels for CaseNode {
            fn find_labels(
                self: &Rc<Self>,
                scope: &mut Scope,
                labels: &mut HashMap<Rc<String>, Weak<LabelDeclarationNode>>,
            ) -> Result<(), Error> {
                self.next_statement.find_labels(scope, labels)
            }
        }

        impl FindLabels for IfNode {
            fn find_labels(
                self: &Rc<Self>,
                scope: &mut Scope,
                labels: &mut HashMap<Rc<String>, Weak<LabelDeclarationNode>>,
            ) -> Result<(), Error> {
                self.body.find_labels(scope, labels)?;

                if let Some(else_body) = &self.else_body {
                    else_body.find_labels(scope, labels)?;
                }

                Ok(())
            }
        }

        impl FindLabels for WhileNode {
            fn find_labels(
                self: &Rc<Self>,
                scope: &mut Scope,
                labels: &mut HashMap<Rc<String>, Weak<LabelDeclarationNode>>,
            ) -> Result<(), Error> {
                self.body.find_labels(scope, labels)
            }
        }

        impl FindLabels for Statement {
            fn find_labels(
                self: &Rc<Self>,
                scope: &mut Scope,
                labels: &mut HashMap<Rc<String>, Weak<LabelDeclarationNode>>,
            ) -> Result<(), Error> {
                match self.as_ref() {
                    Statement::Compound(comp_stmt) =>
                        comp_stmt.find_labels(scope, labels),

                    Statement::Declaration(decl_node) =>
                        decl_node.find_labels(scope, labels),

                    Statement::Switch(sw) =>
                        sw.find_labels(scope, labels),

                    Statement::Case(cs) =>
                        cs.find_labels(scope, labels),

                    Statement::If(r#if) =>
                        r#if.find_labels(scope, labels),

                    Statement::While(wh) =>
                        wh.find_labels(scope, labels),

                    _ => Ok(()),
                }
            }
        }

        impl FindLabels for StatementNode {
            fn find_labels(
                self: &Rc<Self>,
                scope: &mut Scope,
                labels: &mut HashMap<Rc<String>, Weak<LabelDeclarationNode>>,
            ) -> Result<(), Error> {
                self.statement.find_labels(scope, labels)
            }
        }

        impl FindLabels for FunctionDefinitionNode {
            fn find_labels(
                self: &Rc<Self>,
                scope: &mut Scope,
                labels: &mut HashMap<Rc<String>, Weak<LabelDeclarationNode>>,
            ) -> Result<(), Error> {
                self.body.find_labels(scope, labels)
            }
        }

        fn add_global_name(
            def: &Rc<DefinitionNode>,
            scope: &mut Scope,
            warnings: &mut Vec<Warning>,
        ) -> Result<(), Error> {
            let (name, def_type) =
                match def.as_ref() {
                    DefinitionNode::Variable(var_def) =>
                        (var_def.name.clone(), GlobalDefinition::Variable),
                    DefinitionNode::Vector(vec_def) =>
                        (vec_def.name.clone(), GlobalDefinition::Variable),
                    DefinitionNode::Function(fn_def) =>
                        (fn_def.name.clone(), GlobalDefinition::Function {
                            parameter_list_length: Some(fn_def.parameters.len())
                        }),
                };

            if let Some(prev_def) = scope.globals.insert(
                name.clone(),
                GlobalDefinitionInfo {
                    definition: def_type,
                    import: None,
                    defined_here: Some(Rc::downgrade(def)),
                },
            ) {
                match prev_def {
                    GlobalDefinitionInfo {
                        definition: prev_def_type,
                        import,
                        defined_here,
                    } => {
                        assert!(import.is_none());

                        if let Some(prev_def) = defined_here {
                            return Err(Error::NameRedefined {
                                name,
                                curr_def_pos: def.get_position(),
                                prev_def_pos: Some(prev_def.upgrade().unwrap().get_position()),
                            });
                        } else {
                            warnings.push(Warning::StandardNameRedefined(def.clone()));
                        }
                    }
                }
            }

            Ok(())
        }

        let mut res = String::new();

        for def in program_node.get_definitions() {
            if let Err(error) = add_global_name(def, &mut scope, &mut warnings) {
                return (warnings, Err(error));
            }
        }

        for def in program_node.get_definitions() {
            let gen =
                match def.as_ref() {
                    DefinitionNode::Variable(var_def) => {
                        var_def.generate_code(&mut scope.clone(),
                                              self.compiler_options,
                                              &mut string_pool,
                                              &mut warnings)
                    }

                    DefinitionNode::Vector(vec_def) => {
                        vec_def.generate_code(&mut scope.clone(),
                                              self.compiler_options,
                                              &mut string_pool,
                                              &mut warnings)
                    }

                    DefinitionNode::Function(fn_def) => {
                        let mut labels = HashMap::<Rc<String>, Weak<LabelDeclarationNode>>::new();

                        let mut scope = scope.clone();
                        scope.curr_fn = Rc::downgrade(fn_def);

                        if let Err(error) = fn_def.find_labels(&mut scope,
                                                               &mut labels) {
                            return (warnings, Err(error));
                        }
                        scope.labels = labels;

                        fn_def.generate_code(&mut scope,
                                             self.compiler_options,
                                             &mut string_pool,
                                             &mut warnings)
                    }
                };

            if let Ok(code) = gen {
                res.push_str(code.as_str());
            } else {
                return (warnings, Err(gen.err().unwrap()));
            }
        }

        res.push_str(".rodata:\n");
        for (s, idx) in string_pool.data {
            todo!();
            res.push_str(&*format!("{}{}", string_pool.prefix, idx));
        }

        let target = self.compiler_options.target_platform;
        match target {
            TargetPlatform { platform_name: PlatformName::Linux, .. } => {
                if !scope.globals.contains_key(&"_start".to_string()) {
                    res.push_str(".text:\n");

                    res.push_str("global _start\n");
                    res.push_str("_start:\n");
                    res.push_str("call main\n");

                    res.push_str("xor rdi, rdi\n");
                    res.push_str("mov rax, 60\n");
                    res.push_str("syscall");
                }
            }
            _ => todo!()
        }

        (warnings, Ok(res))
    }
}
