use anyhow::{Error, Result};
use config_manager::config;
use config_manager::ConfigInit;

use crate::utils::{Arch, PlatformName, TargetPlatform};

#[config(
    clap(version, author),
    env_prefix = "b_compiler",
    file(
        format = "toml",
        clap(long = "config", short = 'c', help = "path to configuration file"),
        env = "b_compiler_config",
        optional = true
    )
)]
struct InternalConfig {
    #[source(clap(long, short), env, config)]
    input: String,
    #[source(clap(long), env, config, default = false)]
    ir: bool,
    #[source(clap(long, short), env, config, default)]
    arch: Option<String>,
    #[source(clap(long, short), env, config, default)]
    target_platform: Option<String>,
    #[source(clap(long), env, config, default = false)]
    enable_continue: bool,
    #[source(clap(long), env, config, default = 65536)]
    heap: u64,
    #[source(clap(long), env, config, default = 4096)]
    stack: u64,
}

#[derive(Clone)]
pub struct Config {
    pub input: String,
    pub ir: bool,
    pub(crate) target_platform: TargetPlatform,
    pub(crate) enable_continue: bool,
    pub(crate) heap_size: u64,
    pub(crate) stack_size: u64,
}

impl Config {
    pub fn try_parse() -> Result<Self> {
        let config = InternalConfig::parse()?;

        let InternalConfig {
            input,
            ir,
            arch,
            target_platform,
            enable_continue,
            heap: heap_size,
            stack: stack_size,
        } = config;

        let target_platform = if target_platform.is_none() {
            TargetPlatform::native()
        } else {
            let platform_name = match target_platform.as_deref() {
                Some("linux") => PlatformName::Linux,
                Some("win" | "windows") => PlatformName::Windows,
                Some(target_platform) => Err(Error::msg(format!(
                    "the target platform {target_platform} is either unknown or unsupported",
                )))?,
                _ => unreachable!(),
            };
            let arch = match arch.as_deref() {
                None => TargetPlatform::default().arch,
                Some("x86-64" | "x64" | "x86_64" | "amd64") => Arch::x86_64,
                Some(arch) => Err(Error::msg(format!(
                    "the architecture {arch} is either unknown or unsupported",
                )))?,
            };
            TargetPlatform {
                platform_name,
                arch,
            }
        };

        Ok(Self {
            input,
            ir,
            target_platform,
            enable_continue,
            heap_size,
            stack_size,
        })
    }
}
