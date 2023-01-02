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
        default = "./b-compiler-config.toml"
    )
)]
struct InternalConfig {
    #[source(clap(long), env, config)]
    ir: bool,
    #[source(clap(long, short), env, config)]
    arch: Option<String>,
    #[source(clap(long, short), env, config)]
    target_platform_name: Option<String>,
    #[source(clap(long), env, config)]
    enable_continue: bool,
    #[source(clap(long, short), env, config)]
    heap_size: u64,
    #[source(clap(long, short), env, config)]
    stack_size: u64,
}

#[derive(Copy, Clone)]
pub struct Config {
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
            ir,
            arch,
            target_platform_name,
            enable_continue,
            heap_size,
            stack_size,
        } = config;

        let target_platform = if target_platform_name.is_none() {
            TargetPlatform::native()
        } else {
            let platform_name = match target_platform_name.as_deref() {
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
            ir,
            target_platform,
            enable_continue,
            heap_size,
            stack_size,
        })
    }
}
