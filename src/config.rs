use std::collections::HashMap;

use anyhow::Result;

use config_manager::ConfigInit;
use config_manager::config;

#[derive(Debug)]
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
    #[source(clap(long, short), env, config)]
    out: String,
    #[source(clap(long, short), env, config)]
    ir: bool,
    #[source(clap(long, short), env, config)]
    arch: Arch,
    #[source(clap(long, short), env, config)]
    platform: TargetPlatform,
    #[source(clap(long, short), env, config)]
    enable_continue: bool,
    #[source(clap(long, short), env, config)]
    heap: u64,
    #[source(clap(long, short), env, config)]
    stack: u64,
}

pub(crate) struct Config {

}

impl Config {
    pub(crate) fn try_parse() -> Result<Self> {
        let config = InternalConfig::parse()?;
        todo!()
    }
}