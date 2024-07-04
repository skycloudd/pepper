use serde::Deserialize;

#[derive(Debug, Deserialize)]
pub struct Config {
    pub target: Target,
}

#[derive(Debug, Deserialize)]
pub struct Target {
    #[serde(rename = "aarch64-apple-darwin")]
    pub aarch64_apple_darwin: Aarch64AppleDarwin,

    #[serde(rename = "x86_64-unknown-linux-gnu")]
    pub x86_64_unknown_linux_gnu: X8664UnknownLinuxGnu,
}

#[derive(Debug, Deserialize)]
pub struct Aarch64AppleDarwin {
    linker: Linker,
}

#[derive(Debug, Deserialize)]
pub struct X8664UnknownLinuxGnu {
    linker: Linker,
}

#[derive(Debug, Deserialize)]
pub struct Linker {
    pub command: String,
    pub flags: Vec<String>,
}

pub trait WithLinker {
    fn linker(&self) -> &Linker;
}

impl WithLinker for Aarch64AppleDarwin {
    fn linker(&self) -> &Linker {
        &self.linker
    }
}

impl WithLinker for X8664UnknownLinuxGnu {
    fn linker(&self) -> &Linker {
        &self.linker
    }
}
