use serde::Deserialize;

#[derive(Debug, Deserialize)]
pub struct Config {
    pub target: Target,
}

#[derive(Debug, Deserialize)]
pub struct Target {
    #[serde(rename = "aarch64-apple-darwin")]
    pub aarch64_apple_darwin: Aarch64AppleDarwin,
}

#[derive(Debug, Deserialize)]
pub struct Aarch64AppleDarwin {
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
