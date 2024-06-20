use error::Error;

pub mod error;

#[salsa::accumulator]
pub struct Diagnostics(pub Error);
