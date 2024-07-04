# Pepper

[![Rust](https://github.com/skycloudd/pepper/actions/workflows/rust.yml/badge.svg)](https://github.com/skycloudd/pepper/actions/workflows/rust.yml)

A small toy programming language that compiles to native executables.

## Example

```rs
fn main() -> int =
    f(10) - df(2, 10) - 11

fn f(x: int) -> int =
    x * x

fn df(n: int, x: int) -> int =
    n * x
```

## Usage

```sh
# compile the program
$ cargo run -- examples/program.pr

# run the executable
$ ./a.out

# exit code should be 69
$ echo $?
```

## Supported targets

- aarch64-apple-darwin
- x86_64-unknown-linux-gnu
