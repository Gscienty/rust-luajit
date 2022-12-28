mod binopr;
mod unopr;

mod ctrlpc;
mod emit;
mod exp;
mod func;
mod lex;
mod stmt;
mod var;

mod block;
mod fscope;

use block::*;
use fscope::*;

mod errors;
mod parse;
pub(crate) use errors::*;
pub(crate) use parse::*;

#[cfg(test)]
mod stmt_test;
