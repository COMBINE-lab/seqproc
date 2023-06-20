use chumsky::prelude::*;
use std::{env, fs};

fn main() {
    let src = fs::read_to_string(env::args().nth(1).unwrap()).unwrap();
    let infile = env::args().nth(2).unwrap();
    let outfile = env::args().nth(3).unwrap();

    match seqproc::parser().parse(src) {
        Ok(read_description) => seqproc::interpret(infile, outfile, read_description),
        Err(errs) => println!("Error: {:?}", errs),
    }
}
