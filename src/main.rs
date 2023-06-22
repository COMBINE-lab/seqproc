use chumsky::prelude::*;
use std::time::Instant;

fn main() {
    let args: seqproc::Args = argh::from_env();

    let start = Instant::now();
    match seqproc::parser().parse(args.geom) {
        Ok(read_description) => seqproc::interpret(
            args.file1,
            args.file2,
            args.out1,
            args.out2,
            read_description,
        ),
        Err(errs) => println!("Error: {:?}", errs),
    }
    let duration = start.elapsed();
    println!("tranformation completed in {:.2}s", duration.as_secs_f32());
}
