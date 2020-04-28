#[macro_use]
extern crate clap;

use common::{set_log_level, Fibonacci};

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 25, https://projecteuler.net/problem=25")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg digits: "First fibonacci number with n digits")
    ).get_matches();
    set_log_level(&args);

    let digits = args.value_of("digits").map(|n| n.parse::<u32>().unwrap()).unwrap_or(1000);
    println!("{}", Fibonacci::new()
        .enumerate()
        .take_while(|(_, fib)| *fib < 10usize.pow(digits))
        .next()
        .unwrap()
        .1
    );
}
