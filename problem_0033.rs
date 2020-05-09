#[macro_use]
extern crate clap;
extern crate num;

use common::set_log_level;
use common::digits::Digits;
use log::debug;
use num::integer::gcd;
use std::collections::HashSet;

fn remove_duplicate_digits(numuerator: usize, denominator: usize) -> HashSet<(usize, usize)> {
    let num = Digits::from(numuerator);
    let den = Digits::from(denominator);
    let num_digits = num.digits.iter().map(|n| n.to_owned()).collect::<HashSet<usize>>();
    let den_digits = den.digits.iter().map(|d| d.to_owned()).collect::<HashSet<usize>>();
    let mut result = HashSet::new();
    for n in num_digits {
        if n == 0 || !den_digits.contains(&n){
            continue;
        }
        result.insert((usize::from(num.filter(&n)), usize::from(den.filter(&n))));
    }
    result
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 33, https://projecteuler.net/problem=33")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "Threads")
    ).get_matches();
    set_log_level(&args);

    let threads = args.value_of("threads").map(|n| n.parse::<usize>().unwrap()).unwrap_or(1);
    let mut numberator = 1;
    let mut denominator = 1;
    for den in 1..100 {
        for num in 1..den {
            for (n, d) in remove_duplicate_digits(num, den) {
                if d == 0 || n == 0 {
                    continue
                }
                if num * d == den * n {
                    numberator *= n;
                    denominator *= d;
                    debug!("{}/{} = {}/{}", num, den, n, d);
                }
            }
        }
    }
    let common = gcd(numberator, denominator);
    println!("{}", denominator / common);
}
