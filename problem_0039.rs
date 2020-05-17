#[macro_use]
extern crate clap;

use common::{set_log_level, integer_square_root};
use log::info;
use std::collections::{HashMap, HashSet};

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 39, https://projecteuler.net/problem=39")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
        (@arg limit: "Search limit")
    ).get_matches();
    set_log_level(&args);

    let limit = args.value_of("limit").map(|n| n.parse::<usize>().unwrap()).unwrap_or(1000);
    info!("Limit {}", limit);

    let mut result: HashMap<usize, HashSet<Vec<usize>>> = HashMap::new();
    for a in 1..(limit/3) {
        for b in a..(limit-a)/2 {
            if let Some(c) = integer_square_root(a * a + b * b) {
                if a + b + c > limit {
                    break;
                }
                info!("{}: {}^2 + {}^2 = {}^2", a + b + c, a, b, c);
                result.entry(a + b + c)
                    .or_insert(HashSet::new())
                    .insert(vec![a, b, c]);
            }
        }
    }
    println!("{}", result.iter().max_by_key(|(_, v)| v.len()).unwrap().0);
}
