#[macro_use]
extern crate clap;

use common::set_log_level;
use std::collections::{HashMap, HashSet};
use num::BigInt;
use log::{debug, info};

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 62, https://projecteuler.net/problem=62")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
        (@arg size: +takes_value "Size of set")
        (@arg power: +takes_value "Exponent")
    ).get_matches();
    set_log_level(&args);

    let size = args.value_of("size").map(|n| n.parse::<usize>().unwrap()).unwrap_or(5);
    let power = args.value_of("power").map(|n| n.parse::<u32>().unwrap()).unwrap_or(3);

    let mut permutations: HashMap<Vec<char>, HashSet<BigInt>> = HashMap::new();
    let mut result = None;

    for num in (1..).map(|m| BigInt::from(m).pow(power)) {
        debug!("{:?}", num);
        let mut digits: Vec<char> = num.to_string().chars().collect();
        digits.sort();
        let entry = permutations.entry(digits).or_insert(HashSet::new());
        entry.insert(num);
        if entry.len() >= size {
            info!("{:?}", entry);
            result = Some(entry.clone());
            break;
        }
    }
    println!("{}", result.unwrap().iter().min().unwrap().to_string());
}
