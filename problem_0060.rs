#[macro_use]
extern crate clap;

use common::set_log_level;
use common::primes::Primes;
use log::{debug, info};
use std::collections::{HashSet, HashMap};

fn has_set_of(size: usize, root: usize, map: HashMap<usize, HashSet<usize>>) -> bool {
    unimplemented!()
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 58, https://projecteuler.net/problem=58")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
        (@arg set_size: +takes_value "Number of primes to take")
    ).get_matches();
    set_log_level(&args);
    
    let set_size = args.value_of("set_size").map(|n| n.parse::<usize>().unwrap()).unwrap_or(5);

    let mut primes = Primes::new();
    let mut concat_primes: HashMap<usize, HashSet<usize>> = HashMap::new();
    for p in 1.. {
        let prime = primes.nth_prime(p);
        for q in 0..p {
            let qrime = primes.nth_prime(q);
            let crime = (prime.to_string() + &qrime.to_string()).parse::<usize>().unwrap();
            let drime = (qrime.to_string() + &prime.to_string()).parse::<usize>().unwrap();
            if primes.is_prime(crime) && primes.is_prime(drime) {
                info!("{} + {} = {}, {} + {} = {}", qrime, prime, drime, prime, qrime, crime);
                concat_primes.entry(qrime).or_insert(HashSet::new()).insert(prime);
                concat_primes.entry(prime).or_insert(HashSet::new()).insert(qrime);
                if has_set_of(set_size, prime, concat_primes) {
                    break;
                }
            }
        }
    }
    unimplemented!();
}
