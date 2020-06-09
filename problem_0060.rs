#[macro_use]
extern crate clap;
extern crate maplit;

use common::set_log_level;
use common::primes::Primes;
use log::{debug, info};
use std::collections::{HashSet, HashMap};

fn find_valid_set(candidates: HashSet<usize>, map: &HashMap<usize, HashSet<usize>>, size: usize) -> Option<HashSet<usize>> {
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
    let mut result = None;
    let mut p = 0;
    while result.is_none() {
        p += 1;
        let prime = primes.nth_prime(p);
        for q in 0..p {
            let qrime = primes.nth_prime(q);
            let crime = (prime.to_string() + &qrime.to_string()).parse::<usize>().unwrap();
            let drime = (qrime.to_string() + &prime.to_string()).parse::<usize>().unwrap();
            if primes.is_prime(crime) && primes.is_prime(drime) {
                debug!("{} + {} = {}, {} + {} = {}", qrime, prime, drime, prime, qrime, crime);
                concat_primes.entry(qrime).or_insert(HashSet::new()).insert(prime);
                concat_primes.entry(prime).or_insert(HashSet::new()).insert(qrime);
                let candidates = vec![prime, qrime].iter().map(|n| n.clone()).collect::<HashSet<usize>>();
                result = find_valid_set(candidates, &concat_primes, set_size);
            }
        }
    }
    let result = result.unwrap();
    info!("{:?}", result);
    println!("{}", result.iter().sum::<usize>());
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_has_set() {
        assert_eq!(
            hashmap!{
                3 => hashset!{7, 109, 673},
                7 => hashset!{3, 109, 673},
                109 => hashset!{3, 7, 673},
                673 => hashset!{3, 7, 109}
            },
            Some(hashset!{3, 7, 109, 673}),
        )
    }
}
