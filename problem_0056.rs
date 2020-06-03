#[macro_use]
extern crate clap;

use common::set_log_level;
use num::bigint::{BigInt, BigUint};
use num_traits::Pow;
use std::cmp::max;
use std::sync::mpsc::channel;
use threadpool::ThreadPool;

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 56, https://projecteuler.net/problem=56")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
        (@arg limit: +takes_value "limit of powers and bases")
    ).get_matches();
    set_log_level(&args);

    let threads = args.value_of("threads").map(|n| n.parse::<usize>().unwrap()).unwrap_or(1);
    let limit = args.value_of("limit").map(|n| n.parse::<usize>().unwrap()).unwrap_or(100);

    let (tx, rx) = channel();
    let pool = ThreadPool::new(threads);
    for t in 0..threads {
        let tx = tx.clone();
        pool.execute(move || {
            let mut sum_max = 0; 
            for a in ((t+1)..(limit+1)).step_by(threads) {
                let base = BigInt::from(a);
                for b in 1..(limit+1) {
                    let exponent = BigUint::from(b);
                    let sum = base.pow(&exponent).to_string().chars().map(|c| c.to_digit(10).unwrap()).sum();
                    sum_max = max(sum, sum_max);
                }
            }
            tx.send(sum_max).unwrap();
            drop(tx);
        });
    }
    drop(tx);
    println!("{}", rx.iter().max().unwrap());
}
