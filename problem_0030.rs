#[macro_use]
extern crate clap;

use common::set_log_level;
use log::{debug, info};
use std::sync::mpsc::channel; 
use threadpool::ThreadPool;

fn get_digits(num: usize) -> Vec<usize> {
    let mut remainer = num;
    let mut result = Vec::new();
    while remainer > 0 {
        result.push(remainer % 10);
        remainer /= 10;
    }
    result
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 30, https://projecteuler.net/problem=30")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg digits: "digits")
        (@arg threads: -t --threads +takes_value "threads")
    ).get_matches();
    set_log_level(&args);

    let digits = args.value_of("digits").map(|n| n.parse::<u32>().unwrap()).unwrap_or(5);
    let threads = args.value_of("threads").map(|n| n.parse::<usize>().unwrap()).unwrap_or(1);
    let mut max = 9;
    while max < get_digits(max).iter().map(|d| d.pow(digits)).sum() {
        max = max * 10 + 9;
    }
    info!("Limit: {}", max);

    let pool = ThreadPool::new(threads);
    let (tx, rx) = channel();
    for t in 0..threads {
        let tx = tx.clone();
        pool.execute(move || {
            for num in ((10 +t)..max).step_by(threads) {
                if get_digits(num).iter().map(|d| d.pow(digits)).sum::<usize>() == num {
                    debug!("Matches {}", num);
                    tx.send(num).unwrap();
                }
            }
            drop(tx);
        });
    }
    drop(tx);

    println!("{}", rx.iter().sum::<usize>());
}
