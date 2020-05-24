#[macro_use]
extern crate clap;

use common::set_log_level;
use std::sync::mpsc::channel;
use threadpool::ThreadPool;

fn pow(base: usize, pow: usize, digits: u32) -> usize {
    let mut result = base;
    for _ in 1..pow {
        result = (result * base) % 10usize.pow(digits);
    }
    result
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 48, https://projecteuler.net/problem=48")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
        (@arg limit: +takes_value "Limit of self powers")
        (@arg digits: +takes_value "Number of digits to capture")
    ).get_matches();
    set_log_level(&args);

    let limit = args.value_of("limit").map(|n| n.parse::<usize>().unwrap()).unwrap_or(1000);
    let digits = args.value_of("digits").map(|n| n.parse::<u32>().unwrap()).unwrap_or(10);
    let threads = args.value_of("threads").map(|n| n.parse::<usize>().unwrap()).unwrap_or(1);

    let (tx, rx) = channel();
    let pool = ThreadPool::new(threads);
    for t in 0..threads {
        let tx = tx.clone();
        pool.execute(move || {
            let mut total = 0;
            for n in ((1+t)..(limit+1)).step_by(threads) {
                total = (total + pow(n, n, digits)) % 10usize.pow(digits);
            }
            tx.send(total).unwrap();
            drop(tx);
        });
    }
    drop(tx);
    println!("{}", rx.iter().sum::<usize>() % 10usize.pow(digits));
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_pow() {
        assert_eq!(pow(2, 2, 10), 4);
        assert_eq!(pow(3, 3, 10), 27);
        assert_eq!(pow(10, 10, 10), 0);
    }
}
