#[macro_use]
extern crate clap;

use common::set_log_level;
use common::digits::Digits;
use log::{info, debug};
use std::sync::mpsc::channel;
use threadpool::ThreadPool;

fn factorial(num: usize) -> usize {
    if num == 0 {
        return 1;
    }
    (1..num).fold(num, |acc, n| acc * n)
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 34, https://projecteuler.net/problem=34")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "Threads")
    ).get_matches();
    set_log_level(&args);

    let threads = args.value_of("threads").map(|n| n.parse::<usize>().unwrap()).unwrap_or(1);

    let pool = ThreadPool::new(threads);
    let (tx, rx) = channel();
    for t in 0..threads {
        let tx = tx.clone();
        pool.execute(move || {
            for number in ((3+t)..(8 * factorial(9))).step_by(threads) {
                let digits = Digits::from(number);
                debug!("{:?} == {}", digits.digits, number);
                if digits.digits.iter().fold(0, |acc, &n| acc + factorial(n)) == number {
                    info!("{}", number);
                    tx.send(number).unwrap();
                }
            }
            drop(tx);
        });
    }
    drop(tx);
    println!("{}", rx.iter().sum::<usize>());
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_exponents() {
        assert_eq!(factorial(0), 1);
        assert_eq!(factorial(1), 1);
        assert_eq!(factorial(2), 2);
        assert_eq!(factorial(3), 6);
        assert_eq!(factorial(4), 24);
        assert_eq!(factorial(5), 120);
        assert_eq!(factorial(6), 720);
        assert_eq!(factorial(7), 5040);
        assert_eq!(factorial(8), 40320);
        assert_eq!(factorial(9), 362880);
    }
}
