extern crate clap;

use clap::ArgMatches;
use env_logger::Builder;
use log::{info, LevelFilter};
use std::collections::HashSet;
use std::io::Write;

pub fn set_log_level(args: &ArgMatches) -> LevelFilter {
    let log_level = match args.occurrences_of("verbose") {
        0 => LevelFilter::Off,
        1 => LevelFilter::Error,
        2 => LevelFilter::Warn,
        3 => LevelFilter::Info,
        4 => LevelFilter::Debug,
        _ => LevelFilter::Trace,
    };
    Builder::new()
        .filter_level(log_level)
        .format(|buf, record| writeln!(buf, "[{}] {}", record.level(), record.args()))
        .init();
    info!("Set log level {}", log_level);
    log_level
}

pub mod primes {
    pub struct Primes {
        primes: Vec<usize>,
        current: usize,
    }

    impl Primes {
        pub fn new() -> Primes {
            const SIZE: usize = 1000000;
            let mut block = [true; SIZE] ;
            let mut primes = Vec::new();
            log::debug!("Starting seive {}", SIZE);
            for i in 2..SIZE {
                if block[i] {
                    log::debug!("Seived Prime: {} {}", primes.len(), i);
                    primes.push(i);
                    for i in ((2*i)..SIZE).step_by(i) {
                        block[i] = false;
                    }
                }
            }
            log::debug!("Finished seive {}", SIZE);
            Primes {
                primes: primes,
                current: 0,
            }
        }
    }

    impl Iterator for Primes {
        type Item = usize;

        fn next(&mut self) -> Option<usize> {
            let mut num: usize = self.primes.last().unwrap() + 2;
            while self.current >= self.primes.len() {
                let mut is_prime = true;
                for prime in &self.primes {
                    if num < prime * prime {
                        break;
                    } else if num % prime == 0 {
                        is_prime = false;
                        break;
                    }
                }
                if is_prime {
                    log::debug!("Found Prime {}: {}", self.current, num);
                    self.primes.push(num);
                }
                num += 2;
            }
            self.current += 1;
            Some(self.primes[self.current-1])
        }
    }
}

pub fn find_divisors(num: usize) -> HashSet<usize> {
    let mut result = HashSet::new();
    for n in 1.. {
        if n * n > num {
            break
        }
        if num % n == 0 {
            result.insert(n);
            result.insert(num / n);
        }
    };
    result
}
