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
        .format(|buf, record| writeln!(buf, "[{}:{}] {}", record.level(), record.module_path().unwrap_or("None"), record.args()))
        .init();
    info!("Set log level {}", log_level);
    log_level
}

pub mod primes {
    pub struct Primes {
        primes: Vec<usize>,
        pub current: usize,
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

        pub fn is_prime(&mut self, number: usize) -> bool {
            let mut last_prime: usize = *self.primes.last().unwrap();
            while last_prime * last_prime <= number {
                last_prime = self.next_prime();
                self.primes.push(last_prime);
            }
            for p in &self.primes { 
                if number % p == 0 && &number != p {
                    return false 
                } else if number < p * p {
                    return true 
                }
            }
            panic!("We sould never run out of primes");
        }

        fn next_prime(&self) -> usize {
            let mut num: usize = *self.primes.last().unwrap();
            loop {
                num += 2;
                for prime in &self.primes {
                    if num < prime * prime {
                        return num;
                    } else if num % prime == 0 {
                        break;
                    }
                }
            }
        }
    }

    impl Iterator for Primes {
        type Item = usize;

        fn next(&mut self) -> Option<usize> {
            while self.current >= self.primes.len() {
                let next_prime = self.next_prime();
                log::debug!("Found Prime {}: {}", self.current, next_prime);
                self.primes.push(next_prime);
            }
            self.current += 1;
            Some(self.primes[self.current-1])
        }
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn test_primes() {
            let mut primes = Primes::new();
            assert!(primes.is_prime(2));
            assert!(primes.is_prime(3));
            assert!(primes.is_prime(5));
            assert!(primes.is_prime(7));
            assert!(!primes.is_prime(4));
            assert!(!primes.is_prime(9));
            assert!(!primes.is_prime(100));
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

pub struct Fibonacci {
    curr: usize,
    next: usize,
}

impl Fibonacci {
    pub fn new() -> Fibonacci {
        Fibonacci { curr: 1, next: 1 }
    }
}

impl Iterator for Fibonacci {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        let next = self.curr + self.next;

        self.curr = self.next;
        self.next = next;

        Some(self.curr)
    }
}

pub mod digits {
    #[derive(Debug, Clone)]
    pub struct Digits {
        pub digits: Vec<usize>
    }

    impl Digits {
        pub fn filter(&self, digit: &usize) -> Digits {
            Digits {
                digits: self.digits.iter().map(|&d| d).filter(|d| d != digit).collect()
            }
        }
    }

    impl From<usize> for Digits {
        fn from(number: usize) -> Self {
            let mut digits = Vec::new();
            let mut remainer = number;
            while remainer > 0 {
                digits.insert(0, remainer % 10);
                remainer /= 10;
            }
            Digits { digits: digits }
        }
    }

    impl From<Digits> for usize {
        fn from(number: Digits) -> Self {
            let mut result = 0;
            for d in number.digits {
                result = result * 10 + d;
            }
            result
        }
    }
}
