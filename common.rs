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

pub fn integer_square_root(number: usize) -> Option<usize> {
    let root = (number as f64).sqrt() as usize;
    if root * root == number { Some(root) } else { None }
}

#[cfg(test)]
mod test {
    use super::integer_square_root;
    
    #[test]
    fn test_integer_square_root() {
        assert_eq!(integer_square_root(12), None);
        assert_eq!(integer_square_root(4), Some(2));
        assert_eq!(integer_square_root(1), Some(1));
        assert_eq!(integer_square_root(7), None);
    }
}

pub mod primes {
    use std::collections::HashMap;

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
            if number < 2 {
                return false;
            }
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

        pub fn nth_prime(&mut self, nth: usize) -> usize {
            while nth >= self.primes.len() {
                self.primes.push(self.next_prime());
            }
            self.primes[nth]
        }

        pub fn prime_factors(&mut self, number: usize) -> HashMap<usize, usize> {
            let mut result = HashMap::new();
            let mut remainer = number;
            for n in 0.. {
                if self.is_prime(remainer) {
                    *result.entry(remainer).or_insert(0) += 1;
                    break;
                } else if remainer == 1 {
                    break;
                }
                let prime = self.nth_prime(n);
                while remainer % prime == 0 {
                    *result.entry(prime).or_insert(0) += 1;
                    remainer /= prime;
                }
            }
            result
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
        use maplit::hashmap;

        #[test]
        fn test_primes() {
            let mut primes = Primes::new();
            assert!(primes.is_prime(2));
            assert!(primes.is_prime(3));
            assert!(primes.is_prime(5));
            assert!(primes.is_prime(7));
            assert!(!primes.is_prime(0));
            assert!(!primes.is_prime(1));
            assert!(!primes.is_prime(4));
            assert!(!primes.is_prime(9));
            assert!(!primes.is_prime(100));
            assert!(!primes.is_prime(68341));
            assert!(!primes.is_prime(41683));
        }

        #[test]
        fn test_prime_factors() {
            let mut primes = Primes::new();
            assert_eq!(primes.prime_factors(4), hashmap!{2 => 2});
            assert_eq!(primes.prime_factors(7), hashmap!{7 => 1});
            assert_eq!(primes.prime_factors(14), hashmap!{2 => 1, 7 => 1});
            assert_eq!(primes.prime_factors(15), hashmap!{3 => 1, 5 => 1});
            assert_eq!(primes.prime_factors(644), hashmap!{2 => 2, 7 => 1, 23 => 1});
            assert_eq!(primes.prime_factors(645), hashmap!{3 => 1, 5 => 1, 43 => 1});
            assert_eq!(primes.prime_factors(646), hashmap!{2 => 1, 17 => 1, 19 => 1});
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
    use num::bigint::BigInt;

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

    impl From<BigInt> for Digits {
        fn from(number: BigInt) -> Self {
            Digits { 
                digits: number.to_string().chars().map(|d| d.to_digit(10).unwrap() as usize).collect()
            }
        }
    }

    impl From<Digits> for BigInt {
        fn from(number: Digits) -> Self {
            let mut result = BigInt::from(0);
            for d in number.digits {
                result = result * BigInt::from(10) + BigInt::from(d);
            }
            result
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

    impl From<Vec<usize>> for Digits {
        fn from(digits: Vec<usize>) -> Self {
            Digits { digits: digits }
        }
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn test_from_big_int() {
            assert_eq!(Digits::from(BigInt::from(123456)).digits, vec![1,2,3,4,5,6]);
            assert_eq!(Digits::from(BigInt::from(0)).digits, vec![0]);
            assert_eq!(Digits::from(BigInt::from(10)).digits, vec![1, 0]);
        }
    }
}
