pub mod primes {
    pub struct Primes {
        primes: Vec<usize>,
        current: usize,
    }

    impl Primes {
        pub fn new() -> Primes {
            Primes {
                primes: Vec::new(),
                current: 1,
            }
        }
    }

    impl Iterator for Primes {
        type Item = usize;

        fn next(&mut self) -> Option<usize> {
            loop {
                self.current += if self.current > 2 { 2 } else { 1 };
                if self.primes.iter().all(|p| self.current % p != 0) {
                    self.primes.push(self.current);
                    break;
                }
            }
            Some(self.current)
        }
    }
}
