#[macro_use]
extern crate clap;

use common::set_log_level;
use log::{debug, info};

struct Fibonacci {
    curr: usize,
    next: usize,
}

impl Fibonacci {
    fn new() -> Fibonacci {
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

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 2, https://projecteuler.net/problem=2")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg limit: "Highest fibbonacci number") 
    ).get_matches();
    set_log_level(&args);

    let max = args.value_of("limit").map(|n| n.parse::<usize>().unwrap()).unwrap_or(4000000);
    info!("Max value {}", max);

    println!("{}", Fibonacci::new()
        .take_while(|&f| f <= max)
        .filter(|f| {
            debug!("Divisable {}", f);
            f % 2 == 0
        })
        .sum::<usize>()
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fib() {
        let mut f = Fibonacci::new();
        assert_eq!(f.next().unwrap(), 1);
        assert_eq!(f.next().unwrap(), 2);
        assert_eq!(f.next().unwrap(), 3);
        assert_eq!(f.next().unwrap(), 5);
        assert_eq!(f.next().unwrap(), 8);
    }
}
