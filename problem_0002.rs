#[macro_use]
extern crate clap;
extern crate log;
extern crate env_logger;

use env_logger::Builder;
use log::{debug, info, LevelFilter};
use std::io::Write;

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
