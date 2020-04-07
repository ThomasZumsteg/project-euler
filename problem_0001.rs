#[macro_use]
extern crate clap;
extern crate log;
extern crate env_logger;

use env_logger::Builder;
use log::{debug, info, LevelFilter};
use std::io::Write;

fn sums<'a, I>(range: I, multiples: &Vec<usize>) -> usize 
        where I: Iterator<Item=usize> {
    let mut total = 0;
    for n in range {
        for m in multiples {
            if n % m == 0 {
                debug!("Divisable {} % {} == 0", n, m);
                total += n;
                break
            }
        }
    }
    return total
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 1, https://projecteuler.net/problem=1")
        (@arg limit: -l --limit +takes_value "Highest integer to include")
        (@arg verbose: -v +multiple "Run the test suite")
        (@arg multiple: +multiple "Integers to") 
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

    let max = args.value_of("limit").map(|n| n.parse::<usize>().unwrap()).unwrap_or(1000);
    info!("Max values {}", max);

    let range = 1..max;
    let multiples = args
        .values_of("multiple")
        .map(|vec| vec
             .map(|v| v
                 .parse::<usize>()
                 .unwrap()
             )
             .collect()
        )
        .unwrap_or(vec![3, 5]);
    info!("Multiples {:?}", multiples);

    println!("{}", sums(range, &multiples));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic() {
        assert_eq!(sums((1..11), &vec![1]), 55);
    }

    #[test]
    fn test_evens() {
        assert_eq!(sums((1..11), &vec![2]), 30);
    }

    #[test]
    fn test_evens_and_threes() {
        assert_eq!(sums((1..11), &vec![2, 3]), 42);
    }

    #[test]
    fn test_mutiples() {
        assert_eq!(sums((1..11), &vec![]), 0);
    }
}
