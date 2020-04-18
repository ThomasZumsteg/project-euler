#[macro_use]
extern crate clap;

use common::set_log_level;
use log::{debug, info};

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
        (@arg verbose: -v +multiple "Increase log level")
        (@arg multiple: +multiple "Integers to divide by") 
    ).get_matches();
    set_log_level(&args);

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
        assert_eq!(sums(1..11, &vec![1]), 55);
    }

    #[test]
    fn test_evens() {
        assert_eq!(sums(1..11, &vec![2]), 30);
    }

    #[test]
    fn test_evens_and_threes() {
        assert_eq!(sums(1..11, &vec![2, 3]), 42);
    }

    #[test]
    fn test_mutiples() {
        assert_eq!(sums(1..11, &vec![]), 0);
    }
}
