#[macro_use]
extern crate clap;

use common::set_log_level;
use log::info;
use num::bigint::BigUint;

fn select_r_from_n(n: usize, r: usize) -> BigUint {
    // (n!/r!) * (1/(n-r)!)
    // r <= n
    assert!(r <= n);
    if n == r {
        return BigUint::from(1usize);
    }
    let n_fact_div_r_fact = ((r+1)..(n+1)).fold(BigUint::from(1usize), |acc, i| acc * BigUint::from(i));
    let n_minus_r_fact = (1..(n+1-r)).fold(BigUint::from(1usize), |acc, i| acc * BigUint::from(i));
    info!("{} / {}", n_fact_div_r_fact, n_minus_r_fact);
    n_fact_div_r_fact / n_minus_r_fact
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 53, https://projecteuler.net/problem=53")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
        (@arg search_limit: +takes_value "limit")
        (@arg size_limit: +takes_value "limit")
    ).get_matches();
    set_log_level(&args);

    let search_limit = args.value_of("search_limit").map(|n| n.parse::<usize>().unwrap()).unwrap_or(100);
    let size_limit = args.value_of("size_limit")
        .map(|n| BigUint::from(n.parse::<usize>().unwrap()))
        .unwrap_or(BigUint::from(1000000usize));

    let mut total = 0;
    for n in 1..(search_limit+1) {
        for r in 1..(n+1) {
            if select_r_from_n(n, r) > size_limit {
                total += 1;
            }
        }
    }
    println!("{}", total);
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_select_r_from_n() {
        assert_eq!(select_r_from_n(5, 3), 10);
        assert_eq!(select_r_from_n(23, 10), 1144066);
        assert_eq!(select_r_from_n(23, 10), 1144066);
        assert_eq!(select_r_from_n(43, 43), 1);
        assert_eq!(select_r_from_n(1, 1), 1);
    }

    #[test]
    #[should_panic]
    fn test_panic() {
        select_r_from_n(1, 2);
    }
}
