#[macro_use]
extern crate clap;

use common::set_log_level;
use log::{debug, info};
use std::sync::mpsc::channel;
use threadpool::ThreadPool;


fn is_binary_palindrome(num: usize) -> bool {
    let binary = format!("{:b}", num);
    let len = binary.len() / 2;
    let first = &binary[..len];
    let last = &binary[(binary.len() - len)..].chars().rev().collect::<String>();
    first == last
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 36, https://projecteuler.net/problem=36")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg limit: "limit")
        (@arg threads: -t --threads +takes_value "threads")
    ).get_matches();
    set_log_level(&args);

    let threads = args.value_of("threads").map(|n| n.parse::<usize>().unwrap()).unwrap_or(1);
    let limit = args.value_of("limit").map(|n| n.parse::<usize>().unwrap()).unwrap_or(1000000);
    let base_limit = 10usize.pow(limit.to_string().len() as u32 / 2) as usize;
    info!("Base Limit: {}", base_limit);
    info!("Numeric limit: {}", limit);

    let pool = ThreadPool::new(threads);
    let (tx, rx) = channel();
    for t in 0..threads {
        info!("{} - {}-{}", t, t, base_limit);
        let tx = tx.clone();
        pool.execute(move || {
            for base in (t..base_limit).step_by(threads) {
                debug!("{}", base);
                let str_num = base.to_string();
                let str_mun = str_num.chars().rev().collect::<String>();
                for pal in vec![str_num.clone() + &str_mun, str_num.clone() + &str_mun[1..]] {
                    let pal_num = pal.parse::<usize>().unwrap();
                    if pal_num > limit {
                        continue
                    }
                    if is_binary_palindrome(pal_num) {
                        info!("{}: {}", t, pal_num);
                        tx.send(pal_num).unwrap();
                    }
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
    fn test_binary() {
        assert!(is_binary_palindrome(585));
        assert!(!is_binary_palindrome(101));
    }
}
