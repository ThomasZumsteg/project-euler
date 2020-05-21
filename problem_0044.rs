#[macro_use]
extern crate clap;

use common::set_log_level;
use log::{info, debug};
use threadpool::ThreadPool;
use std::sync::{Arc, RwLock};


mod pentagonal {
    use common::integer_square_root;

    pub fn pentagonal_number(n: usize) -> usize {
        n * (3 * n - 1) / 2
    }
    
    pub fn pentagonal_index(number: usize) -> Option<usize> {
        if let Some(index) = integer_square_root(4 * 3 * 2 * number + 1) {
            if (index + 1) % 6 == 0 {
                return Some((index + 1) / 6);
            }
        }
        None
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn test_pentagon_numbers() {
            assert_eq!(pentagonal_number(1), 1);
            assert_eq!(pentagonal_number(2), 5);
            assert_eq!(pentagonal_number(3), 12);
            assert_eq!(pentagonal_number(4), 22);
            assert_eq!(pentagonal_number(5), 35);
            assert_eq!(pentagonal_number(6), 51);
        }

        #[test]
        fn test_is_pentagon() {
            assert_eq!(pentagonal_index(1), Some(1));
            assert_eq!(pentagonal_index(2), None);
            assert_eq!(pentagonal_index(3), None);
            assert_eq!(pentagonal_index(5), Some(2));
            assert_eq!(pentagonal_index(10), None);
            assert_eq!(pentagonal_index(12), Some(3));
        }
    }
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 44, https://projecteuler.net/problem=44")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
    ).get_matches();
    set_log_level(&args);

    let threads = args.value_of("threads").map(|n| n.parse::<usize>().unwrap()).unwrap_or(1);
    info!("Running with {} threads", threads);

    let result: Arc<RwLock<Option<usize>>> = Arc::new(RwLock::new(None));

    let pool = ThreadPool::new(threads);
    for t in 0..threads {
        let result = result.clone();
        pool.execute(move || {
            for n in ((t+1)..).step_by(threads) {
                let p_n = pentagonal::pentagonal_number(n);
                let read = result.read().unwrap().clone();
                if read.is_some() && read.unwrap() >= pentagonal::pentagonal_number(n+1) - p_n {
                    return;
                }
                for m in 1..n {
                    let p_m = pentagonal::pentagonal_number(m);
                    debug!("{}: Testing {} ± {}", t, p_n, p_m);
                    if pentagonal::pentagonal_index(p_n - p_m).is_some() && pentagonal::pentagonal_index(p_n + p_m).is_some() {
                        info!("{} ± {} is pentagonal", p_n, p_m);
                        if read.is_none() || read.unwrap() > p_n - p_m {
                            let mut write_result = result.write().unwrap();
                            if write_result.is_none() || write_result.unwrap() > p_n - p_m {
                                *write_result = Some(p_n - p_m);
                            }
                        }
                    }
                }
            }
        });
    }
    pool.join();
    println!("{}", result.read().unwrap().unwrap());
}
