#[macro_use]
extern crate clap;

use common::set_log_level;
use common::primes::Primes;
use log::info;
use std::sync::{Arc, RwLock};
use threadpool::ThreadPool;

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 50, https://projecteuler.net/problem=50")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
        (@arg limit: +takes_value "Limit of search")
    ).get_matches();
    set_log_level(&args);

    let limit = args.value_of("limit").map(|n| n.parse::<usize>().unwrap()).unwrap_or(1000000);
    let threads = args.value_of("threads").map(|n| n.parse::<usize>().unwrap()).unwrap_or(1);

    let primes: Arc<Vec<usize>> = Arc::new(Primes::new().take_while(|&p| p <= limit).collect());
    let pool = ThreadPool::new(threads);
    let result: Arc<RwLock<Option<(usize, usize)>>> = Arc::new(RwLock::new(None));
    for t in 0..threads {
        let primes = primes.clone();
        let result = result.clone();
        pool.execute(move || {
            for (p, &prime) in primes.iter().enumerate().filter(|(p, _)| p % threads == t) {
                let mut total = prime;
                for (len, &q) in primes[(p+1)..].iter().enumerate() {
                    total += q;
                    if total > limit {
                        break;
                    }
                    if let Ok(_) = primes.binary_search(&total) {
                        info!("({}) {} - {}", t, total, len);
                        let mut write = result.write().unwrap();
                        if write.is_none() || write.unwrap().1 < len {
                            *write = Some((total, len));
                        }
                    }
                }
            }
        });
    }
    pool.join();
    println!("{}", result.read().unwrap().unwrap().0);
}
