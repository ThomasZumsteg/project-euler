#[macro_use]
extern crate clap;
extern crate threadpool;

use log::debug;
use common::set_log_level;
use threadpool::ThreadPool;
use std::sync::mpsc::channel; 

struct Collatz(usize);

impl Collatz {
    fn new(num: usize) -> Collatz {
        Collatz(num)
    }
}

impl Iterator for Collatz {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        if self.0 <= 1 { return None };
        self.0 = if self.0 % 2 == 0 { self.0 / 2 } else { self.0 * 3 + 1 };
        Some(self.0)
    }
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 14, https://projecteuler.net/problem=14")
        (@arg threads: -t --threads +takes_value "Threads")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg limit: "Large number") 
    ).get_matches();
    set_log_level(&args);
    let threads = args.value_of("threads").map(|n| n.parse::<usize>().unwrap()).unwrap_or(1);
    let max = args.value_of("limit").map(|n| n.parse::<usize>().unwrap()).unwrap_or(1000000);

    let pool = ThreadPool::new(threads);
    let (tx, rx) = channel();
    for t in 0..threads {
        let tx = tx.clone();
        pool.execute(move || {
            let lower = t * max / threads + 1;
            let upper = (t + 1) * max / threads;
            let result: (usize, usize) = (lower..upper)
                .map(|n| (n, Collatz::new(n).count()))
                .max_by_key(|&(_, c)| c)
                .unwrap();
            debug!("Thread: {}, result: {:?}", t, result);
            tx.send(result).expect("Channel will be there waiting for the pool");
        });
    }
    let (longest, _) = rx.iter().take(threads).max_by_key(|&(_, c)| c).unwrap();
    println!("{}", longest);
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_collatz() {
        assert_eq!(
            Collatz::new(13).collect::<Vec<usize>>(),
            vec![40, 20, 10, 5, 16, 8, 4, 2, 1]
        );
    }
}
