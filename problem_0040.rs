#[macro_use]
extern crate clap;

use common::set_log_level;
use common::digits::Digits;
use log::{debug, info};

struct ChampernowneConstant {
    buffer: Vec<usize>,
    current: usize,
}

impl ChampernowneConstant {
    fn new() -> ChampernowneConstant {
        ChampernowneConstant {
            buffer: Vec::new(),
            current: 0,
        }
    }
}

impl Iterator for ChampernowneConstant {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        if self.buffer.len() <= 0 {
            self.current += 1;
            self.buffer.extend(Digits::from(self.current).digits);
        }
        Some(self.buffer.remove(0))
    }
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 40, https://projecteuler.net/problem=40")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
        (@arg digits: "Digits to multiply")
    ).get_matches();
    set_log_level(&args);
    let mut selected = args.values_of("digits")
        .map(|v| v.map(|n| n.parse::<usize>().unwrap()).collect::<Vec<usize>>())
        .unwrap_or(vec![1, 10, 100, 1000, 10000, 100000, 1000000]);
    let mut total = 1;
    selected.sort();
    selected.reverse();
    info!("{:?}", selected);
    for (d, digit) in ChampernowneConstant::new().enumerate() {
        match selected.last() {
            Some(&select) if select == d+1 =>  {
                info!("Match {}: {}", d+1, digit);
                total *= digit;
                selected.pop();
            },
            None => break,
            _ => debug!("{}: {}", d+1, digit),
        }
    }
    println!("{}", total)
}

#[cfg(test)]
mod test {
    use super::*;

    fn test_constant() {
        let mut constant = ChampernowneConstant::new();
        assert_eq!(constant.next(), Some(1));
        assert_eq!(constant.next(), Some(2));
        assert_eq!(constant.next(), Some(3));
        assert_eq!(constant.next(), Some(4));
        assert_eq!(constant.next(), Some(5));
        assert_eq!(constant.next(), Some(6));
        assert_eq!(constant.next(), Some(7));
        assert_eq!(constant.next(), Some(8));
        assert_eq!(constant.next(), Some(9));
        assert_eq!(constant.next(), Some(1));
        assert_eq!(constant.next(), Some(0));
        assert_eq!(constant.next(), Some(1));
        assert_eq!(constant.next(), Some(1));
    }
}
