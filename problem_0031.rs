#[macro_use]
extern crate clap;

use common::set_log_level;
use log::debug;
use std::collections::{HashSet, VecDeque};

struct Change {
    coins: Vec<usize>,
    change: Vec<usize>,
    done: bool,
}

impl Change {
    fn new(total: usize, coins: &Vec<usize>) -> Change {
        Change {
            change: Change::make_change(&coins, total).unwrap(),
            coins: coins.clone(),
            done: false,
        }
    }

    fn make_change(coins: &[usize], total: usize) -> Option<Vec<usize>> {
        let mut remaining = total;
        let mut result = vec![0; coins.len()];
        for (coin, count) in coins.iter().zip(result.iter_mut()).rev() {
            while remaining >= *coin {
                *count += 1;
                remaining -= *coin;
            }
            if remaining == 0 {
                return Some(result);
            }
        }
        None
    }
}

impl Iterator for Change {
    type Item = Vec<usize>;

    fn next(&mut self) -> Option<Vec<usize>> {
        if self.done {
            return None;
        }
        let result: Vec<usize> = self.coins.iter()
            .zip(self.change.iter())
            .map(|(&coin, &count)| vec![coin; count])
            .flatten()
            .collect();
        for (c, count) in self.change.iter_mut().enumerate().rev() {
            if c == 0 {
                self.done = true;
            } else if *count > 0 {
                *count -= 1;
                let small_change = Change::make_change(&self.coins[0..c], self.coins[c]);
                for (curr, diff) in self.change.iter_mut().zip(small_change.unwrap().iter()) {
                    *curr += diff;
                }
                break;
            }
        }
        Some(result)
    }
}

fn make_change(total: usize, coins: &Vec<usize>) -> HashSet<Vec<usize>> {
    let mut queue: VecDeque<(usize, Vec<usize>, Vec<usize>)> = VecDeque::new();
    queue.push_back((total, coins.clone(), Vec::new()));
    let mut seen = HashSet::new();
    while let Some((total, coins, change)) = queue.pop_back() {
        if total == 0 {
            debug!("{:?}", change);
            seen.insert(change.clone());
        }
        if let Some(coin) = coins.last() {
            if coin <= &total {
                let mut new_change = change.clone();
                new_change.push(*coin);
                let mut new_coins = coins.clone();
                new_coins.pop();
                queue.push_back((total, new_coins.clone(), change.clone()));
                queue.push_back((total - coin, coins.clone(), new_change));
            }
        }
    }
    seen
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 31, https://projecteuler.net/problem=31")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg total: "Total to make change for")
        (@arg threads: -t --threads +takes_value "Threads")
        (@arg coins: -c --coins +takes_value "Coins, comma seperated")
    ).get_matches();
    set_log_level(&args);

    let total = args.value_of("total").map(|n| n.parse::<usize>().unwrap()).unwrap_or(200);
    let threads = args.value_of("threads").map(|n| n.parse::<usize>().unwrap()).unwrap_or(1);
    let mut coins: Vec<usize> = args
        .value_of("coins")
        .map(|coins|
            coins.split(",").map(|c| c.parse::<usize>().unwrap()).collect()
        )
        .unwrap_or(vec![1,2,5,10,20,50,100,200]);
    coins.sort();
    println!("{}", Change::new(total, &coins).count());
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_make_change() {
        let coins = vec![1,2,5,10,20, 50,100,200];
        assert_eq!(Change::make_change(&coins, 100), Some(vec![0,0,0,0,0,0,1,0]));
        assert_eq!(Change::make_change(&coins, 150), Some(vec![0,0,0,0,0,1,1,0]));
    }

    #[test]
    fn test_ways_to_make_change() {
        let mut change = Change::new(5, &vec![1,2,5]);
        assert_eq!(change.next(), Some(vec![5]));
        assert_eq!(change.next(), Some(vec![1,2,2]));
        assert_eq!(change.next(), Some(vec![1,1,1,2]));
        assert_eq!(change.next(), Some(vec![1,1,1,1,1]));
        assert_eq!(change.next(), None);
    }
}
