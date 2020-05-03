#[macro_use]
extern crate clap;

use common::set_log_level;
use log::debug;
use std::collections::{HashSet, VecDeque};

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
    coins.reverse();
    println!("{}", make_change(total, &coins).len());
}
