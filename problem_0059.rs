#[macro_use]
extern crate clap;

use common::set_log_level;
use std::fs;
use itertools::Itertools;
use log::{info, debug};

trait Decode {
    fn decode(&self, key: &Vec<u8>) -> Vec<u8>;
    fn score(&self) -> f64;
}

impl Decode for Vec<u8> {
    fn decode(&self, key: &Vec<u8>) -> Vec<u8> {
        self.iter()
            .zip(key.iter().cycle())
            .map(|(ct, k)| ct ^ k)
            .collect()
    }

    fn score(&self) -> f64 {
        let total = self.len() as f64;
        let letters = self.iter().filter(|&&c| 'a' as u8 <= c && c <= 'z' as u8).count() as f64;
        letters / total
    }
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 59, https://projecteuler.net/problem=59")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
        (@arg file_name: +takes_value "File to decode")
        (@arg keysize: +takes_value "Size of key")
    ).get_matches();
    set_log_level(&args);

    let keysize = args.value_of("keysize").map(|n| n.parse::<usize>().unwrap()).unwrap_or(3);
    let file_name = args.value_of("file_name").unwrap_or("problem_0059.txt");
    let text = fs::read_to_string(file_name)
        .unwrap()
        .trim()
        .split(',')
        .map(|n| n.parse::<u8>().unwrap())
        .collect::<Vec<u8>>();
    info!("{:?}", text);

    let mut result: Option<(f64, Vec<u8>, Vec<u8>)> = None;
    for key in (('a' as u8)..('z' as u8 + 1)).permutations(keysize) {
        let plain_text = text.decode(&key);
        let score = plain_text.score();
        debug!("{}: {}", key.iter().map(|&c| c as char).collect::<String>(), score);
        if result.is_none() || result.clone().unwrap().0 < score {
            result = Some((score, key.clone(), plain_text.clone()));
        }
    }
    info!("{:0.4}: {}",
        result.clone().unwrap().0,
        result.clone().unwrap().1.iter().map(|&c| c as char).collect::<String>()
    );
    info!("{}", result.clone().unwrap().2.iter().map(|&c| c as char).collect::<String>());
    println!("{}", result.clone().unwrap().2.iter().map(|&n| n as usize).sum::<usize>());
}
