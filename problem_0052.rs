#[macro_use]
extern crate clap;

use common::set_log_level;
use common::digits::Digits;
use log::info;

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 52, https://projecteuler.net/problem=52")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
        (@arg multiples: +takes_value "Mutliples")
        (@arg limit: "limit to search")
    ).get_matches();
    set_log_level(&args);

    let limit = args.value_of("limit").map(|n| n.parse::<usize>().unwrap());
    let multiples = args.value_of("multiples").map(|m| m.parse::<usize>().unwrap()).unwrap_or(6);
    
    let mut result = None;
    let mut num = 1;
    while result.is_none() {
        if limit.is_some() && limit.unwrap() < num {
            break;
        }
        let mut num_digits = Digits::from(num).digits;
        if num_digits.len() < Digits::from(num * multiples).digits.len() {
            num = 10usize.pow(num_digits.len() as u32);
            continue;
        }
        info!("{}", num);
        num_digits.sort();
        if (2..multiples).all(|m| {
            let mut multiple_digits = Digits::from(num * m).digits;
            multiple_digits.sort();
            multiple_digits == num_digits
        }) {
            result = Some(num);
        }
        num += 1;
    }
    println!("{}", result.unwrap());
}
