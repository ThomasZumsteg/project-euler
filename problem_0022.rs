#[macro_use]
extern crate clap;

use common::set_log_level;
use log::debug;
use std::fs::File;
use std::io::{Read, BufReader};

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 21, https://projecteuler.net/problem=21")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg file: "File with names") 
    ).get_matches();
    set_log_level(&args);

    let file_name = args.value_of("file").unwrap_or("problem_0022_names.txt");
    let file = File::open(file_name).unwrap();
    let mut buffer = String::new();
    BufReader::new(file).read_to_string(&mut buffer).unwrap();
    let mut names: Vec<String> = buffer.split(",").map(|m| m.to_string()).collect();
    names.sort();
    let mut total: usize = 0;
    for (n, name) in names.iter().enumerate() {
        let name_value: &usize = &name[1..(name.len()-1)].chars().map(|c| 1 + c as usize - 'A' as usize).sum();
        debug!("{}: {} = {}", n, &name[1..(name.len()-1)], name_value);
        total += (n+1) * name_value;
    }
    println!("{}", total);
}
