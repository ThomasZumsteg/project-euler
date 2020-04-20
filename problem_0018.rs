#[macro_use]
extern crate clap;

use common::set_log_level;
use log::{info, debug};
use std::cmp::max;
use std::fs::File;
use std::io::{BufRead, BufReader};

fn find_max(triangle: &Vec<Vec<usize>>) -> usize {
    let mut step = triangle.last().unwrap().clone();
    for row in triangle.iter().rev().skip(1) {
        assert!(row.len() + 1 == step.len());
        debug!("{:?}", row);
        step = row.iter().enumerate().map(|(e, element)| element + max(step[e], step[e+1])).collect();
        debug!("{:?}", step);
    }
    assert_eq!(step.len(), 1);
    step[0]
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 18, https://projecteuler.net/problem=18")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg file: "file") 
    ).get_matches();
    set_log_level(&args);

    let file_name = args.value_of("file").unwrap_or("problem_0018.txt");
    let file = File::open(file_name).unwrap();

    let mut triangle: Vec<Vec<usize>> = Vec::new();
    for line in BufReader::new(file).lines() {
        triangle.push(line.unwrap().split(" ").map(|n| n.parse::<usize>().unwrap()).collect());
    }
    info!("{:?}", triangle);
    println!("{}", find_max(&triangle));
}
