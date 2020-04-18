#[macro_use]
extern crate clap;

use common::set_log_level;
use log::debug;
use std::fs::File;
use std::io::{BufRead, BufReader};

struct Grid(Vec<Vec<usize>>);

#[derive(PartialEq)]
enum Direction {
    Row = 0,
    Column,
    Down,
    Up
}

struct Adjacent<'a> {
    row: usize,
    col: usize,
    grid: &'a Grid,
    direction: Direction,
    size: usize,
}

impl Grid {
    fn new(digits: Vec<Vec<usize>>) -> Grid {
        Grid(digits)
    }

    fn adjacent(&self, size: usize) -> Adjacent {
        Adjacent { row: 0, col: 0, grid: self, size: size, direction: Direction::Row }
    }
}

impl <'a> Adjacent<'a> {
    fn step(&mut self) {
        self.direction = match self.direction {
            Direction::Row => Direction::Column,
            Direction::Column => Direction::Down,
            Direction::Down => Direction::Up,
            Direction::Up => {
                self.col += 1;
                if self.col >= self.grid.0.len() {
                    self.row += 1;
                    self.col = 0;
                }
                Direction::Row
            }
        };
    }
}

impl <'a> Iterator for Adjacent<'a> {
    type Item = Vec<usize>;
     
    fn next(&mut self) -> Option<Vec<usize>> {
        'search: loop {
            if self.row > self.grid.0.len() {
                return None;
            }
            let mut result = Vec::with_capacity(self.size);
            for s in 0..self.size {
                let (dr, dc) =  match self.direction {
                    Direction::Row => (0, s),
                    Direction::Column => (s, 0),
                    Direction::Down => (s, s),
                    Direction::Up => (self.size - s, s),
                };
                if !(dr <= self.row && self.row + dr < self.grid.0.len() && 
                    dc <= self.col && self.col + dc < self.grid.0[self.row + dr].len()) {
                    self.step();
                    continue 'search;
                }
                result.push(self.grid.0[self.row + dr][self.col + dc]);
            };
            self.step();
            return Some(result);
        }
    }
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 11, https://projecteuler.net/problem=11")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg file: "Product grid") 
    ).get_matches();
    set_log_level(&args);

    let file_name = args.value_of("file").unwrap_or("problem_0011.txt");
    let file = File::open(file_name).unwrap();
    let mut digits: Vec<Vec<usize>> = Vec::new();
    for line in BufReader::new(file).lines() {
        let digit_line = line.unwrap().split(' ').map(|n| n.parse::<usize>().unwrap()).collect::<Vec<usize>>();
        debug!("Line: {:?}", digit_line);
        digits.push(digit_line);
    }

    println!("{:?}", Grid::new(digits).adjacent(4).map(|v| {debug!("{:?}", v); v.iter().fold(1, |acc, n| acc * n)}).max().unwrap());
}
