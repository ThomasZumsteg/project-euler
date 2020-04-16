#[macro_use]
extern crate clap;
extern crate log;
extern crate env_logger;

use env_logger::Builder;
use log::{debug, info, LevelFilter};
use std::fs::File;
use std::io::{Write, BufRead, BufReader};

struct Grid(Vec<Vec<usize>>);

#[derive(PartialEq)]
enum Direction {
    Row,
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

impl <'a> Iterator for Adjacent<'a> {
    type Item = Vec<usize>;
     
    fn next(&mut self) -> Option<Vec<usize>> {
        if self.row + self.size > self.grid.0.len() {
            return None
        }
        let (diff_row, diff_col) = match self.direction {
            Direction::Row => (0, 1),
            Direction::Column => (1, 0),
            Direction::Down => (1, 1),
            Direction::Up => (-1, 1),
        };
        let mut result = Vec::with_capacity(self.size);
        for s in 0isize..self.size as isize {
            let row = self.row as isize + if self.direction != Direction::Up { s * diff_row } else { self.size as isize - s };
            let col = s * diff_col + self.col as isize;
            result.push(self.grid.0[row as usize][col as usize]);
        };
        self.col += 1;
        if self.col < self.grid.0.len() {
            self.direction = Direction::Column;
        } else {
            self.row += 1;
            self.col = 0;
        }
        Some(result)
    }
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 11, https://projecteuler.net/problem=11")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg file: "Product grid") 
    ).get_matches();

    let log_level = match args.occurrences_of("verbose") {
        0 => LevelFilter::Off,
        1 => LevelFilter::Error,
        2 => LevelFilter::Warn,
        3 => LevelFilter::Info,
        4 => LevelFilter::Debug,
        _ => LevelFilter::Trace,
    };
    Builder::new()
        .filter_level(log_level)
        .format(|buf, record| writeln!(buf, "[{}] {}", record.level(), record.args()))
        .init();
    info!("Set log level {}", log_level);

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
