#[macro_use]
extern crate clap;

use common::set_log_level;
use log::debug;
use std::collections::VecDeque;

fn grid_paths(rows: usize, cols: usize) -> usize {
    let mut grid = vec![vec![0; cols+1]; rows+1];
    debug!("Before {:?}", grid);
    grid[0][0] = 1;
    for row in 0..(rows+1) {
        for col in 0..(cols+1) {
            if 0 < row {
                grid[row][col] += grid[row-1][col];
            }
            if 0 < col {
                grid[row][col] += grid[row][col-1];
            }
        }
    }
    debug!("After {:?}", grid);
    grid[rows][cols]
}

#[allow(dead_code)]
fn grid_paths_brute(rows: usize, cols: usize) -> usize {
    let mut queue = VecDeque::new();
    queue.push_back((0, 0));
    let mut total = 0;
    while let Some((row, col)) = queue.pop_front() {
        if row == rows && col == cols {
            total += 1;
        } else if row <= rows && col <= cols {
            queue.push_back((row + 1, col));
            queue.push_back((row, col + 1));
        }
    }
    total
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 15, https://projecteuler.net/problem=15")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg size: "Grid size") 
    ).get_matches();
    set_log_level(&args);

    let grid_size = args.value_of("size").map(|n| n.parse::<usize>().unwrap()).unwrap_or(20);
    println!("{}", grid_paths(grid_size, grid_size));
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_example() {
        assert_eq!(grid_paths_brute(2, 2), 6);
    }

    #[test]
    fn test_small() {
        assert_eq!(grid_paths_brute(1, 1), 2);
    }

    #[test]
    fn test_reflection() {
        assert_eq!(grid_paths_brute(1, 2), grid_paths_brute(2, 1));
    }

    #[test]
    fn test_against_brute() {
        for n in 1..5 {
            for m in 1..5 {
                assert_eq!(grid_paths_brute(n, m), grid_paths(n, m), "Testing {}x{}", n, m);
            }
        }
    }
}
