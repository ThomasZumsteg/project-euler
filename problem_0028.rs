#[macro_use]
extern crate clap;

use common::set_log_level;
use log::{info, debug};

struct Spiral {
    level: usize,
    corner: usize,
    counter: usize,
}

impl Spiral {
    fn new() -> Spiral {
        Spiral { level: 0, corner: 3, counter: 0 }
    }
}

impl Iterator for Spiral {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        self.counter += if self.level > 0 {
            2 * self.level
        } else {
            1
        };
        self.corner += 1;
        if self.corner > 3 {
            self.level += 1;
            self.corner = 0;
        }
        Some(self.counter)
    }
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 28, https://projecteuler.net/problem=28")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg spiral_size: "Spiral size")
    ).get_matches();
    set_log_level(&args);

    let spiral_size = args.value_of("spiral_size").map(|n| n.parse::<usize>().unwrap()).unwrap_or(1001);
    info!("Spiral Size: {}", spiral_size);
    let mut sum = 0;
    let mut spiral = Spiral::new();
    while spiral.level * 2 < spiral_size + 1 {
        let step = spiral.next().unwrap();
        sum += step;
        debug!("sum: {}, step: {}", sum, step);
    }
    println!("{}", sum);
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_spiral() {
        let mut spiral = Spiral::new();
        assert_eq!(spiral.next(), Some(1));
        assert_eq!(spiral.next(), Some(3));
        assert_eq!(spiral.next(), Some(5));
        assert_eq!(spiral.next(), Some(7));
        assert_eq!(spiral.next(), Some(9));
        assert_eq!(spiral.next(), Some(13));
        assert_eq!(spiral.next(), Some(17));
        assert_eq!(spiral.next(), Some(21));
        assert_eq!(spiral.next(), Some(25));
    }
}
