#[macro_use]
extern crate clap;

use common::set_log_level;
use log::{debug, info};

struct Digits(Vec<usize>);

impl Digits {
    fn new(digits: Vec<usize>) -> Digits {
        Digits(digits)
    }
}

impl Iterator for Digits {
    type Item = Vec<usize>;

    fn next(&mut self) -> Option<Vec<usize>> {
        // https://www.quora.com/How-would-you-explain-an-algorithm-that-generates-permutations-using-lexicographic-ordering
        let mut x = self.0.len() - 2;
        while self.0[x] >= self.0[x+1] {
            if x == 0 { return None; }
            x -= 1;
        }
        let mut y = self.0.len() - 1;
        while self.0[x] >= self.0[y] {
            y -= 1;
        }
        self.0.swap(x, y);
        x += 1;
        let mut n = self.0.len() - 1;
        while x < n {
            self.0.swap(x, n);
            n -= 1;
            x += 1
        }
        Some(self.0.clone())
    }
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 24, https://projecteuler.net/problem=24")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg digits: --digits -d +takes_value "Digits to use") 
        (@arg nth: "nth digit order")
    ).get_matches();
    set_log_level(&args);

    let nth = args.value_of("nth").map(|n| n.parse().unwrap()).unwrap_or(1000000);
    info!("nth: {}", nth);
    let starting_digits: Vec<usize> = args
        .value_of("digits")
        .map(|n|
            n.chars()
            .map(|d| d.to_digit(10).unwrap() as usize)
            .collect()
        ).unwrap_or(vec![0,1,2,3,4,5,6,7,8,9]);
    info!("digits: {:?}", starting_digits);
    for (n, digits) in Digits::new(starting_digits).enumerate() {
        debug!("{}: {:?}", n + 1, digits);
        if n + 2 == nth {
            println!("{}", digits.iter().fold(String::new(), |acc, d| acc + &d.to_string()));
            break
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_simple() {
        let mut order = Digits::new(vec![0,1,2]);
        assert_eq!(order.next(), Some(vec![0,2,1]));
        assert_eq!(order.next(), Some(vec![1,0,2]));
        assert_eq!(order.next(), Some(vec![1,2,0]));
        assert_eq!(order.next(), Some(vec![2,0,1]));
        assert_eq!(order.next(), Some(vec![2,1,0]));
        assert_eq!(order.next(), None);
    }
}
