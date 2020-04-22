
#[macro_use]
extern crate clap;

use common::set_log_level;

fn proper_divisors(num: usize) -> Vec<usize> {
    let mut result = vec![1];
    let mut n = 2;
    while n * n <= num {
        if num % n == 0 {
            result.push(n);
            result.push(num / n);
        }
        n += 1;
    };
    return result;
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 21, https://projecteuler.net/problem=21")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg limit: "Power") 
    ).get_matches();
    set_log_level(&args);

    let limit = args.value_of("limit").map(|n| n.parse::<usize>().unwrap()).unwrap_or(10000);
    let mut total = 0;
    for num in 1..limit {
        let pair = proper_divisors(num).iter().sum();
        let pair_sum: usize = proper_divisors(pair).iter().sum();
        if  pair_sum == num && pair != num {
            total += num;
        }
    }
    println!("{}", total);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple() {
        let mut result = proper_divisors(220);
        result.sort();
        assert_eq!(result, vec![1,2,4,5,10,11,20,22,44,55,110])
    }

    #[test]
    fn test_pair() {
        let mut result = proper_divisors(284);
        result.sort();
        assert_eq!(result, vec![1,2,4,71,142])
    }
}
