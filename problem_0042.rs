#[macro_use]
extern crate clap;

use common::{set_log_level, integer_square_root};
use log::{debug, info};
use std::fs::File;
use std::io::{Read, BufReader};

fn is_triangle(number: usize) -> bool {
    if let Some(root) = integer_square_root(8 * number + 1) {
        root % 2 == 1
    } else {
        false
    }
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 42, https://projecteuler.net/problem=42")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
        (@arg file: "File with words") 
    ).get_matches();
    set_log_level(&args);
    let file_name = args.value_of("file").unwrap_or("problem_0042.txt");
    let file = File::open(file_name).unwrap();
    let mut buffer = String::new();
    BufReader::new(file).read_to_string(&mut buffer).unwrap();

    let words = buffer.split(",").map(|chars| chars[1..chars.len()-1].to_string());
    let mut total = 0;
    for word in words {
        let word_score = word.chars().map(|c| c as usize - 'A' as usize + 1).sum::<usize>();
        debug!("{}: {}", word, word_score);
        if is_triangle(word_score) {
            info!("{} is a triangle word", word);
            total += 1;
        }
    }
    println!("{}", total);
}

#[cfg(test)]
mod test {
    use super::is_triangle;

    #[test]
    fn test_is_triangle() {
        assert!(is_triangle(1));
        assert!(!is_triangle(2));
        assert!(is_triangle(3));
        assert!(!is_triangle(4));
        assert!(is_triangle(6));
        assert!(is_triangle(10));
    }
}
