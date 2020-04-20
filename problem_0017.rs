#[macro_use]
extern crate clap;

use common::set_log_level;
use log::debug;

fn digit_names(digits: usize) -> Option<String> {
    match digits {
        0 => None,
        1 => Some("one".to_string()),
        2 => Some("two".to_string()),
        3 => Some("three".to_string()),
        4 => Some("four".to_string()),
        5 => Some("five".to_string()),
        6 => Some("six".to_string()),
        7 => Some("seven".to_string()),
        8 => Some("eight".to_string()),
        9 => Some("nine".to_string()),
        10 => Some("ten".to_string()),
        11 => Some("eleven".to_string()),
        12 => Some("twelve".to_string()),
        13 => Some("thirteen".to_string()),
        14 => Some("fourteen".to_string()),
        15 => Some("fifteen".to_string()),
        16 => Some("sixteen".to_string()),
        17 => Some("seventeen".to_string()),
        18 => Some("eighteen".to_string()),
        19 => Some("nineteen".to_string()),
        20 => Some("twenty".to_string()),
        30 => Some("thirty".to_string()),
        40 => Some("forty".to_string()),
        50 => Some("fifty".to_string()),
        60 => Some("sixty".to_string()),
        70 => Some("seventy".to_string()),
        80 => Some("eighty".to_string()),
        90 => Some("ninety".to_string()),
        _ => None,
    }
}

fn to_english(number: usize) -> String {
    if number == 1000 { return "one thousand".to_string() }
    else if number > 1000 { unimplemented!() }
    let mut result: String = String::new();
    if let Some(part) = digit_names(number / 100) {
        result += &format!("{} hundred", part);
        if number % 100 > 0 {
            result += " and ";
        }
    }
    let teens = digit_names(number % 100);
    let tens = digit_names(((number % 100) / 10) * 10);
    let ones = digit_names(number % 10);
    if let Some(teen) = teens {
        result += &teen;
    } else if tens.is_some() && ones.is_none() {
        result += &tens.unwrap();
    } else if tens.is_none() && ones.is_some() {
        result += &ones.unwrap();
    } else if tens.is_some() && ones.is_some() {
        result += &format!("{}-{}", &tens.unwrap(), &ones.unwrap());
    }
    result
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 17, https://projecteuler.net/problem=17")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg limit: "Limit") 
    ).get_matches();
    set_log_level(&args);

    let limit = args.value_of("limit").map(|n| n.parse::<usize>().unwrap()).unwrap_or(1000);
    let char_count = (1..(limit+1)).fold(0, |acc, n| {
        let phrase = to_english(n);
        let mut count = 0;
        for chr in phrase.chars() {
            if chr.is_ascii_alphabetic() {
                count += 1;
            }
        }
        debug!("({}){} - {}", n, phrase, count);
        acc + count
    });
    println!("{}", char_count);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_composition() {
        assert_eq!(to_english(115), "one hundred and fifteen");
        assert_eq!(to_english(110), "one hundred and ten");
        assert_eq!(to_english(342), "three hundred and forty-two");
    }

    #[test]
    fn test_basics() {
        assert_eq!(to_english(1000), "one thousand");
        assert_eq!(to_english(200), "two hundred");
        assert_eq!(to_english(100), "one hundred");
        assert_eq!(to_english(19), "nineteen");
        assert_eq!(to_english(18), "eighteen");
        assert_eq!(to_english(17), "seventeen");
        assert_eq!(to_english(16), "sixteen");
        assert_eq!(to_english(15), "fifteen");
        assert_eq!(to_english(14), "fourteen");
        assert_eq!(to_english(13), "thirteen");
        assert_eq!(to_english(12), "twelve");
        assert_eq!(to_english(11), "eleven");
        assert_eq!(to_english(10), "ten");
        assert_eq!(to_english(9), "nine");
        assert_eq!(to_english(8), "eight");
        assert_eq!(to_english(7), "seven");
        assert_eq!(to_english(6), "six");
        assert_eq!(to_english(5), "five");
        assert_eq!(to_english(4), "four");
        assert_eq!(to_english(3), "three");
        assert_eq!(to_english(2), "two");
        assert_eq!(to_english(1), "one");
    }
}
