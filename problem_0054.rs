#[macro_use]
extern crate clap;

use common::set_log_level;
use log::{info, debug};
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::cmp::Ordering;

#[derive(Debug)]
struct Hand {
    rank: u8,
    values: Vec<u8>
}
 
impl Hand {
    fn from(cards: &[&str]) -> Hand {
        let mut value_map = HashMap::new();
        let mut suits: HashSet<&str> = HashSet::new();
        for card in cards {
            let value = match &card[0..1] {
                "2" => 2,
                "3" => 3,
                "4" => 4,
                "5" => 5,
                "6" => 6,
                "7" => 7,
                "8" => 8,
                "9" => 9,
                "T" => 10,
                "J" => 11,
                "Q" => 12,
                "K" => 13,
                "A" => 14,
                _ => panic!("Not a card {}", card)
            };
            *value_map.entry(value).or_insert(0) += 1;
            suits.insert(&card[1..2]);
        };
        let mut flat_map = value_map.iter().collect::<Vec<(&u8, &u8)>>();
        flat_map.sort_by(|a, b| {
            if a.1 == b.1 { a.0.cmp(&b.0)
            } else { a.1.cmp(&b.1) }
        });
        let mut values = Vec::new();
        let mut value_count = Vec::new();
        for (&value, &count) in flat_map.iter().rev() {
            value_count.push(count);
            for _ in 0..count {
                values.push(value);
            }
        }
        if values == vec![2,3,4,5,14] {
            values.pop();
            values.insert(0, 1);
        }
        let is_straight = value_count.len() == 5 && (0..4).all(|i| values[i] - values[i+1] == 1);
        if is_straight && suits.len() == 1 && values[0] == 14 {
            debug!("{:?} Royal Flush {:?}", cards, values);
            Hand { rank: 10, values: values }
        } else if is_straight && suits.len() == 1 {
            debug!("{:?} Straight Flush {:?}", cards, values);
            Hand { rank: 9, values: values }
        } else if value_count == vec![4, 1] {
            debug!("{:?} Four of a Kind {:?}", cards, values);
            Hand { rank: 8, values: values }
        } else if value_count == vec![3, 2] {
            debug!("{:?} Full House {:?}", cards, values);
            Hand { rank: 7, values: values }
        } else if suits.len() == 1 {
            debug!("{:?} Flush {:?}", cards, values);
            Hand { rank: 6, values: values }
        } else if is_straight {
            debug!("{:?} Straight {:?}", cards, values);
            Hand { rank: 5, values: values }
        } else if value_count == vec![3, 1, 1] {
            debug!("{:?} Three of a Kind {:?}", cards, values);
            Hand { rank: 4, values: values }
        } else if value_count == vec![2, 2, 1] {
            debug!("{:?} Two Pairs {:?}", cards, values);
            Hand { rank: 3, values: values }
        } else if value_count == vec![2, 1, 1, 1] {
            debug!("{:?} One Pairs {:?}", cards, values);
            Hand { rank: 2, values: values }
        } else {
            debug!("{:?} High Card {:?}", cards, values);
            Hand { rank: 1, values: values }
        }
    }
}

impl PartialEq for Hand {
    fn eq(&self, other: &Self) -> bool {
        self.rank == other.rank && self.values.iter().zip(&other.values).all(|(v1, v2)| v1 == v2)
    }
}

impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.rank != other.rank {
            return Some(self.rank.cmp(&other.rank));
        }
        for (v1, v2) in self.values.iter().zip(other.values.iter()) {
            if v1 != v2 {
                return Some(v1.cmp(v2));
            }
        }
        Some(Ordering::Equal)
    }
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 54, https://projecteuler.net/problem=54")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
        (@arg file: +takes_value "File name with poker hands")
    ).get_matches();
    set_log_level(&args);

    let file_name = args.value_of("file").unwrap_or("problem_0054.txt");
    let file = File::open(file_name).unwrap();

    let mut total = 0;
    for line in BufReader::new(file).lines() {
        let line = line.unwrap();
        let cards = line.split(" ").collect::<Vec<&str>>();
        assert!(cards.len() == 10);
        let player1 = Hand::from(&cards[0..5]);
        let player2 = Hand::from(&cards[5..10]);
        if player1 > player2 {
            debug!("{:?}, {:?}", player1, player2);
            info!("{:?} >  {:?}", &cards[0..5], &cards[5..10]);
            total += 1;
        } else {
            info!("{:?} <= {:?}", &cards[0..5], &cards[5..10]);
        }
    }
    println!("{}", total);
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_hand_rank() {
        assert!(Hand::from(&vec!["TC","JC","QC","KC","AC"]) == Hand::from(&vec!["TH","JH","QH","KH","AH"]));
        assert!(Hand::from(&vec!["AC","QC","JC","KC","TC"]) == Hand::from(&vec!["TH","JH","QH","KH","AH"]));
        assert!(Hand::from(&vec!["9C","TC","JC","QC","KC"]) < Hand::from(&vec!["TH","JH","QH","KH","AH"]));
        assert!(Hand::from(&vec!["9C","TC","JC","QC","KC"]) > Hand::from(&vec!["8H","9H","TH","JH","QH"]));
        assert!(Hand::from(&vec!["9C","TC","JC","QC","KC"]) > Hand::from(&vec!["8H","8S","8D","8C","TH"]));
        assert!(Hand::from(&vec!["4C","4H","4S","4D","KC"]) < Hand::from(&vec!["8H","8S","8D","8C","TH"]));
        assert!(Hand::from(&vec!["3C","3H","3S","TD","TC"]) < Hand::from(&vec!["8H","8S","8D","4C","4H"]));
        assert!(Hand::from(&vec!["3C","7C","2C","TC","AC"]) > Hand::from(&vec!["6H","8H","2H","JH","4H"]));

        assert!(Hand::from(&vec!["5H","5C","6S","7S","KD"]) < Hand::from(&vec!["2C","3S","8S","8D","TD"]));
        assert!(Hand::from(&vec!["5D","8C","9S","JS","AC"]) > Hand::from(&vec!["2C","5C","7D","8S","QH"]));
        assert!(Hand::from(&vec!["2D","9C","AS","AH","AC"]) < Hand::from(&vec!["3D","6D","7D","TD","QD"]));
        assert!(Hand::from(&vec!["4D","6S","9H","QH","QC"]) > Hand::from(&vec!["3D","6D","7H","QD","QS"]));
        assert!(Hand::from(&vec!["2H","2D","4C","4D","4S"]) > Hand::from(&vec!["3C","3D","3S","9S","9D"]));

        assert!(Hand::from(&vec!["2H","2D","4C","4D","4S"]) > Hand::from(&vec!["3C","3D","3S","9S","9D"]));
        assert!(Hand::from(&vec!["2C","9D","KC","QH","TH"]) > Hand::from(&vec!["QS","JC","9C","4H","TS"]));
        assert!(Hand::from(&vec!["JC","KC","6H","TS","QS"]) < Hand::from(&vec!["TD","KS","8H","8C","9S"]));
        assert!(Hand::from(&vec!["9S","6S","TC","QS","JC"]) < Hand::from(&vec!["5C","5D","9C","TH","8C"]));
        assert!(Hand::from(&vec!["TD","JD","QC","4D","9S"]) < Hand::from(&vec!["7S","TS","AD","7D","AC"]));
        assert!(Hand::from(&vec!["9H","TD","3S","8H","7S"]) < Hand::from(&vec!["AC","5C","6C","AH","7C"]));
        assert!(Hand::from(&vec!["AH","QH","3C","JD","KC"]) < Hand::from(&vec!["4S","5S","5D","TD","KS"]));
        assert!(Hand::from(&vec!["AS","JS","2S","QD","KH"]) < Hand::from(&vec!["8H","4S","AC","8D","8S"]));
        assert!(Hand::from(&vec!["TC","9H","8H","JC","3C"]) < Hand::from(&vec!["9S","8D","KS","AD","KC"]));
    }

}
