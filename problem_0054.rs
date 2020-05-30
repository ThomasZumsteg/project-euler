#[macro_use]
extern crate clap;

use common::set_log_level;
use log::info;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::cmp::Ordering;

#[derive(Clone, Debug)]
enum Suit {
    HEART,
    SPADE,
    DIMOND,
    CLUB,
}

impl From<&str> for Suit {
    fn from(value: &str) -> Self {
        match value {
            "C" => Suit::CLUB,
            "D" => Suit::DIMOND,
            "S" => Suit::SPADE,
            "H" => Suit::HEART,
            _ => panic!("Unknow value: {}", value),
        }
    }
}

#[derive(Clone, Debug)]
enum Value {
    ACE,
    KING,
    QUEEN,
    JACK,
    TEN,
    NINE,
    EIGHT,
    SEVEN,
    SIX,
    FIVE,
    FOUR,
    THREE,
    TWO
}

impl From<&str> for Value {
    fn from(repr: &str) -> Self {
        match repr {
            "A" => Value::ACE,
            "K" => Value::KING,
            "Q" => Value::QUEEN,
            "J" => Value::JACK,
            "T" => Value::TEN,
            "9" => Value::NINE,
            "8" => Value::EIGHT,
            "7" => Value::SEVEN,
            "6" => Value::SIX,
            "5" => Value::FIVE,
            "4" => Value::FOUR,
            "3" => Value::THREE,
            "2" => Value::TWO,
            _ => unimplemented!(),
        }
    }
}

#[derive(Clone, Debug)]
struct Card {
    suit: Suit,
    value: Value,
}

impl Card {
    fn from(card: &str) -> Card {
        assert!(card.len() == 2);
        Card {
            value: Value::from(&card[0..1]),
            suit: Suit::from(&card[1..2]),
        }
    }
}

struct Hand {
    cards: Vec<Card>,
}

enum Hands {
    HIGHCARD(Value),
    ONEPAIR(Value),
    TWOPAIR(Value, Value),
    THREEOFAKIND(Value),
    STRAIGHT(Value),
    FLUSH(Value),
    FULLHOUSE(Value, Value),
    FOUROFAKIND(Value),
    STRAIGHTFLUSH(Value),
    ROYALFLUSH,
}

impl Hand {
    fn from(cards: &[Card]) -> Hand {
        Hand { cards: cards.to_vec() }
    }
}

impl PartialEq for Hand {
    fn eq(&self, other: &Self) -> bool {
        unimplemented!();
    }
}

impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        unimplemented!()
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
        info!("'{}'", line);
        let cards = line.split(" ").map(|c| Card::from(c)).collect::<Vec<Card>>();
        assert!(cards.len() == 10);
        let player1 = Hand::from(&cards[0..5]);
        let player2 = Hand::from(&cards[5..10]);
        if player1 > player2 {
            total += 1;
        }
    }
    println!("{}", total);
}
