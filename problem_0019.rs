#[macro_use]
extern crate clap;
extern crate chrono;

use common::set_log_level;
use chrono::{Datelike, Duration, NaiveDate, Weekday};
use log::debug;

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 19, https://projecteuler.net/problem=19")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg start: "Start date") 
        (@arg end: "End date") 
    ).get_matches();
    set_log_level(&args);

    let mut total = 0;
    let mut start = args.value_of("start")
        .map(|date| NaiveDate::parse_from_str(date, "%Y-%b-%d").unwrap())
        .unwrap_or(NaiveDate::from_ymd(1901, 1, 1));
    let end = args.value_of("end")
        .map(|date| NaiveDate::parse_from_str(date, "%Y-%b-%d").unwrap())
        .unwrap_or(NaiveDate::from_ymd(2000, 12, 31));
    while start.weekday() != Weekday::Sun {
        start += Duration::days(1);
    }
    while start <= end {
        if start.day() == 1 {
            debug!("{}", start);
            total += 1;
        }
        start += Duration::days(7);
    }
    println!("{}", total);
}
