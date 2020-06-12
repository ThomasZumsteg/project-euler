#[macro_use]
extern crate clap;
extern crate maplit;

use common::set_log_level;
use common::primes::Primes;
use log::{debug, info};
use itertools::Itertools;
use std::collections::{HashSet, HashMap};

fn find_valid_sets(candidates: HashSet<usize>, map: &HashMap<usize, HashSet<usize>>, size: usize) -> Vec<HashSet<usize>> {
    let candidate_set = candidates.iter()
        .map(|c| map[&c].clone())
        .fold(None, |acc: Option<HashSet<usize>>, s| {
            if let Some(values) = acc {
                Some(values.intersection(&s).map(|&n| n).collect())
            } else {
                Some(s)
            }
        })
        .unwrap();
    let mut results = Vec::new();
    for combination in candidate_set.iter().combinations(size - candidates.len()) {
        let collection = candidates.iter()
            .map(|&n| n.clone())
            .chain(combination.iter().map(|&n| n.clone()))
            .collect::<HashSet<usize>>();
        if collection.iter().all(|c| collection.iter().all(|d| d == c || map[c].contains(d))) {
            info!("Found: {:?}", collection);
            results.push(collection);
        }
    }
    results
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 58, https://projecteuler.net/problem=58")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
        (@arg set_size: +takes_value "Number of primes to take")
    ).get_matches();
    set_log_level(&args);
    
    let set_size = args.value_of("set_size").map(|n| n.parse::<usize>().unwrap()).unwrap_or(5);

    let mut primes = Primes::new();
    let mut concat_primes: HashMap<usize, HashSet<usize>> = HashMap::new();
    let mut p = 1;
    let mut result: Option<HashSet<usize>> = None;
    loop {
        p += 1;
        let prime = primes.nth_prime(p);
        if result.is_some() && result.clone().unwrap().iter().sum::<usize>() < prime {
            break
        }
        for q in 0..p {
            let qrime = primes.nth_prime(q);
            let crime = (prime.to_string() + &qrime.to_string()).parse::<usize>().unwrap();
            let drime = (qrime.to_string() + &prime.to_string()).parse::<usize>().unwrap();
            if primes.is_prime(crime) && primes.is_prime(drime) {
                debug!("{} + {} = {}, {} + {} = {}", qrime, prime, drime, prime, qrime, crime);
                concat_primes.entry(qrime).or_insert(HashSet::new()).insert(prime);
                concat_primes.entry(prime).or_insert(HashSet::new()).insert(qrime);
                let candidates = vec![prime, qrime].iter().map(|n| n.clone()).collect::<HashSet<usize>>();
                for set in find_valid_sets(candidates, &concat_primes, set_size) {
                    if result.is_none() || result.clone().unwrap().iter().sum::<usize>() > set.iter().sum::<usize>() {
                        info!("{:?}: {}", set, set.iter().sum::<usize>());
                        result = Some(set);
                    }
                }
            }
        }
    };
    debug!("{:?}", concat_primes);
    info!("{:?}", result);
    println!("{}", result.unwrap().iter().sum::<usize>());
}

#[cfg(test)]
mod test {
    use super::*;
    use maplit::{hashset, hashmap};

    #[test]
    fn test_has_set() {
        let map = hashmap!{
            3 => hashset!{7, 109, 673},
            7 => hashset!{3, 109, 673},
            109 => hashset!{3, 7, 673},
            673 => hashset!{3, 7, 109}
        };
        assert_eq!(
            find_valid_sets(hashset!{3, 7}, &map, 4),
            vec![hashset!{3, 7, 109, 673}],
        )
    }

    #[test]
    fn test_large_map() {
        let map = hashmap!{
            3 => hashset!{67, 137, 331, 541, 37, 229, 31, 11, 449, 467, 17, 7, 617, 271, 607, 613, 673, 109, 373, 359, 499, 59, 73, 191, 557}, 
            7 => hashset!{523, 433, 19, 127, 229, 283, 541, 547, 3, 61, 673, 97, 109, 487}, 
            11 => hashset!{251, 353, 587, 503, 3, 113, 239, 23}, 
            13 => hashset!{331, 367, 61, 103, 337, 523, 577, 19, 127, 241}, 
            17 => hashset!{431, 449, 491, 383, 389, 3, 83, 239, 257}, 
            19 => hashset!{7, 163, 571, 433, 577, 181, 13, 79, 97, 31}, 
            23 => hashset!{89, 47, 311, 509, 11}, 
            29 => hashset!{347, 179, 383, 71, 137, 167, 401, 569, 599}, 
            31 => hashset!{151, 3, 181, 19, 139}, 
            37 => hashset!{67, 3, 277, 79, 607, 199, 313, 463}, 
            41 => hashset!{227, 593, 257}, 
            43 => hashset!{223, 103, 97, 271, 499, 613}, 
            47 => hashset!{251, 293, 419, 149, 23, 269, 521}, 
            53 => hashset!{113, 353, 401, 419, 653, 269, 197}, 
            59 => hashset!{3, 419, 167, 197}, 
            61 => hashset!{487, 409, 331, 151, 13, 7}, 
            67 => hashset!{139, 37, 157, 547, 601, 619, 3}, 
            71 => hashset!{257, 389, 443, 29, 233, 263, 317}, 
            73 => hashset!{607, 277, 3, 571, 643, 547}, 
            79 => hashset!{613, 367, 631, 241, 193, 397, 19, 37}, 
            83 => hashset!{443, 227, 563, 17, 311, 449}, 
            89 => hashset!{107, 431, 443, 137, 293, 521, 23}, 
            97 => hashset!{157, 19, 373, 7, 241, 379, 43}, 
            101 => hashset!{107, 641, 359, 197, 149, 383, 467}, 
            103 => hashset!{43, 421, 307, 13}, 
            107 => hashset!{449, 89, 101}, 
            109 => hashset!{661, 3, 199, 673, 7, 139, 313}, 
            113 => hashset!{131, 53, 227, 383, 149, 647, 11, 167, 233}, 
            127 => hashset!{7, 271, 13, 601, 373, 607, 331, 157, 163, 241}, 
            131 => hashset!{449, 641, 479, 113, 617}, 
            137 => hashset!{89, 359, 587, 239, 353, 29, 197, 491, 659, 191, 3}, 
            139 => hashset!{367, 457, 661, 31, 67, 547, 619, 109}, 
            149 => hashset!{251, 113, 101, 47, 173, 491, 563}, 
            151 => hashset!{31, 397, 163, 607, 631, 61, 433, 499}, 
            157 => hashset!{97, 229, 277, 571, 67, 127, 181}, 
            163 => hashset!{151, 19, 127, 307, 409, 193, 613, 367}, 
            167 => hashset!{443, 491, 29, 59, 521, 641, 113, 269}, 
            173 => hashset!{347, 431, 191, 149, 659, 293}, 
            179 => hashset!{383, 593, 317, 29, 269}, 
            181 => hashset!{619, 421, 283, 31, 19, 499, 193, 397, 157, 199, 607}, 
            191 => hashset!{599, 461, 251, 137, 3, 173, 227, 281}, 
            193 => hashset!{433, 541, 577, 601, 181, 373, 79, 163, 283}, 
            197 => hashset!{569, 641, 347, 53, 59, 101, 311, 137}, 
            199 => hashset!{211, 379, 37, 181, 373, 109}, 
            211 => hashset!{313, 571, 373, 199, 271, 499, 283, 349}, 
            223 => hashset!{277, 337, 547, 229, 43},
            227 => hashset!{41, 281, 83, 113, 191, 593}, 
            229 => hashset!{3, 157, 499, 373, 223, 433, 613, 631, 547, 7}, 
            233 => hashset!{347, 113, 617, 71, 251, 239}, 
            239 => hashset!{17, 11, 641, 509, 233, 347, 137, 263, 461}, 
            241 => hashset!{421, 97, 127, 313, 601, 13, 271, 79}, 
            251 => hashset!{491, 47, 191, 233, 431, 11, 149, 347}, 
            257 => hashset!{263, 41, 293, 71, 17}, 
            263 => hashset!{293, 443, 239, 647, 257, 71}, 
            269 => hashset!{389, 431, 179, 53, 47, 317, 461, 167, 617}, 
            271 => hashset!{127, 43, 3, 241, 211, 409}, 
            277 => hashset!{331, 73, 499, 37, 157, 223}, 
            281 => hashset!{557, 227, 509, 653, 419, 191}, 
            283 => hashset!{7, 181, 463, 193, 397, 541, 601, 211, 487}, 
            293 => hashset!{263, 467, 173, 89, 257, 47, 311, 617}, 
            307 => hashset!{163, 523, 577, 631, 367, 103}, 
            311 => hashset!{83, 293, 653, 23, 197, 359}, 
            313 => hashset!{619, 241, 211, 109, 37}, 
            317 => hashset!{269, 353, 503, 71, 179, 419}, 
            331 => hashset!{3, 61, 127, 277, 349, 13, 577}, 
            337 => hashset!{349, 223, 397, 607, 13}, 
            347 => hashset!{29, 233, 239, 197, 443, 251, 401, 173}, 
            349 => hashset!{211, 373, 409, 337, 331, 499}, 
            353 => hashset!{443, 11, 53, 359, 137, 317}, 
            359 => hashset!{509, 563, 599, 3, 353, 311, 137, 101}, 
            367 => hashset!{13, 79, 163, 139, 307, 613, 457}, 
            373 => hashset!{97, 211, 229, 127, 3, 193, 199, 349, 661}, 
            379 => hashset!{199, 397, 97}, 
            383 => hashset!{419, 101, 17, 29, 179, 113}, 
            389 => hashset!{269, 17, 71}, 
            397 => hashset!{79, 151, 283, 337, 379, 547, 181}, 
            401 => hashset!{593, 347, 29, 53}, 
            409 => hashset!{61, 349, 163, 271}, 
            419 => hashset!{47, 563, 59, 449, 443, 599, 53, 281, 383, 317}, 
            421 => hashset!{241, 181, 103, 433, 607, 643, 661}, 
            431 => hashset!{89, 479, 17, 173, 251, 269}, 
            433 => hashset!{19, 193, 571, 151, 229, 7, 421}, 
            439 => hashset!{601, 613, 541, 661}, 
            443 => hashset!{419, 89, 167, 263, 353, 71, 347, 83}, 
            449 => hashset!{107, 83, 419, 3, 17, 563, 557, 131}, 
            457 => hashset!{367, 643, 139}, 
            461 => hashset!{653, 479, 569, 191, 269, 239}, 
            463 => hashset!{643, 37, 283, 523, 613}, 
            467 => hashset!{3, 293, 587, 617, 101, 641}, 
            479 => hashset!{593, 569, 599, 131, 461, 431}, 
            487 => hashset!{283, 7, 601, 61}, 
            491 => hashset!{17, 251, 653, 137, 149, 167, 593}, 
            499 => hashset!{211, 3, 229, 43, 151, 181, 277, 349}, 
            503 => hashset!{563, 11, 647, 317, 653}, 
            509 => hashset!{281, 359, 647, 23, 239}, 
            521 => hashset!{659, 167, 641, 47, 89, 557}, 
            523 => hashset!{541, 7, 577, 13, 463, 307}, 
            541 => hashset!{523, 193, 3, 661, 571, 7, 439, 283}, 
            547 => hashset!{397, 229, 7, 139, 67, 223, 577, 643, 661, 73}, 
            557 => hashset!{281, 521, 3, 449}, 
            563 => hashset!{419, 449, 587, 503, 359, 83, 149}, 
            569 => hashset!{461, 197, 29, 479, 659}, 
            571 => hashset!{19, 211, 157, 433, 73, 541}, 
            577 => hashset!{331, 307, 193, 613, 19, 523, 13, 547}, 
            587 => hashset!{137, 11, 467, 563, 617}, 
            593 => hashset!{227, 179, 479, 41, 401, 491, 647}, 
            599 => hashset!{419, 29, 359, 191, 479}, 
            601 => hashset!{439, 193, 283, 487, 67, 127, 241}, 
            607 => hashset!{127, 181, 73, 151, 337, 619, 37, 421, 3}, 
            613 => hashset!{43, 229, 367, 661, 577, 163, 439, 3, 463, 79}, 
            617 => hashset!{233, 269, 467, 131, 3, 293, 647, 587}, 
            619 => hashset!{607, 313, 139, 67, 181}, 
            631 => hashset!{79, 307, 151, 229}, 
            641 => hashset!{131, 197, 239, 467, 521, 101, 167}, 
            643 => hashset!{547, 457, 463, 73, 421}, 
            647 => hashset!{263, 503, 509, 113, 593, 617}, 
            653 => hashset!{281, 491, 311, 461, 53, 503, 659}, 
            659 => hashset!{521, 137, 653, 173, 569}, 
            661 => hashset!{439, 373, 109, 421, 541, 547, 139, 613}, 
            673 => hashset!{3, 7, 109}, 
        };
        assert_eq!(
            find_valid_sets(hashset!{109, 673}, &map, 4),
            vec![hashset!{3, 7, 109, 673}]
        )
    }
}
