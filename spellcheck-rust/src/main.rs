extern crate ndarray;

use std::fs::File;
use std::io::{stdin, stdout, BufRead, BufReader, Write};
use ndarray::Array2;

fn levenshtein_distance(token1: &str, token2: &str) -> i32 {
    let len_token1 = token1.len();
    let len_token2 = token2.len();

    let mut distances = Array2::<i32>::zeros((len_token1 + 1, len_token2 + 1));

    // Initialize first column
    for t1 in 0..=len_token1 {
        distances[[t1, 0]] = t1 as i32;
    }

    // Initialize first row
    for t2 in 0..=len_token2 {
        distances[[0, t2]] = t2 as i32;
    }

    for t1 in 1..=len_token1 {
        for t2 in 1..=len_token2 {
            let cost = if token1.chars().nth(t1 - 1) == token2.chars().nth(t2 - 1) {
                0
            } else {
                1
            };

            let a = distances[[t1, t2 - 1]];
            let b = distances[[t1 - 1, t2]];
            let c = distances[[t1 - 1, t2 - 1]];

            distances[[t1, t2]] = *[
                a + 1,
                b + 1,
                c + cost
            ].iter().min().unwrap();
        }
    }

    distances[[len_token1, len_token2]]
}

fn calc_dict_distance(word: &str, num_words: usize) -> Vec<String> {
    let mut dict_word_dist = Vec::new();
    
    if let Ok(file) = File::open("words.txt") {
        let reader = BufReader::new(file);
        
        for line in reader.lines() {
            if let Ok(line) = line {
                let word_distance = levenshtein_distance(word, line.trim());
                let word_distance = if word_distance >= 10 { 9 } else { word_distance };
                dict_word_dist.push(format!("{}-{}", word_distance, line.trim()));
            }
        }
    }
    
    dict_word_dist.sort();
    
    let closest_words: Vec<String> = dict_word_dist.iter()
        .take(num_words)
        .map(|dist| dist.split('-').last().unwrap().to_string())
        .collect();
    
    closest_words
}

fn main() {
    let mut in_str = String::new();
    println!("Enter a word: ");
    let _ = stdout().flush();
    stdin().read_line(&mut in_str).expect("No word was entered");
    if let Some('\n') = in_str.chars().next_back() {
        in_str.pop();
    }
    if let Some('\r') = in_str.chars().next_back() {
        in_str.pop();
    }
    let num_words = 5;
    let closest_words = calc_dict_distance(&in_str, num_words);
    
    println!("{:?}", closest_words);
}
