use std::cmp::min;
use std::env::args;
use std::fs::read_to_string;
use std::collections::{HashMap, HashSet};

pub struct Dictionary {
    data: HashSet<String>,
}

impl Dictionary {
    pub fn new() -> Self {
        let words = read_to_string("words.txt").expect("Failed to read file");
        let mut dictionary: HashSet<String> = HashSet::new();
        for word in words.lines() {
            dictionary.insert(word.to_string());
        }

        Self { data: dictionary }
    }

    pub fn add(&mut self, word: String) {
        self.data.insert(word);
    }

    pub fn to_vec(&self) -> Vec<&String> {
        self.data.iter().collect()
    }

    pub fn get(&self, key: &str) -> Option<&String> {
        self.data.get(key)
    }
}

pub fn damerau_levenshtein(word: &str, target: &str) -> usize {
    let (word_chars, target_chars): (Vec<char>, Vec<char>) = (word.chars().collect(), target.chars().collect());
    let word_elems = word_chars.as_slice();
    let target_elems = target_chars.as_slice();
    let word_len = word_elems.len();
    let target_len = target_elems.len();

    if word_len == 0 {
        return target_len;
    }
    if target_len == 0 {
        return word_len;
    }

    let width = word_len + 2;
    let mut distances = vec![0; (word_len + 2) * (target_len + 2)];
    let max_distance = word_len + target_len;
    distances[0] = max_distance;

    for i in 0..(word_len + 1) {
        distances[flat_index(i + 1, 0, width)] = max_distance;
        distances[flat_index(i + 1, 1, width)] = i;
    }

    for j in 0..(target_len + 1) {
        distances[flat_index(0, j + 1, width)] = max_distance;
        distances[flat_index(1, j + 1, width)] = j;
    }

    let mut elems: HashMap<char, usize> = HashMap::with_capacity(64);

    for i in 1..(word_len + 1) {
        let mut db = 0;

        for j in 1..(target_len + 1) {
            let k = match elems.get(&target_elems[j - 1]) {
                Some(&value) => value,
                None => 0,
            };

            let insertion_cost = distances[flat_index(i, j + 1, width)] + 1;
            let deletion_cost = distances[flat_index(i + 1, j, width)] + 1;
            let transposition_cost =
                distances[flat_index(k, db, width)] + (i - k - 1) + 1 + (j - db - 1);

            let mut substitution_cost = distances[flat_index(i, j, width)] + 1;
            if word_elems[i - 1] == target_elems[j - 1] {
                db = j;
                substitution_cost -= 1;
            }

            distances[flat_index(i + 1, j + 1, width)] = min(
                substitution_cost,
                min(insertion_cost, min(deletion_cost, transposition_cost)),
            );
        }

        elems.insert(word_elems[i - 1].clone(), i);
    }

    distances[flat_index(word_len + 1, target_len + 1, width)]
}

fn flat_index(i: usize, j: usize, width: usize) -> usize {
    j * width + i
}

pub fn calc_dict_distance(word: &str, num_words: usize, words: &Dictionary) -> Vec<(usize, String)> {
    let mut dict_word_dist = Vec::new();
    
    for dict_word in words.to_vec() {
        let word_distance = damerau_levenshtein(word, dict_word);
        if word_distance <= 3 {
            dict_word_dist.push((word_distance, dict_word.to_string()));
        }
    }
    
    dict_word_dist.sort_by_key(|&(dist, _)| dist);
    
    dict_word_dist.into_iter().take(num_words).collect()
}

fn main() {
    let mut args = args();
    args.next();
    let filename = match args.next() {
        Some(arg) => arg,
        None => {
            println!("Usage: suggest <filename>");
            return;
        }
    };

    let file_content = match read_to_string(&filename) {
        Ok(content) => content,
        Err(_) => {
            println!("Failed to read file '{}'", filename);
            return;
        }
    };

    let num_words = 3;
    let dictionary = Dictionary::new();
    for word in file_content.split_whitespace() {
        if word.chars().all(|c| c.is_alphabetic()) {
            let word_cleaned = word.chars()
                .filter(|c| c.is_alphabetic())
                .map(|c| c.to_lowercase().to_string())
                .collect::<String>();

            if !dictionary.data.contains(&word_cleaned) {
                let closest_words = calc_dict_distance(&word_cleaned, num_words, &dictionary);
                println!("Word: {}", word);
                for (_distance, suggestion) in closest_words {
                    println!("Suggested Change: {} => {}", word, suggestion);
                }
            }
        }
    }
}
