use std::fs;
use std::collections::HashSet;

// Part 1
fn num_any_yes_answers(group: &str) -> usize {
    let mut set: HashSet<char> = HashSet::new();
    group.chars().for_each(|c| if !c.is_whitespace() { set.insert(c); } );
    return set.len();
}

// Part 2
fn num_all_yes_answers(group: &str) -> usize {
    let persons: Vec<&str> = group.split("\n").collect();
    let mut sets: Vec<HashSet<char>> = persons.iter().map(|person| {
        let mut set: HashSet<char> = HashSet::new();
        person.chars().for_each(|c| if !c.is_whitespace() { set.insert(c); } );
        set
    }).collect();
    let init = sets.pop().unwrap();
    let set: HashSet<char> = sets.iter().fold(init,
        |accum, s| accum.intersection(s).cloned().collect());
    return set.len();
}

fn main() {
    let contents = fs::read_to_string("input").unwrap();
    let v: Vec<&str> = contents.split("\n\n").collect();
    let fs: Vec<fn(&str) -> usize> = vec![num_any_yes_answers, num_all_yes_answers];

    for f in fs {
        let count = v.iter().map(|s| f(s)).fold(0, |sum, c| sum + c);
        println!("{}", count);
    }
}
