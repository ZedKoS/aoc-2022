use std::fs;

use day4::part1;

fn main() -> Result<(), std::io::Error> {
    let contents = fs::read_to_string("input.txt")?;
    let result = part1(&contents);
    println!("Result: {result}");
    Ok(())
}
