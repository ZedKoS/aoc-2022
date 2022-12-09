use std::fs;

use day4::part2;

fn main() -> Result<(), std::io::Error> {
    let contents = fs::read_to_string("input.txt")?;
    let result = part2(&contents);
    println!("Result: {result}");
    Ok(())
}
