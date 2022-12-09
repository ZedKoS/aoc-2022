use day5::part1;
use std::io;

fn main() -> io::Result<()> {
    let contents = std::fs::read_to_string("input.txt")?;
    let result = part1(&contents);

    println!("Part 1 answer: {result}");
    Ok(())
}
