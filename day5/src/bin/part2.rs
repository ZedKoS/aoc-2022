use day5::part2;
use std::io;

fn main() -> io::Result<()> {
    let contents = std::fs::read_to_string("input.txt")?;
    let result = part2(&contents);

    println!("Part 2 answer: {result}");
    Ok(())
}
