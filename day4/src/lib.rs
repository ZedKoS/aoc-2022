use std::ops::RangeInclusive;

fn contained_in(a: &RangeInclusive<i32>, b: &RangeInclusive<i32>) -> bool {
    b.start() <= a.start() && a.end() <= b.end()
}

fn overlap(a: &RangeInclusive<i32>, b: &RangeInclusive<i32>) -> bool {
    a.start() <= b.start() && b.start() <= a.end() ||
    b.start() <= a.start() && a.start() <= b.end()
}

pub fn part1(input: &str) -> String {
    let result = input
        .lines()
        .map(|pair_line| {
            let mut elves = pair_line.split(',').map(|elf| {
                let mut range_raw = elf.split('-').map(|value| value.parse::<i32>().unwrap());

                let (a, b) = (range_raw.next().unwrap(), range_raw.next().unwrap());
                a..=b
            });

            let (ref a, ref b) = (elves.next().unwrap(), elves.next().unwrap());

            contained_in(a, b) || contained_in(b, a)
        })
        .filter(|&p| p)
        .count();

    result.to_string()
}

pub fn part2(input: &str) -> String {
    let result = input
        .lines()
        .map(|pair_line| {
            let mut elves = pair_line.split(',').map(|elf| {
                let mut range_raw = elf.split('-').map(|value| value.parse::<i32>().unwrap());

                let (a, b) = (range_raw.next().unwrap(), range_raw.next().unwrap());
                a..=b
            });

            let (ref a, ref b) = (elves.next().unwrap(), elves.next().unwrap());

            overlap(a, b)
        })
        .filter(|&p| p)
        .count();

    result.to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1_test() {
        let example = "2-4,6-8\n\
         2-3,4-5\n\
         5-7,7-9\n\
         2-8,3-7\n\
         6-6,4-6\n\
         2-6,4-8\n";

        let result = part1(example);
        assert_eq!(result, "2")
    }
}
