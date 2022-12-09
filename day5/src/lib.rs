use nom::{
    self, branch::*, bytes::complete::*, character::complete::*, multi::*, sequence::delimited,
};

#[derive(PartialEq, Clone, Copy, Debug)]
struct Crate(char);
type CrateStack = Vec<Crate>;

const EMPTY_SPOT: Crate = Crate(' ');

#[derive(Clone, Copy)]
struct Action {
    crates: u32,
    from: u32,
    to: u32,
}

pub fn part1(input: &str) -> String {
    let (input, mut stacks) = parse_crate_stacks(input).unwrap();
    dbg!(&stacks);

    let (input, _) = newline::<&str, ()>(input).unwrap();
    let (_, actions) = parse_actions(input).unwrap();

    execute_actions1(&mut stacks, &actions);

    let mut tops = String::with_capacity(stacks.len());
    stacks.iter().for_each(|s| {
        if let Some(Crate(last)) = s.last().cloned() {
            tops.push(last);
        }
    });

    tops
}

pub fn part2(input: &str) -> String {
    let (input, mut stacks) = parse_crate_stacks(input).unwrap();
    dbg!(&stacks);

    let (input, _) = newline::<&str, ()>(input).unwrap();
    let (_, actions) = parse_actions(input).unwrap();

    execute_actions2(&mut stacks, &actions);

    dbg!(&stacks);

    let mut tops = String::with_capacity(stacks.len());
    stacks.iter().for_each(|s| {
        if let Some(Crate(last)) = s.last().cloned() {
            tops.push(last);
        } else {
            tops.push(' ');
        }
    });

    tops
}

fn parse_crate_stacks(input: &str) -> nom::IResult<&str, Vec<CrateStack>> {
    let (input, rows) = separated_list1(newline, parse_row)(input)?;
    let (input, _) = newline(input)?;
    dbg!(&rows);

    let (input, _) = parse_stack_numbers(input)?;

    let stacks = transposed(rows)
        .into_iter()
        .map(|stack| {
            stack
                .into_iter()
                .filter(|&c| c != EMPTY_SPOT)
                .rev()
                .collect()
        })
        .collect();

    Ok((input, stacks))
}

fn transposed<T: Clone>(mat: Vec<Vec<T>>) -> Vec<Vec<T>> {
    let mut transposed = Vec::new();

    let rows = mat.len();
    if rows == 0 {
        return transposed;
    }

    let cols = mat[0].len();

    for _ in 0..cols {
        // add new row to transposed matrix
        transposed.push(Vec::with_capacity(rows));
    }

    for y in 0..rows {
        for x in 0..cols {
            transposed[x].push(mat[y][x].clone());
        }
    }

    transposed
}

fn parse_row(input: &str) -> nom::IResult<&str, Vec<Crate>> {
    let one_crate = |input| {
        let (input, result) = delimited(tag("["), anychar, tag("]"))(input)?;
        Ok((input, Crate(result)))
    };

    let no_crate = |input| {
        let (input, _) = tag("   ")(input)?;
        Ok((input, EMPTY_SPOT))
    };

    separated_list1(tag(" "), alt((one_crate, no_crate)))(input)
}

fn parse_stack_numbers(input: &str) -> nom::IResult<&str, ()> {
    let (input, _) = many1(none_of("\n"))(input)?;
    let (input, _) = newline(input)?;
    Ok((input, ()))
}

fn parse_actions(input: &str) -> nom::IResult<&str, Vec<Action>> {
    let action = |i| {
        let (i, _) = tag("move ")(i)?;
        let (i, n) = u32(i)?;
        let (i, _) = tag(" from ")(i)?;
        let (i, from) = u32(i)?;
        let (i, _) = tag(" to ")(i)?;
        let (i, to) = u32(i)?;
        Ok((
            i,
            Action {
                crates: n,
                from: from - 1,
                to: to - 1,
            },
        ))
    };

    separated_list1(newline, action)(input)
}

fn execute_actions1(stacks: &mut [CrateStack], actions: &[Action]) {
    for &action in actions {
        // move n crates
        for _ in 0..action.crates {
            let c = stacks[action.from as usize].pop();
            if let Some(c) = c {
                stacks[action.to as usize].push(c);
            }
        }
    }
}

fn execute_actions2(stacks: &mut [CrateStack], actions: &[Action]) {
    for &action in actions {
        let from = &mut stacks[action.from as usize];
        let start = if from.len() >= action.crates as usize {
            from.len() - action.crates as usize
        } else {
            0
        };

        let removed = from.drain(start..).collect::<Vec<_>>();
        for c in removed {
            stacks[action.to as usize].push(c);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn transposed_test() {
        let mat = vec![vec![1, 2, 3, 4], vec![5, 6, 7, 8], vec![9, 10, 11, 12]];

        let mat_t = transposed(mat);

        assert_eq!(
            mat_t,
            vec![
                vec![1, 5, 9],
                vec![2, 6, 10],
                vec![3, 7, 11],
                vec![4, 8, 12]
            ]
        )
    }
}
