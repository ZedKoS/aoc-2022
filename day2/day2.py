
ROCK     = 1
PAPER    = 2
SCISSORS = 3

def counter(move):
    return move % 3 + 1

def uncounter(move):
    return counter(counter(move))


def translateMove(move: str) -> int:
    if move == 'A' or move == 'X':
        return ROCK
    if move == 'B' or move == 'Y':
        return PAPER
    if move == 'C' or move == 'Z':
        return SCISSORS


def run_match(opp, move) -> int:
    opp_counter = opp % 3 + 1
    if move == opp_counter:
        return 6
    elif move == opp:
        return 3
    else:
        return 0


def choose_move(opp, outcome) -> int:
    # lose
    if outcome == 'X':
        return uncounter(opp)
    # draw
    elif outcome == 'Y':
        return opp
    # win
    elif outcome == 'Z':
        return counter(opp)

#

f = open("input.txt", "r")

score = 0

for line in f.readlines():
    opp, outcome = line.strip().split(' ')

    opp = translateMove(opp)
    move = choose_move(opp, outcome)

    outcome = run_match(opp, move)

    score += outcome + move
    
print(f"Final score: {score}")

f.close()
