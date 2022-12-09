
def priority(t: str) -> int:
    value = ord(t.lower()) - ord('a') + 1

    if 'a' <= t <= 'z':
        return value
    elif 'A' <= t <= 'Z':
        return value + 26


def read_group(f) -> list[str]:
    lines = []
    for _ in range(3):
        line = f.readline()
        if line == '':
            return None

        lines.append(line.strip())

    return lines

#

f = open("input.txt", 'r')

badges_sum = 0

while True:
    lines = read_group(f)
    if lines is None:
        break

    # print(f"Lines:\n{lines}")

    sacks = list(map(
        lambda line: set(line),
        lines))

    # print(f"Sacks:\n{lines}")
    
    badge = list(sacks[0] & sacks[1] & sacks[2])[0]
    badges_sum += priority(badge)

print(f"Sum of badges: {badges_sum}")

f.close()