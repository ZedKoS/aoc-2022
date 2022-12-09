
def priority(t: str) -> int:
    value = ord(t.lower()) - ord('a') + 1

    if 'a' <= t <= 'z':
        return value
    elif 'A' <= t <= 'Z':
        return value + 26

# 

f = open("input.txt", 'r')

priority_sum = 0

for line in f.readlines():
    line = line.strip()

    half = len(line) // 2
    c1, c2 = line[:half], line[half:]

    # print(f"{i+1: 4}) 1: {c1: <30} | 2: {c2: <30}")

    c1, c2 = set(c1), set(c2)
    shared = list(c1 & c2)[0]

    # print(f"    Shared: {shared}")

    priority_sum += priority(shared)

print(f"Sum of priorities: {priority_sum}")

f.close()