
import sys
import itertools as it

#

GROUP_SIZE = 7

#

def group_monkeys(lines):
    lines = iter(lines)
    while True:
        monkey = []
        for i in range(GROUP_SIZE):
            try:
                monkey.append(next(lines).strip())
            except StopIteration:
                yield monkey
                return
        yield monkey

def monkey_items(monkey: list[str]) -> list[str]:
    ln = monkey[1]
    return list(map(int, ln[ln.find(":") + 2:].split(", ")))

def monkey_eq(monkey: list[str]):
    ln = monkey[2]
    terms = ln[ln.find(":") + 2:].split(" ")[-3:]

    def eq(old):
        a = old if terms[0] == "old" else int(terms[0])
        b = old if terms[2] == "old" else int(terms[2])
        if terms[1] == '+': return a + b
        else: return a * b
    
    return eq

def monkey_test_factor(monkey: list[str]):
    ln = monkey[3]
    num = int(ln.split("by ")[1])
    return num


def monkey_test(monkey: list[str]):
    num = monkey_test_factor(monkey)
    def test(v):
        return v % num == 0
    return test


def monkey_recv(monkey: list[str]):
    def f(test):
        n = 4 if test else 5
        return int(monkey[n][-1])
    return f

def make_monkey(monkey: list[str]) -> dict:
    return {
        "items": monkey_items(monkey),
        "eq": monkey_eq(monkey),
        "test": monkey_test(monkey),
        "recv": monkey_recv(monkey),
        "insp": 0,
        "fact": monkey_test_factor(monkey)
    }

def find_factors(monkeys):
    acc = 1
    for m in monkeys:
        f = m["fact"]
        acc *= f
    return acc

def run_monkey(monkeys, fact, id: int):
    monkey = monkeys[id]

    for _ in range(len(monkey["items"])):
        item = monkey["items"].pop(0)
        monkey["insp"] += 1

        test = monkey["test"]

        # worry = int(monkey["eq"](item) / 3) % fact
        worry = int(monkey["eq"](item)) % fact

        t = test(worry)
        recv = monkey["recv"](t)

        monkeys[recv]["items"].append(worry)

def run_round(monkeys, fact):
    for i in range(len(monkeys)):
        run_monkey(monkeys, fact, i)

#

if len(sys.argv) == 1:
    input_file = "input.txt"
else:
    input_file = sys.argv[1]

file = open(input_file, "r")

monkeys = list(map(make_monkey, group_monkeys(file.readlines())))

f = find_factors(monkeys)

# for i in range(20):
for i in range(10_000):
    # print(i)
    run_round(monkeys, f)

a, b = sorted(monkeys,
    key=lambda monke: monke["insp"],
    reverse=True)[:2]

business = a["insp"] * b["insp"]

print(f"Monke business is: {business}")

file.close()