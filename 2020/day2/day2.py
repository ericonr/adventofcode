#!/usr/bin/python3

count = 0
lines = 0
count2 = 0 # for part 2
with open("data", "r") as f:
    for line in f:
        lines += 1
        parts = line.split(' ')
        amount = parts[0]
        char = parts[1]
        pwd = parts[2]

        first, second = amount.split('-')
        first, second = int(first), int(second)

        c = char[0]
        cpwd = pwd.count(c)
        if not first <= cpwd <= second:
            #print(f"bad: {first}-{second} {c}: {pwd} - has {cpwd}")
            count += 1

        l = len(pwd)
        match = 0
        for p in [first, second]:
            if l >= p:
                if pwd[p - 1] == c:
                    match += 1
        if match != 1:
            count2 += 1

print(f'bad count: {count}')
print(f'good count: {lines - count}')
print(f'total lines: {lines}')

print(f'2: bad count: {count2}')
print(f'2: good count: {lines - count2}')
