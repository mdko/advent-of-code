import sys

with open("input", "r") as f:
    arr = f.readlines()

# Part 1
def part1():
    for i in range(len(arr)):
        x = int(arr[i])
        for j in range(len(arr)):
            if i != j:
                y = int(arr[j])
                if (x + y) == 2020:
                    print(x * y)
                    return
    print("None found")

# Part 2
def part2():
    for i in range(len(arr)):
        x = int(arr[i])
        for j in range(len(arr)):
            if i == j:
                continue
            y = int(arr[j])
            if x + y > 2020:
                continue  # Speed up
            for k in range(len(arr)):
                if i == k or j == k:
                    continue
                assert (i != j) and (i != k) and (j != k)
                z = int(arr[k])
                if (x + y + z) == 2020:
                    print(x * y * z)
                    return
    print("None found")

part1()
part2()