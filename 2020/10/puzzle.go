package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
)

func main() {
	file, err := os.Open("input")
	if err != nil {
		panic(err)
	}

	scanner := bufio.NewScanner(file)
	scanner.Split(bufio.ScanLines)

	jolts := []int{0}

	for scanner.Scan() {
		n, err := strconv.Atoi(scanner.Text())
		if err != nil {
			panic(err)
		}
		jolts = append(jolts, n)
	}

	sort.Ints(jolts)

	jolts = append(jolts, jolts[(len(jolts)-1)]+3)

	file.Close()

	part1(jolts)
	part2(jolts)
}

func part1(jolts []int) {
	var three_bit_diffs int = 0
	var one_bit_diffs int = 0

	for i, jolt := range jolts {
		if i == 0 {
			continue
		}
		diff := jolt - jolts[i-1]
		if diff == 1 {
			one_bit_diffs++
		} else if diff == 3 {
			three_bit_diffs++
		}
	}
	fmt.Println(three_bit_diffs * one_bit_diffs)
}

func part2(jolts []int) {
	count := count_paths(jolts)
	fmt.Println(count)
}

func count_paths(jolts []int) int {
	i := len(jolts) - 2
	npaths := make([]int, len(jolts))
	copy(npaths, jolts)
	npaths[i+1] = 1
	// Important that we traversing topologically (by virtue of them being sorted)
	for {
		jolt := jolts[i]
		ndescendants := 0
		j := i + 1
		for {
			if j >= len(jolts) {
				break
			}
			next := jolts[j]
			if (next - jolt) < 4 {
				ndescendants += npaths[j]
			} else {
				break
			}
			j++
		}
		npaths[i] = ndescendants
		if i == 0 {
			break
		}
		i--
	}
	return npaths[0]
}
