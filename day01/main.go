package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func main() {
	lines, err := readLines("input.txt")
	if err != nil {
		os.Exit(1)
	}

	part1(lines)
	part2(lines)
}

func readLines(path string) ([]string, error) {
	file, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines, scanner.Err()
}

func part1(lines []string) {
	var prev = -1
	var counter = 0
	for _, line := range lines {
		i, err := strconv.Atoi(line)
		if err != nil {
			os.Exit(1)
		}
		if prev != -1 {
			if i > prev {
				counter += 1
			}
		}
		prev = i
	}
	fmt.Println("Larger measurements:", counter)
}

func part2(lines []string) {
	// previous values, 1 is the oldest
	var prev1 = -1
	var prev2 = -1
	var prev3 = -1

	var counter = 0
	for _, line := range lines {
		i, err := strconv.Atoi(line)
		if err != nil {
			os.Exit(1)
		}
		if prev1 != -1 && prev2 != -1 && prev3 != -1 {
			oldWindow := prev1 + prev2 + prev3
			newWindow := prev2 + prev3 + i
			if oldWindow < newWindow {
				counter += 1
			}
		}
		prev1 = prev2
		prev2 = prev3
		prev3 = i
	}
	fmt.Println("Larger measurements:", counter)
}
