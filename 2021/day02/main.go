package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	lines, err := transform("input.txt")
	if err != nil {
		os.Exit(1)
	}

	fmt.Println("Part1")
	part1(lines)
	fmt.Println("\nPart2")
	part2(lines)
}

func part1(lines []string) {
	depth := 0
	horizontalPos := 0

	for _, line := range lines {
		values := strings.Split(line, " ")
		direction := values[0]
		amount, err := strconv.Atoi(values[1])
		if err != nil {
			os.Exit(1)
		}

		switch direction {
		case "forward":
			horizontalPos += amount
		case "down":
			depth += amount
		case "up":
			depth -= amount
		default:
			fmt.Println("Unknown command", direction)
		}
	}

	fmt.Println("Depth", depth, "Horizontal pos", horizontalPos)
	fmt.Println("Result", depth*horizontalPos)
}

func part2(lines []string) {
	depth := 0
	horizontalPos := 0
	aim := 0

	for _, line := range lines {
		values := strings.Split(line, " ")
		direction := values[0]
		amount, err := strconv.Atoi(values[1])
		if err != nil {
			os.Exit(1)
		}

		switch direction {
		case "forward":
			horizontalPos += amount
			depth += aim * amount
		case "down":
			aim += amount
		case "up":
			aim -= amount
		default:
			fmt.Println("Unknown command", direction)
		}
	}

	fmt.Println("Depth", depth, "Horizontal pos", horizontalPos)
	fmt.Println("Result", depth*horizontalPos)
}

func transform(path string) ([]string, error) {
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
