package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
)

func main() {
	crabs := readInput("input.txt")

	part1(crabs)
	part2(crabs)
}

func part1(crabs []int) {
	minPosition := -1
	minDistance := math.MaxInt64

	for pos := 0; pos < 1000; pos++ {
		distance := 0
		for _, crab := range crabs {
			distance += abs(pos - crab)
		}

		if distance < minDistance {
			minPosition = pos
			minDistance = distance
		}
	}

	fmt.Println("Part1", minPosition, minDistance)
}

func part2(crabs []int) {
	minPosition := -1
	minCost := math.MaxInt64

	for pos := 0; pos < 1000; pos++ {
		totalCost := 0
		for _, crab := range crabs {
			distance := abs(pos - crab)
			totalCost += (distance*distance + distance) / 2
		}

		if totalCost < minCost {
			minPosition = pos
			minCost = totalCost
		}
	}

	fmt.Println("Part2", minPosition, minCost)
}

func sum(list []int) int {
	sum := 0
	for _, element := range list {
		sum += element
	}
	return sum
}

func readInput(path string) []int {
	file, err := os.Open(path)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	var crabs []int
	scanner.Scan()
	line := scanner.Text()
	numbers := strings.Split(line, ",")
	for _, fishString := range numbers {
		crabs = append(crabs, atoi(fishString))
	}

	return crabs
}

func atoi(in string) int {
	r, _ := strconv.Atoi(in)
	return r
}

func abs(i int) int {
	if i > 0 {
		return i
	} else {
		return -i
	}
}
