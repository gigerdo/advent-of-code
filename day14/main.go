package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"strings"
)

func main() {
	var polymerTemplate, rules = readInput("input.txt")

	fmt.Println("Part 1", count(polymerTemplate, rules, 10))
	fmt.Println("Part 2", count(polymerTemplate, rules, 40))
}

func count(template string, rules map[string]string, steps int) int {
	var pairs = make(map[string]int)

	for i := 0; i < len(template)-1; i++ {
		var pair = template[i : i+2]
		pairs[pair] += 1
	}

	for step := 0; step < steps; step++ {
		var pairsNext = make(map[string]int)
		for k, v := range pairs {
			var rule, ruleExists = rules[k]
			if ruleExists {
				pairsNext[string(k[0])+rule] += v
				pairsNext[rule+string(k[1])] += v
			} else {
				pairsNext[k] += v
			}
		}
		pairs = pairsNext
	}

	// The final expanded polymer is the first char of every pair + the very last char of the input polymer
	var results = make(map[rune]int)
	for k, v := range pairs {
		results[rune(k[0])] += v
	}
	results[rune(template[len(template)-1])] += 1

	var min = math.MaxInt64
	var max = 0
	for _, v := range results {
		if v != 0 {
			if v < min {
				min = v
			}
			if v > max {
				max = v
			}
		}
	}

	return max - min
}

func readInput(path string) (string, map[string]string) {
	file, err := os.Open(path)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	scanner.Scan()
	polymerTemplate := scanner.Text()

	// Empty newline
	scanner.Scan()

	var rules = make(map[string]string)
	for scanner.Scan() {
		lineText := scanner.Text()

		r := strings.Split(lineText, " -> ")
		rules[r[0]] = r[1]
	}

	return polymerTemplate, rules
}
