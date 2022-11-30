package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
)

func main() {
	mapping := map[rune]rune{
		'(': ')',
		'[': ']',
		'{': '}',
		'<': '>',
	}
	scoreTable := map[rune]int{
		')': 3,
		']': 57,
		'}': 1197,
		'>': 25137,
	}
	autocompleteScoreTable := map[rune]int{
		')': 1,
		']': 2,
		'}': 3,
		'>': 4,
	}

	lines := readInput("input.txt")

	score := 0
	var autocompleteScores []int

	for _, line := range lines {
		var stack []rune
		error := false

		for _, current := range line {
			if containsKey(mapping, current) {
				stack = append(stack, mapping[current]) // Store closing bracket in stack
			} else {
				expected := stack[len(stack)-1]
				stack = stack[:len(stack)-1]
				if expected != current {
					fmt.Println("Expected", string(expected), "but found", string(current))
					score += scoreTable[current]
					error = true
					break
				}
			}
		}

		if !error {
			// Incomplete line
			autocompleteScore := 0
			for i := len(stack) - 1; i >= 0; i-- {
				autocompleteScore *= 5
				autocompleteScore += autocompleteScoreTable[stack[i]]
			}
			autocompleteScores = append(autocompleteScores, autocompleteScore)
		}
	}

	fmt.Println("Part 1", score)

	sort.Ints(autocompleteScores)
	fmt.Println("Part 2", autocompleteScores[len(autocompleteScores)/2])
}

func readInput(path string) []string {
	file, err := os.Open(path)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var lines []string
	for scanner.Scan() {
		lineText := scanner.Text()
		lines = append(lines, lineText)
	}

	return lines
}

func containsKey(l map[rune]rune, r rune) bool {
	for k := range l {
		if k == r {
			return true
		}
	}
	return false
}
