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
	lines := readInput("input.txt")

	part1(lines)

	sumOfOutput := 0

	for _, line := range lines {
		var digits [10]string

		// First pass, unique length
		for _, pattern := range line.pattern {
			switch len(pattern) {
			case 2: // 1
				digits[1] = pattern
			case 3: // 7
				digits[7] = pattern
			case 4: // 4
				digits[4] = pattern
			case 5: // 2, 3, 5
			case 6: // 0, 6, 9
			case 7: // 8
				digits[8] = pattern
			}
		}

		// Second pass, decode based on already known digits (1, 4, 7, 8)
		for _, pattern := range line.pattern {
			switch len(pattern) {
			case 6: // 0, 6, 9
				if stringContainsChars(pattern, digits[4]) {
					// 4 is only contained in 9
					digits[9] = pattern
				} else if stringContainsChars(pattern, digits[7]) {
					// 0 contains 7, 6 doesn't
					digits[0] = pattern
				} else {
					digits[6] = pattern
				}
			}
		}

		// Third pass for 2, 3, 5
		for _, pattern := range line.pattern {
			switch len(pattern) {
			case 5:
				if stringContainsChars(pattern, digits[1]) {
					// 1 is only contained in 3
					digits[3] = pattern
				} else if stringContainsChars(digits[6], pattern) {
					// 6 only contains 5, not 2
					digits[5] = pattern
				} else {
					digits[2] = pattern
				}
			}
		}

		decodedOutput := 0
		for outputIndex, value := range line.output {
			for digit, pattern := range digits {
				if equalCharsInStrings(value, pattern) {
					decodedOutput += int(math.Pow10(3-outputIndex)) * digit
				}
			}
		}
		sumOfOutput += decodedOutput
	}

	fmt.Println("Part2", sumOfOutput)
}

func stringContainsChars(a string, b string) bool {
	equal := true
	for _, c := range b {
		if !strings.Contains(a, string(c)) {
			return false
		}
	}
	return equal
}

func equalCharsInStrings(a string, b string) bool {
	return stringContainsChars(a, b) && stringContainsChars(b, a)
}

func part1(lines []line) {
	numberOfDigits := 0
	for _, line := range lines {

		for _, value := range line.output {
			switch len(value) {
			case 2:
				numberOfDigits += 1 // 1
			case 3:
				numberOfDigits += 1 // 7
			case 4:
				numberOfDigits += 1 // 4
			case 7:
				numberOfDigits += 1 // 8
			}
		}
	}
	fmt.Println("Part1", numberOfDigits)
}

type line struct {
	pattern []string
	output  []string
}

func readInput(path string) []line {
	file, err := os.Open(path)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var lines []line
	for scanner.Scan() {
		lineText := scanner.Text()
		inputOutput := strings.Split(lineText, " | ")
		input := strings.Fields(inputOutput[0])
		output := strings.Fields(inputOutput[1])
		lines = append(lines, line{
			pattern: input,
			output:  output,
		})
	}

	return lines
}
