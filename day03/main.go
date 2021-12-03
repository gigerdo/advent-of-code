package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func main() {
	lines := readLines("input.txt")

	// Part1
	bitCounter := calculateMostCommonBits(lines)
	gamma, epsilon := calculateGammaEpsilon(bitCounter)
	fmt.Println("Part1")
	fmt.Println("Gamma rate", gamma)
	fmt.Println("Epsilon rate", epsilon)
	fmt.Println("Power consumption", gamma*epsilon)

	// Part2
	fmt.Println("\nPart2")
	oxygenGeneratorRating := filterLinesByBitCriteria(lines, mostCommon)
	fmt.Println("Oxygen generator rating", oxygenGeneratorRating)
	co2ScrubberRating := filterLinesByBitCriteria(lines, leastCommon)
	fmt.Println("CO2 scrubber rating", co2ScrubberRating)
	fmt.Println("Life support rating", oxygenGeneratorRating*co2ScrubberRating)
}

func readLines(path string) []string {
	file, err := os.Open(path)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines
}

func calculateGammaEpsilon(bitCounter []int) (uint64, uint64) {
	gammaBinaryString := ""
	epsilonBinaryString := ""
	for _, i := range bitCounter {
		gammaBinaryString += strconv.Itoa(mostCommon(i))
		epsilonBinaryString += strconv.Itoa(leastCommon(i))
	}
	gammaRate, _ := strconv.ParseUint(gammaBinaryString, 2, 64)
	epsilonRate, _ := strconv.ParseUint(epsilonBinaryString, 2, 64)

	return gammaRate, epsilonRate
}

func filterLinesByBitCriteria(lines []string, bitCriteria func(int) int) int {
	filteredLines := lines
	numberOfBits := len(lines[0])
	for currentBitIndex := 0; currentBitIndex < numberOfBits; currentBitIndex++ {
		bitCounter := calculateMostCommonBitAtIndex(filteredLines, currentBitIndex)
		bitToKeep := bitCriteria(bitCounter)
		var temp []string
		for _, line := range filteredLines {
			bitToCheck, _ := strconv.Atoi(string(line[currentBitIndex]))
			if bitToCheck == bitToKeep {
				temp = append(temp, line)
			}
		}
		filteredLines = temp
		if len(filteredLines) == 1 {
			break
		}
	}
	decimalValue, _ := strconv.ParseUint(filteredLines[0], 2, 64)
	return int(decimalValue)
}

// Counts how many more 1s exist compared to 0s per bit
//   >0 more 1s
//   <0 more 0s
//   =0 equal amounts
func calculateMostCommonBits(lines []string) []int {
	bitCounter := make([]int, len(lines[0]))
	for _, line := range lines {
		for pos, char := range line {
			switch char {
			case '0':
				bitCounter[pos] -= 1
			case '1':
				bitCounter[pos] += 1
			}
		}
	}
	return bitCounter
}

// Same as calculateMostCommonBits but only for one bit at index
func calculateMostCommonBitAtIndex(lines []string, index int) int {
	bitCounter := 0
	for _, line := range lines {
		switch line[index] {
		case '0':
			bitCounter -= 1
		case '1':
			bitCounter += 1
		}
	}
	return bitCounter
}

func mostCommon(i int) int {
	// determine the most common value (0 or 1) in the current bit position,
	// and keep only numbers with that bit in that position. If 0 and 1 are equally
	// common, keep values with a 1 in the position being considered.
	if i > 0 {
		return 1
	} else if i < 0 {
		return 0
	} else {
		return 1
	}
}

func leastCommon(i int) int {
	// determine the least common value (0 or 1) in the current bit position,
	// and keep only numbers with that bit in that position. If 0 and 1 are equally
	// common, keep values with a 0 in the position being considered.
	if i > 0 {
		return 0
	} else if i < 0 {
		return 1
	} else {
		return 0
	}
}
