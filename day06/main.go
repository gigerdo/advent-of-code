package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func main() {
	school := readInput("input.txt")

	for day := 0; day < 256; day++ {
		zeroes := school[0]
		school = append(school[1:], zeroes)
		school[6] += zeroes

		if day == 79 {
			fmt.Println("Part 1", sum(school))
		}
	}

	fmt.Println("Part 2", sum(school))
}

func sum(school []uint64) uint64 {
	sum := uint64(0)
	for _, fish := range school {
		sum += fish
	}
	return sum
}

func readInput(path string) []uint64 {
	file, err := os.Open(path)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	school := make([]uint64, 9)
	scanner.Scan()
	line := scanner.Text()
	numbers := strings.Split(line, ",")
	for _, fishString := range numbers {
		fish := atoi(fishString)
		school[fish] += 1
	}

	return school
}

func atoi(in string) int {
	r, _ := strconv.Atoi(in)
	return r
}
