package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
	"unicode"
)

func main() {
	var caves = readInput("input.txt")

	var part1Paths = spelunk(caves, path{"start"}, true)
	fmt.Println("Part 1", len(part1Paths))

	var part2Paths = spelunk(caves, path{"start"}, false)
	fmt.Println("Part 2", len(part2Paths))
}

func spelunk(caves cavesystem, pathSoFar path, jokerVisit bool) []path {
	var currentCave = pathSoFar[len(pathSoFar)-1]
	if currentCave == "end" {
		return []path{pathSoFar}
	}

	var nextCaves = caves[currentCave]
	var fullPaths []path
	for nextCave := range nextCaves {
		if nextCave == "start" {
			continue
		}

		var result []path
		if !jokerVisit && smallCave(nextCave) && alreadyVisited(nextCave, pathSoFar) {
			// jokerVisit not yet used, and is small cave that has already been visited
			result = spelunk(caves, append(pathSoFar, nextCave), true)
		} else if !smallCave(nextCave) || !alreadyVisited(nextCave, pathSoFar) {
			// either large cave or not yet visited small cave
			result = spelunk(caves, append(pathSoFar, nextCave), jokerVisit)
		}

		fullPaths = append(fullPaths, result...)
	}
	return fullPaths
}

func smallCave(cave string) bool {
	return unicode.IsLower(rune(cave[0]))
}

func alreadyVisited(cave string, path path) bool {
	for _, c := range path {
		if cave == c {
			return true
		}
	}
	return false
}

type cavesystem = map[string]map[string]bool // Using map[string]bool as a set for caves

type path = []string

func readInput(path string) cavesystem {
	file, err := os.Open(path)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var caveSystem = cavesystem{}
	for scanner.Scan() {
		lineText := scanner.Text()
		caves := strings.Split(lineText, "-")
		start := caves[0]
		end := caves[1]
		addIfAbsent(caveSystem, start)
		caveSystem[start][end] = true
		addIfAbsent(caveSystem, end)
		caveSystem[end][start] = true
	}

	return caveSystem
}

func addIfAbsent(caves cavesystem, cave string) {
	var _, exists = caves[cave]
	if !exists {
		caves[cave] = map[string]bool{}
	}
}
