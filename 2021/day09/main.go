package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
)

func main() {
	grid := readInput("input.txt")

	height := len(grid)
	width := len(grid[0])

	get := func(x int, y int) int {
		if x >= 0 && x < width && y >= 0 && y < height {
			return grid[y][x]
		} else {
			return 9
		}
	}

	// Find all low points
	riskLevel := 0
	var lowPoints []point
	for y := 0; y < height; y++ {
		for x := 0; x < width; x++ {
			location := get(x, y)
			left := get(x-1, y)
			right := get(x+1, y)
			up := get(x, y-1)
			down := get(x, y+1)

			if left > location && right > location && up > location && down > location {
				riskLevel += location + 1
				lowPoints = append(lowPoints, point{x: x, y: y})
			}
		}
	}

	fmt.Println("Part1", riskLevel)

	// Calculate basin for each low point
	var basins []int
	for _, p := range lowPoints {
		pointsToCheck := []point{p}
		var basin []point
		for len(pointsToCheck) > 0 {
			location := pointsToCheck[0]
			pointsToCheck = pointsToCheck[1:]

			if contains(basin, location) {
				// Ensures every point is only counted once
				continue
			}
			basin = append(basin, location)

			x, y := location.x, location.y
			locationValue := get(x, y)
			check := func(p point) {
				neighbourValue := get(p.x, p.y)
				if neighbourValue < 9 && neighbourValue > locationValue {
					pointsToCheck = append(pointsToCheck, p)
				}
			}

			// Check every neighbour
			check(point{x - 1, y})
			check(point{x + 1, y})
			check(point{x, y - 1})
			check(point{x, y + 1})
		}
		basinSize := len(basin)
		basins = append(basins, basinSize)
	}

	// Multiply three largest basins
	sort.Ints(basins)
	topThreeBasins := basins[len(basins)-3:]
	result := topThreeBasins[0] * topThreeBasins[1] * topThreeBasins[2]

	fmt.Println("Part 2", result)
}

func contains(points []point, pt point) bool {
	for _, p := range points {
		if p.x == pt.x && p.y == pt.y {
			return true
		}
	}
	return false
}

type point struct {
	x int
	y int
}

func readInput(path string) [][]int {
	file, err := os.Open(path)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var grid [][]int
	for scanner.Scan() {
		lineText := scanner.Text()
		var lineNumbers []int
		for _, c := range lineText {
			lineNumbers = append(lineNumbers, atoi(c))
		}
		grid = append(grid, lineNumbers)
	}

	return grid
}

func atoi(in rune) int {
	r, _ := strconv.Atoi(string(in))
	return r
}
