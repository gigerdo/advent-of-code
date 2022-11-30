package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func main() {
	grid := readInput("input.txt")

	width := len(grid[0])
	height := len(grid)

	flashes := 0

stepLoop:
	for step := 0; step >= 0; step++ {
		if step == 100 {
			fmt.Println("Part 1", flashes)
		}

		// First, the energy level of each octopus increases by 1.
		for y := range grid {
			for x := range grid[y] {
				if grid[y][x] == -1 {
					grid[y][x] = 1
				} else {
					grid[y][x] += 1
				}
			}
		}

		// Add all octopuses with energy > 9 to the initial queue
		var flashQueue []point
		flash := func(p point) {
			flashQueue = append(flashQueue, p)
			grid[p.y][p.x] = -1 // increased to 0 in the next iteration
			flashes += 1
		}

		for y := range grid {
			for x := range grid[y] {
				if grid[y][x] > 9 {
					flash(point{x, y})
				}
			}
		}

		// Process flashes
		for len(flashQueue) > 0 {
			octopus := flashQueue[0]
			flashQueue = flashQueue[1:]

			for yl := max(octopus.y-1, 0); yl < min(octopus.y+2, height); yl++ {
				for xl := max(octopus.x-1, 0); xl < min(octopus.x+2, width); xl++ {
					if grid[yl][xl] != -1 {
						grid[yl][xl] += 1
						if grid[yl][xl] > 9 {
							flash(point{xl, yl})
						}
					}
				}
			}
		}

		for _, row := range grid {
			for _, cell := range row {
				if cell != -1 {
					continue stepLoop
				}
			}
		}

		fmt.Println("Part 2", step+1)
		break stepLoop
	}

	// printGrid(grid)
}

func printGrid(grid [][]int) {
	for _, row := range grid {
		for _, cell := range row {
			if cell == -1 {
				fmt.Print(" x")
			} else {
				fmt.Printf("%2d", cell)
			}
		}
		fmt.Print("\n")
	}
}

func max(a int, b int) int {
	if a > b {
		return a
	} else {
		return b
	}
}

func min(a int, b int) int {
	if a < b {
		return a
	} else {
		return b
	}
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
			lineNumbers = append(lineNumbers, atoi(string(c)))
		}
		grid = append(grid, lineNumbers)
	}

	return grid
}

func atoi(in string) int {
	r, _ := strconv.Atoi(in)
	return r
}
