package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func main() {
	var seafloor = readInput("input.txt")

	var height = len(seafloor)
	var width = len(seafloor[0])

	printSeafloor(seafloor)
	fmt.Println()

	var hasMoved = true
	for step := 0; hasMoved; step++ {
		hasMoved = false

		// Move >
		for y := 0; y < height; y++ {
			for x := 0; x < width; x++ {
				if seafloor[y][x] == '>' {
					var nextPos = (x + 1) % width
					if seafloor[y][nextPos] == '.' {
						seafloor[y][x] = 'x'
						seafloor[y][nextPos] = '*'
						hasMoved = true
					}
				}
			}
		}

		// Clean
		for y := 0; y < height; y++ {
			for x := 0; x < width; x++ {
				if seafloor[y][x] == 'x' {
					seafloor[y][x] = '.'
				} else if seafloor[y][x] == '*' {
					seafloor[y][x] = '>'
				}
			}
		}

		// Move v
		for y := 0; y < height; y++ {
			for x := 0; x < width; x++ {
				if seafloor[y][x] == 'v' {
					var nextPos = (y + 1) % height
					if seafloor[nextPos][x] == '.' {
						seafloor[y][x] = 'x'
						seafloor[nextPos][x] = '&'
						hasMoved = true
					}
				}
			}
		}

		// Clean
		for y := 0; y < height; y++ {
			for x := 0; x < width; x++ {
				if seafloor[y][x] == 'x' {
					seafloor[y][x] = '.'
				} else if seafloor[y][x] == '&' {
					seafloor[y][x] = 'v'
				}
			}
		}

		if !hasMoved {
			fmt.Println("Part 1", step+1)
		}
	}

}

func printSeafloor(seafloor [][]rune) {
	for _, row := range seafloor {
		for _, c := range row {
			fmt.Print(string(c))
		}
		fmt.Print("\n")
	}
}

func readInput(path string) [][]rune {
	var file, err = os.Open(path)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	var seafloor [][]rune

	var scanner = bufio.NewScanner(file)
	for scanner.Scan() {
		lineText := scanner.Text()

		var line []rune
		for _, c := range lineText {
			line = append(line, c)
		}
		seafloor = append(seafloor, line)
	}

	return seafloor
}
