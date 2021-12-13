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
	var points, folds = readInput("input.txt")

	for i, fold := range folds {
		var pointsAfterFold = map[point]bool{}
		for _, p := range points {
			if fold.axis == "x" {
				if p.x > fold.value {
					var dist = p.x - fold.value
					p.x -= 2 * dist
				}
			} else {
				if p.y > fold.value {
					var dist = p.y - fold.value
					p.y -= 2 * dist
				}
			}
			pointsAfterFold[p] = true
		}

		points = make([]point, len(pointsAfterFold))
		for p := range pointsAfterFold {
			points = append(points, p)
		}

		if i == 0 {
			fmt.Println("Part 1", len(pointsAfterFold))
		}
	}

	// Print Part 2
	fmt.Println("\nPart 2")

	var display [][]bool = make([][]bool, 6)
	for i := 0; i < len(display); i++ {
		display[i] = make([]bool, 40)
	}

	for _, p := range points {
		display[p.y][p.x] = true
	}

	for y := 0; y < len(display); y++ {
		for x := 0; x < len(display[0]); x++ {
			if display[y][x] {
				fmt.Print("#")
			} else {
				fmt.Print(".")
			}
		}
		fmt.Print("\n")
	}
}

type point struct {
	x int
	y int
}

type fold struct {
	axis  string
	value int
}

func readInput(path string) ([]point, []fold) {
	file, err := os.Open(path)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	var points []point
	for scanner.Scan() {
		lineText := scanner.Text()
		if lineText == "" {
			break
		}

		p := strings.Split(lineText, ",")
		points = append(points, point{atoi(p[0]), atoi(p[1])})
	}

	var folds []fold
	for scanner.Scan() {
		lineText := scanner.Text()
		f := strings.Split(lineText[11:], "=")
		folds = append(folds, fold{f[0], atoi(f[1])})
	}

	return points, folds
}

func atoi(in string) int {
	r, _ := strconv.Atoi(in)
	return r
}
