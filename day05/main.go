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
	lines := readInput("input.txt")

	// Allocate 1000x1000 buffer
	n := 1000
	board := make([][]int, n)
	for i := 0; i < n; i++ {
		board[i] = make([]int, n)
	}

	for _, line := range lines {
		if line.start.x == line.end.x {
			// Vertical line
			start, end := minMax(line.start.y, line.end.y)
			for y := start; y <= end; y++ {
				board[y][line.start.x] += 1
			}
		} else if line.start.y == line.end.y {
			// Horizontal line
			start, end := minMax(line.start.x, line.end.x)
			for x := start; x <= end; x++ {
				board[line.start.y][x] += 1
			}
		} else {
			// Part2
			// Diagonal lines
			dx, dy := dxdy(line)
			x, y := line.start.x, line.start.y
			for x != line.end.x && y != line.end.y {
				board[y][x] += 1
				x += dx
				y += dy
			}
			board[y][x] += 1
		}
	}

	overlaps := 0
	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			if board[i][j] >= 2 {
				overlaps += 1
			}
		}
	}

	fmt.Println("Overlaps", overlaps)
}

type point struct {
	x int
	y int
}

type line struct {
	start point
	end   point
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
		if lineText == "" {
			continue
		}
		parts := strings.Split(lineText, " -> ")
		from := strings.Split(parts[0], ",")
		to := strings.Split(parts[1], ",")
		line := line{
			start: point{
				x: atoi(from[0]),
				y: atoi(from[1]),
			},
			end: point{
				x: atoi(to[0]),
				y: atoi(to[1]),
			},
		}
		lines = append(lines, line)
	}

	return lines
}

func minMax(a int, b int) (int, int) {
	if a < b {
		return a, b
	} else {
		return b, a
	}
}

func atoi(in string) int {
	r, _ := strconv.Atoi(in)
	return r
}

// Get movement vector from starting point
func dxdy(line line) (int, int) {
	sx, sy := line.start.x, line.start.y
	ex, ey := line.end.x, line.end.y
	if sx < ex && sy < ey {
		return 1, 1
	} else if sx > ex && sy < ey {
		return -1, 1
	} else if sx < ex && sy > ey {
		return 1, -1
	} else if sx > ex && sy > ey {
		return -1, -1
	} else {
		log.Fatal("Impossible case", line)
		return 0, 0
	}
}
