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
	var cubesToAdd = readInput("input.txt")

	part1(cubesToAdd)

	var cubesAdded []cube

	for _, cubeToAdd := range cubesToAdd {
		var deductions []cube

		for _, cube := range cubesAdded {
			var overlap, hasOverlap = calculateOverlap(cubeToAdd, cube)

			if hasOverlap {
				deductions = append(deductions, overlap)
			}
		}

		if cubeToAdd.operation == "on" {
			cubesAdded = append(cubesAdded, cubeToAdd)
		}
		cubesAdded = append(cubesAdded, deductions...)
	}

	var volume = 0
	for _, c := range cubesAdded {
		if c.operation == "on" {
			volume += calculateVolume(c)
		} else {
			volume -= calculateVolume(c)
		}
	}

	fmt.Println("Part 2", volume)
}

func calculateVolume(step cube) int {
	return (step.xmax - step.xmin + 1) * (step.ymax - step.ymin + 1) * (step.zmax - step.zmin + 1)
}

func calculateOverlap(a cube, b cube) (cube, bool) {
	if b.xmin > a.xmax || a.xmin > b.xmax {
		return cube{}, false
	}
	if b.ymin > a.ymax || a.ymin > b.ymax {
		return cube{}, false
	}
	if b.zmin > a.zmax || a.zmin > b.zmax {
		return cube{}, false
	}

	var operation string
	if a.operation == "on" && b.operation == "on" {
		operation = "off"
	} else if a.operation == "off" && b.operation == "on" {
		operation = "off"
	} else if a.operation == "on" && b.operation == "off" {
		operation = "on"
	} else {
		operation = "on"
	}

	return cube{
		operation,
		max(a.xmin, b.xmin), min(a.xmax, b.xmax),
		max(a.ymin, b.ymin), min(a.ymax, b.ymax),
		max(a.zmin, b.zmin), min(a.zmax, b.zmax),
	}, true

}

func part1(steps []cube) {
	var cubes = make(map[point]bool)

	for _, step := range steps {
		for x := max(step.xmin, -50); x <= min(step.xmax, 50); x++ {
			for y := max(step.ymin, -50); y <= min(step.ymax, 50); y++ {
				for z := max(step.zmin, -50); z <= min(step.zmax, 50); z++ {
					if step.operation == "on" {
						cubes[point{x, y, z}] = true
					} else {
						delete(cubes, point{x, y, z})
					}

				}
			}
		}
	}

	fmt.Println("Part 1", len(cubes))
}

type point struct {
	x int
	y int
	z int
}

type cube struct {
	operation  string
	xmin, xmax int
	ymin, ymax int
	zmin, zmax int
}

func readInput(path string) []cube {
	var file, err = os.Open(path)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	var scanner = bufio.NewScanner(file)

	var steps []cube

	for scanner.Scan() {
		var line = scanner.Text()
		var operation = strings.Split(line, " ")
		var ranges = strings.Split(operation[1], ",")
		var xRange = strings.Split(ranges[0][2:], "..")
		var yRange = strings.Split(ranges[1][2:], "..")
		var zRange = strings.Split(ranges[2][2:], "..")

		steps = append(steps, cube{
			operation[0],
			atoi(xRange[0]), atoi(xRange[1]),
			atoi(yRange[0]), atoi(yRange[1]),
			atoi(zRange[0]), atoi(zRange[1]),
		})
	}

	return steps
}

func atoi(in string) int {
	r, _ := strconv.Atoi(in)
	return r
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
