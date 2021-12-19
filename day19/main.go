package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
	"strings"
)

func main() {
	var scanners = readInput("input.txt")

	var absoluteScanners []scanner                           // scanners with absolute positions
	absoluteScanners = append(absoluteScanners, scanners[0]) // take first scanner as 0,0

	var scannersToProcess = scanners[1:]

outerLoop:
	for len(scannersToProcess) > 0 {
		fmt.Println("Scanners to process", len(scannersToProcess))
		var scannerToProcess = scannersToProcess[0]
		scannersToProcess = scannersToProcess[1:]
		for _, s := range absoluteScanners {
			var foundOverlap, scanner = checkAllPossiblePositions(scannerToProcess, s)
			if foundOverlap {
				absoluteScanners = append(absoluteScanners, scanner)
				continue outerLoop
			}
		}
		scannersToProcess = append(scannersToProcess, scannerToProcess)
	}

	var transformedPoints = make(map[point]bool)
	for _, s := range absoluteScanners {
		for _, p := range s.points {
			var transformedPoint = add(s.origin, rotate(p, s.orientation))
			transformedPoints[transformedPoint] = true
		}
	}

	fmt.Println("Part 1: Total points", len(transformedPoints))

	var distances []int
	for i := 0; i < len(absoluteScanners)-1; i++ {
		for j := i + 1; j < len(absoluteScanners); j++ {
			// Manhattan distance
			var v = sub(absoluteScanners[i].origin, absoluteScanners[j].origin)
			var dist = abs(v.x) + abs(v.y) + abs(v.z)
			distances = append(distances, dist)
		}
	}
	sort.Ints(distances)
	fmt.Println("Part 2: Largest distance", distances[len(distances)-1])
}

func abs(i int) int {
	if i < 0 {
		return -i
	} else {
		return i
	}
}

func listWithout(l []string, el string) []string {
	var newList []string
	for _, c := range l {
		if c != el {
			newList = append(newList, c)
		}
	}
	return newList
}

func checkAllPossiblePositions(scannerToProcess scanner, referenceScanner scanner) (bool, scanner) {
	var orientations []orientation
	var axis = []string{"x", "y", "z"}
	var directions = []int{-1, 1}
	for _, firstAxis := range axis {
		var remainingAxis = listWithout(axis, firstAxis)
		for _, secondAxis := range remainingAxis {
			var thirdAxis = listWithout(remainingAxis, secondAxis)[0]
			for _, firstAxisDir := range directions {
				for _, secondAxisDir := range directions {
					for _, thirdAxisDir := range directions {
						orientations = append(orientations, orientation{
							firstAxis,
							firstAxisDir,
							secondAxis,
							secondAxisDir,
							thirdAxis,
							thirdAxisDir,
						})
					}
				}
			}
		}
	}

	for _, o := range orientations {
		scannerToProcess.orientation = o

		var distances = getAllDistances(referenceScanner, scannerToProcess)

		for _, dist := range distances {
			scannerToProcess.origin = dist
			var overlaps = findOverlaps(referenceScanner, scannerToProcess)
			if overlaps {
				return true, scannerToProcess
			}
		}
	}

	return false, scanner{}
}

type line struct {
	a point
	b point
}

func getAllDistances(a scanner, b scanner) []point {
	var possibleOrigins []point

	for _, rowA := range a.distances {
		for _, rowB := range b.distances {
			var count = 0
			for idxA, eRowA := range rowA {
				for idxB, eRowB := range rowB {
					if eRowA == eRowB {
						count += 1
						if count >= 12 {
							var pointA = a.points[idxA]
							var pointB = b.points[idxB]
							var dist = sub(add(a.origin, rotate(pointA, a.orientation)), rotate(pointB, b.orientation))
							possibleOrigins = append(possibleOrigins, dist)
						}
					}
				}
			}
		}
	}

	return possibleOrigins
}

func findOverlaps(a scanner, b scanner) bool {
	var overlapCount = 0
	for _, pa := range a.points {
		for _, pb := range b.points {
			if add(a.origin, rotate(pa, a.orientation)) == add(b.origin, rotate(pb, b.orientation)) {
				overlapCount += 1
				if overlapCount >= 12 {
					return true
				}
			}
		}
	}
	return false
}

func rotate(p point, o orientation) point {
	return point{
		switchAxis(p, o.xAxis, o.xDir),
		switchAxis(p, o.yAxis, o.yDir),
		switchAxis(p, o.zAxis, o.zDir),
	}
}

func switchAxis(p point, axis string, dir int) int {
	var value int
	switch axis {
	case "x":
		value = p.x
	case "y":
		value = p.y
	case "z":
		value = p.z
	}
	if dir == -1 {
		return -value
	} else {
		return value
	}
}

func add(a point, b point) point {
	return point{a.x + b.x, a.y + b.y, a.z + b.z}
}

func sub(a point, b point) point {
	return point{a.x - b.x, a.y - b.y, a.z - b.z}
}

type orientation struct {
	xAxis string
	xDir  int
	yAxis string
	yDir  int
	zAxis string
	zDir  int
}

type scanner struct {
	name        string
	points      []point
	origin      point
	orientation orientation
	distances   [][]int
}

type point struct {
	x int
	y int
	z int
}

func readInput(path string) []scanner {
	var file, err = os.Open(path)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	var scanners []scanner

	var fileScanner = bufio.NewScanner(file)
	for fileScanner.Scan() {
		var name = fileScanner.Text()
		name = name[4 : len(name)-4]
		var currentPoints []point

		for fileScanner.Scan() {
			var pointText = fileScanner.Text()
			if pointText == "" {
				break
			}
			var axis = strings.Split(pointText, ",")
			currentPoints = append(currentPoints, point{
				x: atoi(axis[0]),
				y: atoi(axis[1]),
				z: atoi(axis[2]),
			})
		}

		scanners = append(scanners, scanner{
			name,
			currentPoints,
			point{0, 0, 0},
			orientation{"x", 1, "y", 1, "z", 1},
			calculateDistance(currentPoints),
		})
	}

	return scanners
}

func calculateDistance(pts []point) [][]int {
	var distances [][]int
	for _, p1 := range pts {
		var singleDist []int
		for _, p2 := range pts {
			singleDist = append(singleDist, dist(p1, p2))
		}
		distances = append(distances, singleDist)
	}
	return distances
}

func dist(p1 point, p2 point) int {
	var v = sub(p2, p1)
	return v.x*v.x + v.y*v.y + v.z*v.z
}

func atoi(in string) int {
	r, _ := strconv.Atoi(in)
	return r
}
