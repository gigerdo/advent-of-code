package main

import (
	"fmt"
)

var xmin = 241
var xmax = 275
var ymin = -75
var ymax = -49

func main() {
	// Sample
	// xmin = 20
	// xmax = 30
	// ymin = -10
	// ymax = -5
	// target area: x=241..275, y=-75..-49

	var count = 0
	for yvel := -1000; yvel < 1000; yvel++ {
		for xvel := 1; xvel < 1000; xvel++ {
			var hit, maxy = simulate(xvel, yvel)
			if hit {
				fmt.Println("Result", xvel, yvel, maxy)
				count += 1
			}
		}
	}

	fmt.Println("Count", count)
}

func simulate(xvel int, yvel int) (bool, int) {
	var x = 0
	var y = 0
	var maxy = 0
	for x < xmax && y > ymin {
		x += xvel
		y += yvel
		if xvel > 0 {
			xvel -= 1
		} else if xvel < 0 {
			xvel += 1
		}
		yvel -= 1

		if y > maxy {
			maxy = y
		}
		if x >= xmin && x <= xmax && y >= ymin && y <= ymax {
			return true, maxy
		}
	}

	return false, maxy
}
