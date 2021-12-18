package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func main() {
	var pairs = readInput("input.txt")

	var sum = &pairs[0]
	for _, n := range pairs[1:] {
		sum = add(sum, &n)
	}

	printPair(sum)
	fmt.Println("Part 1: Magnitude", magnitude(sum))

	var max = 0
	for i := 0; i < len(pairs); i++ {
		for j := 0; j < len(pairs); j++ {
			if i == j {
				continue
			}
			// Get fresh copy of the pairs since I am mutating them
			var sum = add(&pairs[i], &pairs[j])
			var magnitude = magnitude(sum)
			if magnitude > max {
				max = magnitude
			}
		}
	}

	fmt.Println("Part 2: Max magnitude", max)
}

func printPair(p *pair) {
	var internalPrint func(p *pair)
	internalPrint = func(p *pair) {
		if p.left != nil {
			fmt.Print("[")
			internalPrint(p.left)
			fmt.Print(",")
			internalPrint(p.right)
			fmt.Print("]")
		} else {
			fmt.Print(p.value)
		}
	}
	internalPrint(p)
	fmt.Print("\n")
}

type pair struct {
	left  *pair
	right *pair

	value int
}

func add(a *pair, b *pair) *pair {
	var sum = pair{deepCopy(a), deepCopy(b), -1}
	reduce(&sum)
	return &sum
}

func deepCopy(p *pair) *pair {
	if p.left != nil {
		return &pair{deepCopy(p.left), deepCopy(p.right), -1}
	} else {
		return &pair{nil, nil, p.value}
	}
}

func reduce(p *pair) {
	// Mutates the pair in-place
	var finished = false
	for !finished {
		var t = traverse(p, 0, traversal{nil, nil, nil, nil})
		if t.explode != nil {
			if t.left != nil {
				t.left.value += t.explode.left.value
			}
			if t.right != nil {
				t.right.value += t.explode.right.value
			}
			t.explode.value = 0
			t.explode.left = nil
			t.explode.right = nil

		} else if t.split != nil {
			// Split
			t.split.left = &pair{nil, nil, t.split.value / 2.0}
			t.split.right = &pair{nil, nil, (t.split.value + 1) / 2.0}
			t.split.value = -1
		} else {
			finished = true
		}
	}
}

type traversal struct {
	left    *pair
	right   *pair
	explode *pair
	split   *pair
}

func traverse(currentPair *pair, level int, t traversal) traversal {
	var isNumber = currentPair.value != -1
	if isNumber {
		if currentPair.value >= 10 && t.split == nil {
			t.split = currentPair
		}
		if t.explode == nil {
			t.left = currentPair
		} else if t.right == nil {
			t.right = currentPair
		}
		return t
	} else if t.explode == nil &&
		currentPair.left.value != -1 &&
		currentPair.right.value != -1 &&
		level == 4 {
		t.explode = currentPair
		return t
	} else {
		var leftResult = traverse(currentPair.left, level+1, t)
		return traverse(currentPair.right, level+1, leftResult)
	}
}

func magnitude(p *pair) int {
	if p.left != nil {
		return 3*magnitude(p.left) + 2*magnitude(p.right)
	} else {
		return p.value
	}
}

func readInput(path string) []pair {
	var file, err = os.Open(path)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	var numbers []pair

	var scanner = bufio.NewScanner(file)
	for scanner.Scan() {
		lineText := scanner.Text()
		var number = parseNumber(lineText)
		numbers = append(numbers, number)
	}

	return numbers
}

func parseNumber(line string) pair {
	var stack []pair
	for _, c := range line {
		switch c {
		case '[':
			var newPair = pair{nil, nil, -1}
			stack = append(stack, newPair)
		case ']':
			// Pair finished
			if len(stack) == 1 {
				return stack[0]
			}
			var p = stack[len(stack)-1]
			stack = stack[0 : len(stack)-1]
			var parent = stack[len(stack)-1]
			if parent.left == nil {
				parent.left = &p
			} else {
				parent.right = &p
			}
			stack[len(stack)-1] = parent
		case ',':
			// Ignored
		default:
			// decimal number
			var v = atoi(c)
			var parent = stack[len(stack)-1]
			if parent.left == nil {
				parent.left = &pair{nil, nil, v}
			} else {
				parent.right = &pair{nil, nil, v}
			}
			stack[len(stack)-1] = parent
		}
	}
	log.Fatal("Wrong input")
	return pair{nil, nil, -1}
}

func atoi(in rune) int {
	r, _ := strconv.Atoi(string(in))
	return r
}
