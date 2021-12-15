package main

import (
	"bufio"
	"container/heap"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
)

func main() {
	var cave = readInput("input.txt")

	var height = len(cave)
	var width = len(cave[0])

	cave, width, height = buildCave2(cave, width, height)

	var start = point{0, 0}
	var end = point{width - 1, height - 1}

	var dist = make2dArray(width, height, math.MaxInt64)
	dist[0][0] = 0

	var visited = make(map[point]bool)

	var queue = make(PriorityQueue, 0)
	heap.Init(&queue)
	var item = &Item{
		value:    start,
		priority: 0,
	}
	heap.Push(&queue, item)

	for queue.Len() > 0 {
		var u = heap.Pop(&queue).(*Item).value
		visited[u] = true

		if u == end {
			fmt.Println("Total risk", dist[u.y][u.x])
			return
		}

		var checkNeighbour = func(v point) {
			if !visited[v] {
				var newDist = dist[u.y][u.x] + cave[v.y][v.x]
				if newDist < dist[v.y][v.x] {
					dist[v.y][v.x] = newDist
					var item = &Item{
						value:    v,
						priority: newDist,
					}
					heap.Push(&queue, item)
				}
			}
		}

		// -1, 0
		if u.x > 0 {
			checkNeighbour(point{u.x - 1, u.y})
		}
		// +1, 0
		if u.x < width-1 {
			checkNeighbour(point{u.x + 1, u.y})
		}
		// 0, -1
		if u.y > 0 {
			checkNeighbour(point{u.x, u.y - 1})
		}
		// 0, +1
		if u.y < height-1 {
			checkNeighbour(point{u.x, u.y + 1})
		}
	}
}

func buildCave2(cave [][]int, width int, height int) ([][]int, int, int) {
	var cave2 = make2dArray(5*width, 5*height, 0)

	for i := 0; i < 5; i++ {
		for j := 0; j < 5; j++ {
			for y := 0; y < height; y++ {
				for x := 0; x < width; x++ {
					if i == 0 && j == 0 {
						cave2[y][x] = cave[y][x]
					} else if i == 0 {
						// Take values from left
						var nx = j*width + x
						var prevNx = (j-1)*width + x
						if cave2[y][prevNx] >= 9 {
							cave2[y][nx] = 1
						} else {
							cave2[y][nx] = cave2[y][prevNx] + 1
						}
					} else {
						// Take values from top
						var nx = j*width + x
						var ny = i*height + y
						var prevNy = (i-1)*height + y
						if cave2[prevNy][nx] >= 9 {
							cave2[ny][nx] = 1
						} else {
							cave2[ny][nx] = cave2[prevNy][nx] + 1
						}
					}
				}
			}
		}
	}
	return cave2, width * 5, height * 5
}

type point struct {
	x int
	y int
}

func make2dArray(width int, height int, initial int) [][]int {
	var rows = make([][]int, height)
	for i := 0; i < height; i++ {
		rows[i] = make([]int, width)
		for j := 0; j < width; j++ {
			rows[i][j] = initial
		}
	}
	return rows
}

func readInput(path string) [][]int {
	file, err := os.Open(path)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	var cave [][]int
	for scanner.Scan() {
		lineText := scanner.Text()

		var row []int
		for _, c := range lineText {
			row = append(row, atoi(c))
		}

		cave = append(cave, row)
	}

	return cave
}

func atoi(in rune) int {
	r, _ := strconv.Atoi(string(in))
	return r
}

// An Item is something we manage in a priority queue.
type Item struct {
	value    point // The value of the item; arbitrary.
	priority int   // The priority of the item in the queue.
	// The index is needed by update and is maintained by the heap.Interface methods.
	index int // The index of the item in the heap.
}

// A PriorityQueue implements heap.Interface and holds Items.
// Copied from https://pkg.go.dev/container/heap#example-package-PriorityQueue
type PriorityQueue []*Item

func (pq PriorityQueue) Len() int { return len(pq) }

func (pq PriorityQueue) Less(i, j int) bool {
	return pq[i].priority < pq[j].priority
}

func (pq PriorityQueue) Swap(i, j int) {
	pq[i], pq[j] = pq[j], pq[i]
	pq[i].index = i
	pq[j].index = j
}

func (pq *PriorityQueue) Push(x interface{}) {
	n := len(*pq)
	item := x.(*Item)
	item.index = n
	*pq = append(*pq, item)
}

func (pq *PriorityQueue) Pop() interface{} {
	old := *pq
	n := len(old)
	item := old[n-1]
	old[n-1] = nil  // avoid memory leak
	item.index = -1 // for safety
	*pq = old[0 : n-1]
	return item
}
