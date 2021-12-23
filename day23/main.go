package main

import (
	"container/heap"
	"fmt"
)

func main() {
	// // Sample
	// var input = burrow2{
	// 	rooms: map[string][]string{
	// 		"A": {"B", "D", "D", "A"},
	// 		"B": {"C", "C", "B", "D"},
	// 		"C": {"B", "B", "A", "C"},
	// 		"D": {"D", "A", "C", "A"},
	// 	},
	// 	hallway: make([]string, 11),
	// }

	// Actual input
	var input = burrowstate{
		rooms: map[string][]string{
			"A": {"C", "D", "D", "B"},
			"B": {"B", "C", "B", "D"},
			"C": {"D", "B", "A", "A"},
			"D": {"A", "A", "C", "C"},
		},
		hallway: make([]string, 11),
	}

	var queue = make(PriorityQueue, 0)
	heap.Init(&queue)
	var item = &Item{
		value:    input,
		priority: 0,
	}
	heap.Push(&queue, item)

	var visited = make(map[string]bool)

	var addToQueue = func(b burrowstate, c int) {
		var item = &Item{
			value:    b,
			priority: c,
		}
		heap.Push(&queue, item)
	}

	for queue.Len() > 0 {
		var item = heap.Pop(&queue).(*Item)
		var burrow = item.value
		var cost = item.priority

		var burrowAsString = toString(burrow)
		var _, alreadyVisited = visited[burrowAsString]
		if alreadyVisited {
			continue
		} else {
			visited[burrowAsString] = true
		}

		if isSolution(burrow) {
			fmt.Println("Part 2", cost)
			return
		}

		var hallways = []int{0, 1, 3, 5, 7, 9, 10}
		var amphipods = []string{"A", "B", "C", "D"}
		for _, hallway := range hallways {
			for _, amphipod := range amphipods {
				var room = amphipod
				if canMove(burrow, hallway, amphipod) {
					// Hallway to room
					if burrow.hallway[hallway] == amphipod {
						for i := 3; i >= 0; i-- {
							if burrow.rooms[room][i] == "" {
								addToQueue(moveToRoom(burrow, hallway, room, i), cost+moveCost(hallway, room, i, amphipod))
								break
							} else if burrow.rooms[amphipod][i] != amphipod {
								// Room in the back contains wrong amphipod, don't try to fill rooms further up
								break
							}
						}
					}

					// Room to hallway
					if burrow.hallway[hallway] == "" {
						var hasWrongAmphipod = false
						for i := 0; i < 4; i++ {
							if burrow.rooms[room][i] != "" && burrow.rooms[room][i] != amphipod {
								hasWrongAmphipod = true
								break
							}
						}
						if hasWrongAmphipod {
							// Move out the first amphipod
							for i := 0; i < 4; i++ {
								if burrow.rooms[room][i] != "" {
									// Amphipod in the wrong room, move away
									var amphipodBeingMoved = burrow.rooms[room][i]
									addToQueue(moveToHallway(burrow, room, i, hallway), cost+moveCost(hallway, room, i, amphipodBeingMoved))
									break
								}
							}
						}

					}

				}
			}
		}

	}
}

type burrowstate struct {
	rooms   map[string][]string
	hallway []string
}

func toString(burrow burrowstate) string {
	// Create deterministic string from burrow as set key
	var str = ""
	for _, c := range burrow.rooms["A"] {
		str += fmt.Sprintf("%1s", c)
	}
	for _, c := range burrow.rooms["B"] {
		str += fmt.Sprintf("%1s", c)
	}
	for _, c := range burrow.rooms["C"] {
		str += fmt.Sprintf("%1s", c)
	}
	for _, c := range burrow.rooms["D"] {
		str += fmt.Sprintf("%1s", c)
	}
	for _, c := range burrow.hallway {
		str += fmt.Sprintf("%1s", c)
	}
	return str
}

func isSolution(burrow burrowstate) bool {
	for k, v := range burrow.rooms {
		for _, r := range v {
			if k != r {
				return false
			}
		}
	}
	return true
}

func canMove(burrow burrowstate, hallway int, room string) bool {
	var path = getPath(hallway, room)
	for _, p := range path {
		if burrow.hallway[p] != "" {
			return false
		}
	}
	return true
}

func moveToRoom(burrow burrowstate, hallway int, room string, depth int) burrowstate {
	var newBurrow = burrowstate{
		rooms:   map[string][]string{},
		hallway: make([]string, len(burrow.hallway)),
	}
	for k, v := range burrow.rooms {
		var c = make([]string, len(v))
		copy(c, v)
		newBurrow.rooms[k] = c
	}
	copy(newBurrow.hallway, burrow.hallway)
	newBurrow.rooms[room][depth] = newBurrow.hallway[hallway]
	newBurrow.hallway[hallway] = ""
	return newBurrow
}

func moveToHallway(burrow burrowstate, room string, depth int, hallway int) burrowstate {
	var newBurrow = burrowstate{
		rooms:   map[string][]string{},
		hallway: make([]string, len(burrow.hallway)),
	}
	for k, v := range burrow.rooms {
		var c = make([]string, len(v))
		copy(c, v)
		newBurrow.rooms[k] = c
	}
	copy(newBurrow.hallway, burrow.hallway)
	newBurrow.hallway[hallway] = newBurrow.rooms[room][depth]
	newBurrow.rooms[room][depth] = ""
	return newBurrow
}

func moveCost(hallway int, room string, depth int, amphipod string) int {
	var costMap = map[string]int{
		"A": 1,
		"B": 10,
		"C": 100,
		"D": 1000,
	}

	var path = getPath(hallway, room)
	return (len(path) + 2 + depth) * costMap[amphipod]
}

func getPath(hallway int, room string) []int {
	var roomPosition = map[string]int{
		"A": 2,
		"B": 4,
		"C": 6,
		"D": 8,
	}

	var roomPos = roomPosition[room]
	var path []int
	for i := min(hallway, roomPos) + 1; i < max(hallway, roomPos); i++ {
		path = append(path, i)
	}
	return path
}

func min(a int, b int) int {
	if a < b {
		return a
	} else {
		return b
	}
}

func max(a int, b int) int {
	if a > b {
		return a
	} else {
		return b
	}
}

// An Item is something we manage in a priority queue.
type Item struct {
	value    burrowstate // The value of the item; arbitrary.
	priority int         // The priority of the item in the queue.
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
