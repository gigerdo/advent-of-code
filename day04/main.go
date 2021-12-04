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
	numbers, boards := readInput("input.txt")

	finishedBoards := make([]bool, len(boards))

	for _, number := range numbers {
		for boardIndex, board := range boards {
			if finishedBoards[boardIndex] {
				continue
			}
			for i := 0; i < 5; i++ {
				for j := 0; j < 5; j++ {
					boardNumber := board[i][j]
					if boardNumber == number {
						board[i][j] = -1

						// Check for win
						unmarkedFoundVertical := false
						unmarkedFoundHorizonal := false
						for k := 0; k < 5; k++ {
							if board[k][j] != -1 {
								unmarkedFoundVertical = true
							}
							if board[i][k] != -1 {
								unmarkedFoundHorizonal = true
							}
						}
						if !unmarkedFoundHorizonal || !unmarkedFoundVertical {
							// Winning board found
							fmt.Println("\nBoard wins:", boardIndex)
							fmt.Println("Last number", number)
							fmt.Println(board)

							sum := 0
							for m := 0; m < 5; m++ {
								for n := 0; n < 5; n++ {
									val := board[m][n]
									if val != -1 {
										sum += val
									}
								}
							}

							fmt.Println("Final score", sum*number)
							finishedBoards[boardIndex] = true
						}
					}
				}
			}
		}
	}

}

type board = [][]int

func readInput(path string) ([]int, []board) {
	file, err := os.Open(path)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	scanner.Scan()
	numbers := strings.Split(scanner.Text(), ",")

	scanner.Scan() // Discard empty newline in input

	// Scan all the boards
	var boards []board
	var currentBoard board
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			boards = append(boards, currentBoard)
			currentBoard = nil
		} else {
			boardLine := strings.Fields(line)
			currentBoard = append(currentBoard, stringsToInt(boardLine))
		}
	}
	boards = append(boards, currentBoard)

	return stringsToInt(numbers), boards
}

func stringsToInt(input []string) []int {
	var transformed []int
	for _, number := range input {
		parsedNumber, _ := strconv.Atoi(number)
		transformed = append(transformed, parsedNumber)
	}
	return transformed
}
