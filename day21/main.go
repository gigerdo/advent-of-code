package main

import (
	"fmt"
)

func main() {
	part1()

	var p1, p2 = playPart2(1, 0, 3, 0, true)
	fmt.Println("Part 2", max(p1, p2))
}

func playPart2(player1Pos int, player1Score int, player2Pos int, player2Score int, player1sTurn bool) (int, int) {
	if player1Score >= 21 {
		return 1, 0
	}
	if player2Score >= 21 {
		return 0, 1
	}

	var rolledNumbers = make(map[int]int)
	for i := 1; i <= 3; i++ {
		for j := 1; j <= 3; j++ {
			for k := 1; k <= 3; k++ {
				var total = i + j + k
				rolledNumbers[total] += 1
			}
		}
	}

	var player1Wins = 0
	var player2Wins = 0
	for k, v := range rolledNumbers {
		if player1sTurn {
			var newPos = move(player1Pos, k)
			var p1, p2 = playPart2(newPos, player1Score+newPos, player2Pos, player2Score, !player1sTurn)
			player1Wins += p1 * v
			player2Wins += p2 * v
		} else {
			var newPos = move(player2Pos, k)
			var p1, p2 = playPart2(player1Pos, player1Score, newPos, player2Score+newPos, !player1sTurn)
			player1Wins += p1 * v
			player2Wins += p2 * v
		}
	}

	return player1Wins, player2Wins
}

func part1() {
	var player1Pos = 1
	var player1Score = 0

	var player2Pos = 3
	var player2Score = 0

	for true {
		player1Pos = rollAndMove(player1Pos)
		player1Score += player1Pos
		if player1Score >= 1000 {
			break
		}

		player2Pos = rollAndMove(player2Pos)
		player2Score += player2Pos
		if player2Score >= 1000 {
			break
		}
	}

	fmt.Println("Part1", min(player1Score, player2Score)*rolls)
}

func rollAndMove(startPos int) int {
	var roll1 = rollDie()
	var roll2 = rollDie()
	var roll3 = rollDie()
	var totalMove = roll1 + roll2 + roll3

	return move(startPos, totalMove)
}

func move(startPos int, move int) int {
	return (startPos+move-1)%10 + 1
}

var rolls = 0

func rollDie() int {
	var rolledNumber = (rolls % 100) + 1
	rolls += 1
	return rolledNumber
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
