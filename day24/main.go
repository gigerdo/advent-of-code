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
	// Analysis of the code showed that only 7 digits need to be found, the other 7 are derived from these 7
	var vars = []int{0, 1, 2, 3, 5, 6, 8}

	var input []int
	var z int
	for i := 1111111; i < 9999999; i++ {
		input = make([]int, 14)
		for idx, c := range strconv.Itoa(i) {
			var v, _ = strconv.Atoi(string(c))
			input[vars[idx]] = v
		}
		z = runMonad(input)
		if z == 0 {
			fmt.Println("Found solution")
			for _, i := range input {
				fmt.Print(i)
			}
			fmt.Print("\n")
			break
		}
	}

	// Doublecheck against real program
	var instructions = readInput("input.txt")
	var registers = runProgram(instructions, input)

	fmt.Println("Calculating", input, registers["z"], z)
}

func runMonad(w []int) int {
	var z = 0

	// The 14 blocks of the MONAD code can be reduced to this
	z = 26*z + 9 + w[0]

	z = 26*z + 1 + w[1]

	z = 26*z + 11 + w[2]

	z = 26*z + 3 + w[3]

	// Some blocks don't use the input, but the input needs to be a certain value so z is divided by 26 (the only way to reduce z)
	// Derive w[4] based on current z
	w[4] = (z % 26) - 11
	z = z / 26

	z = 26*z + 5 + w[5]

	z = 26*z + 0 + w[6]

	w[7] = (z % 26) - 6
	z = z / 26

	z = 26*z + 9 + w[8]

	w[9] = (z % 26) - 6
	z = z / 26

	w[10] = (z % 26) - 6
	z = z / 26

	w[11] = (z % 26) - 16
	z = z / 26

	w[12] = (z % 26) - 4
	z = z / 26

	w[13] = (z % 26) - 2
	z = z / 26

	for _, i := range w {
		if i <= 0 || i > 9 {
			return -1
		}
	}

	return z
}

func runProgram(instructions []instruction, input []int) map[string]int {
	var registers = make(map[string]int)
	for _, i := range instructions {
		switch i.name {
		case "inp":
			registers[i.a] = input[0]
			input = input[1:]
		case "add":
			var b, err = strconv.Atoi(i.b)
			if err == nil {
				registers[i.a] += b
			} else {
				registers[i.a] += registers[i.b]
			}
		case "mul":
			var b, err = strconv.Atoi(i.b)
			if err == nil {
				registers[i.a] *= b
			} else {
				registers[i.a] *= registers[i.b]
			}
		case "div":
			var b, err = strconv.Atoi(i.b)
			if err == nil {
				registers[i.a] /= b
			} else {
				registers[i.a] /= registers[i.b]
			}
		case "mod":
			var b, err = strconv.Atoi(i.b)
			if err == nil {
				registers[i.a] %= b
			} else {
				registers[i.a] %= registers[i.b]
			}
		case "eql":
			var b, err = strconv.Atoi(i.b)
			var equal bool
			if err == nil {
				equal = registers[i.a] == b
			} else {
				equal = registers[i.a] == registers[i.b]
			}
			if equal {
				registers[i.a] = 1
			} else {
				registers[i.a] = 0
			}
		}
	}
	return registers
}

type instruction struct {
	name string
	a    string
	b    string
}

func readInput(path string) []instruction {
	var file, err = os.Open(path)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	var scanner = bufio.NewScanner(file)

	var instructions []instruction

	for scanner.Scan() {
		var line = scanner.Text()
		var parts = strings.Fields(line)

		var i instruction

		if len(parts) == 3 {
			i = instruction{
				parts[0],
				parts[1],
				parts[2],
			}
		} else {
			i = instruction{
				parts[0],
				parts[1],
				"",
			}
		}

		instructions = append(instructions, i)
	}

	return instructions
}
