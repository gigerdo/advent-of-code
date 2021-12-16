package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
)

func main() {
	var bits = readInput("input.txt")

	var _, result = parse(bits, 0, 0)

	fmt.Println("\n\nResult", result)
}

func parse(bits []uint8, start int, depth int) (int, int) {
	// var version = binToDec(bits[start : start+3]) // version is unused?
	var typeId = binToDec(bits[start+3 : start+6])

	var printIt = func(p string, v int) {
		for i := 0; i < depth; i++ {
			fmt.Print("  ")
		}
		// fmt.Println(typeId, "|", p, v)
		fmt.Println(p, v)
	}

	var end int

	if typeId == 4 {
		var literalValue []uint8
		for i := start + 6; true; i += 5 {
			var t = bits[i : i+5]
			literalValue = append(literalValue, t[1:]...)
			if t[0] == 0 {
				end = i + 5
				break
			}
		}
		printIt("Literal", binToDec(literalValue))
		return end, binToDec(literalValue)
	} else {
		var subPacketNumbers []int
		var lengthTypeId = bits[start+6]
		if lengthTypeId == 0 {
			// next 15bits is total bitlength of this packet
			var totalLengthBits = binToDec(bits[start+7 : start+22])
			var limit = start + 22 + totalLengthBits
			end = start + 22
			for end != limit {
				var result int
				end, result = parse(bits, end, depth+1)
				subPacketNumbers = append(subPacketNumbers, result)
			}
		} else {
			// next 11 bits is number of subpackets
			var totalSubPackets = binToDec(bits[start+7 : start+18])

			end = start + 18
			for i := 0; i < totalSubPackets; i++ {
				var result int
				end, result = parse(bits, end, depth+1)
				subPacketNumbers = append(subPacketNumbers, result)
			}
		}

		switch typeId {
		case 0:
			// Sum
			var sum = 0
			for _, n := range subPacketNumbers {
				sum += n
			}
			printIt("Sum", sum)
			return end, sum
		case 1:
			// Product
			var product = 1
			for _, n := range subPacketNumbers {
				product *= n
			}
			printIt("Product", product)
			return end, product
		case 2:
			// Minimum
			var min = math.MaxInt64
			for _, n := range subPacketNumbers {
				if n < min {
					min = n
				}
			}
			printIt("Min", min)
			return end, min
		case 3:
			// Maximum
			var max = math.MinInt64
			for _, n := range subPacketNumbers {
				if n > max {
					max = n
				}
			}
			printIt("Max", max)
			return end, max
		case 5:
			// Greater than
			if subPacketNumbers[0] > subPacketNumbers[1] {
				printIt("Greater than", 1)
				return end, 1
			} else {
				printIt("Greater than", 0)
				return end, 0
			}
		case 6:
			// Less Than
			if subPacketNumbers[0] < subPacketNumbers[1] {
				printIt("Less than", 1)
				return end, 1
			} else {
				printIt("Less than", 0)
				return end, 0
			}
		case 7:
			// Equal to
			if subPacketNumbers[0] == subPacketNumbers[1] {
				printIt("Equal", 1)
				return end, 1
			} else {
				printIt("Equal", 0)
				return end, 0
			}
		}
	}
	log.Fatal("Unreachable state")
	return -1, -1
}

func binToDec(bin []uint8) int {
	var dec = 0
	var power = 1
	for i := len(bin) - 1; i >= 0; i -= 1 {
		if bin[i] == 1 {
			dec += power
		}
		power *= 2
	}
	return dec
}

func readInput(path string) []uint8 {
	var file, err = os.Open(path)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	var scanner = bufio.NewScanner(file)

	scanner.Scan()
	var lineText = scanner.Text()
	var bits []uint8

	for _, c := range lineText {
		var val, _ = strconv.ParseUint(string(c), 16, 8)

		var charBits []uint8
		for i := 0; i < 4; i++ {
			charBits = append([]uint8{uint8(val & 0x1)}, charBits...)
			// or
			// bits = append(bits, val & 0x1)
			// depending on the order you want
			val = val >> 1
		}
		bits = append(bits, charBits...)
	}

	return bits
}
