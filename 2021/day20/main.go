package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func main() {
	var algorithm, image = readInput("input.txt")

	fmt.Println("Input")
	printImage(image)

	var intermediateImage = image
	var background = false
	for step := 0; step < 50; step++ {
		// Since algorithm[0] == '#', and algorithm[-1] == '.' every step will flip the background
		var backgroundColor rune
		if background {
			backgroundColor = '#'
		} else {
			backgroundColor = '.'
		}
		intermediateImage = applyEnhancementAlgorithm(intermediateImage, algorithm, backgroundColor)
		background = !background
	}

	printImage(intermediateImage)

	var pixelCount = 0
	for _, row := range intermediateImage {
		for _, c := range row {
			if c == '#' {
				pixelCount += 1
			}
		}
	}
	fmt.Println("Part 2", pixelCount)
}

func printImage(image [][]rune) {
	for _, row := range image {
		fmt.Println(string(row))
	}
}

func applyEnhancementAlgorithm(image [][]rune, algorithm string, background rune) [][]rune {
	var newHeight = len(image) + 2
	var newWidth = len(image[0]) + 2

	var newImage = make([][]rune, newHeight)
	for y := 0; y < newHeight; y++ {
		newImage[y] = make([]rune, newWidth)
		for x := 0; x < newWidth; x++ {
			// The previous image is offset 1,1 from the new image
			var index = calculateIndexFrom9x9Window(x-1, y-1, image, background)
			var newChar = rune(algorithm[index])
			newImage[y][x] = newChar
		}
	}
	return newImage
}

func calculateIndexFrom9x9Window(x int, y int, image [][]rune, background rune) int {
	var binary = ""
	for i := -1; i <= 1; i++ {
		for j := -1; j <= 1; j++ {
			var pixel = getPixel(x+j, y+i, image, background)
			if pixel == '#' {
				binary = binary + "1"
			} else {
				binary = binary + "0"
			}

		}
	}
	var result, _ = strconv.ParseInt(binary, 2, 16)
	return int(result)
}

func getPixel(x int, y int, image [][]rune, background rune) rune {
	var height = len(image)
	var width = len(image[0])
	if x >= 0 && x < width && y >= 0 && y < height {
		return image[y][x]
	} else {
		return background
	}
}

func readInput(path string) (string, [][]rune) {
	var file, err = os.Open(path)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	var fileScanner = bufio.NewScanner(file)

	fileScanner.Scan()
	var algorithm = fileScanner.Text()

	// Empty line
	fileScanner.Scan()

	var image [][]rune
	for fileScanner.Scan() {
		var imageRow = fileScanner.Text()
		var chars []rune
		for _, c := range imageRow {
			chars = append(chars, c)
		}
		image = append(image, chars)
	}

	return algorithm, image
}
