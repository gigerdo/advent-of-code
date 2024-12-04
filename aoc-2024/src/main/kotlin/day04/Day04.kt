package day04

import java.io.File

fun main() {
    val input = File("input.txt").readLines()
        .map { it.toList() }

    part1(input)
    part2(input)
}

private fun part1(input: List<List<Char>>) {
    var sum = 0
    for ((y, _) in input.withIndex()) {
        for ((x, _) in input[y].withIndex()) {
            val c = input[y][x]
            if (c == 'X') {
                val xmasFound = listOf(
                    Pair(-1, -1),
                    Pair(-1, 0),
                    Pair(-1, 1),
                    Pair(0, 1),
                    Pair(1, 1),
                    Pair(1, 0),
                    Pair(1, -1),
                    Pair(0, -1)
                ).sumOf {
                    checkXmas(input, y, x, it)
                }
                sum += xmasFound
            }
        }
    }
    println("part1 = $sum")
}

fun checkXmas(input: List<List<Char>>, startY: Int, startX: Int, direction: Pair<Int, Int>): Int {
    var y = startY
    var x = startX
    for (ch in listOf('X', 'M', 'A', 'S')) {
        if (y < 0 || y >= input.size || x < 0 || x >= input[y].size) {
            return 0
        }
        if (input[y][x] != ch) {
            return 0
        }
        y += direction.first
        x += direction.second
    }
    return 1
}

private fun part2(input: List<List<Char>>) {
    var sum = 0
    for ((y, _) in input.withIndex()) {
        if (y == 0 || y == input.size - 1) {
            continue
        }
        for ((x, _) in input[y].withIndex()) {
            if (x == 0 || x == input[y].size - 1) {
                continue
            }
            val c = input[y][x]
            if (c == 'A') {
                val topLeft = input[y - 1][x - 1]
                val bottomLeft = input[y + 1][x - 1]
                val topRight = input[y - 1][x + 1]
                val bottomRight = input[y + 1][x + 1]

                if ((topLeft == 'M' && bottomRight == 'S'
                        || topLeft == 'S' && bottomRight == 'M')
                    && (topRight == 'M' && bottomLeft == 'S'
                        || topRight == 'S' && bottomLeft == 'M')
                ) {
                    sum += 1
                }
            }
        }
    }
    println("part2 = $sum")
}



