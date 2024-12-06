package day06

import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.async
import kotlinx.coroutines.awaitAll
import kotlinx.coroutines.runBlocking
import java.io.File
import kotlin.time.measureTime

fun main() {
    val input = File("input.txt").readLines().map { it.toList() }

    val start = input.withIndex()
        .find { (_, row) -> row.contains('^') }!!
        .let { (y, row) -> Point(y, row.indexOf('^')) }

    val timeTaken = measureTime {
        var (visitedPositions, _) = simulateGuard(start, input, null)

        val distinctPositions = visitedPositions.map { it.first }.distinct()
        println("part1 = ${distinctPositions.size}")

        val result = runBlocking(Dispatchers.Default) {
            distinctPositions
                .map {
                    async {
                        val (_, loop) = simulateGuard(start, input, it)
                        loop
                    }
                }
                .awaitAll()
                .filter { it }
                .count()
        }
        println("part2 = $result")
    }
    println(timeTaken)

}

private fun simulateGuard(
    start: Point,
    originalInput: List<List<Char>>,
    blockedPosition: Point?
): Pair<Set<Pair<Point, Char>>, Boolean> {
    var input = originalInput.map { it.toMutableList() }

    if (blockedPosition != null) {
        input[blockedPosition.y][blockedPosition.x] = '#'
    }

    var currentPos = start
    var currentDir = '^'
    var visitedPositions = mutableSetOf<Pair<Point, Char>>()
    outer@ while (true) {
        visitedPositions += Pair(currentPos, currentDir)

        while (true) {
            var nextPos = when (currentDir) {
                '^' -> Point(currentPos.y - 1, currentPos.x)
                'v' -> Point(currentPos.y + 1, currentPos.x)
                '<' -> Point(currentPos.y, currentPos.x - 1)
                '>' -> Point(currentPos.y, currentPos.x + 1)
                else -> throw IllegalArgumentException("Invalid character")
            }

            if (!(inBounds(nextPos, input))) {
                break@outer
            }

            when (input[nextPos.y][nextPos.x]) {
                '#' -> when (currentDir) {
                    '^' -> currentDir = '>'
                    'v' -> currentDir = '<'
                    '<' -> currentDir = '^'
                    '>' -> currentDir = 'v'
                    else -> throw IllegalArgumentException("Invalid character")
                }

                else -> {
                    currentPos = nextPos
                    break
                }
            }
        }

        if (visitedPositions.contains(Pair(currentPos, currentDir))) {
            // Debug print map
            if (false) {
                println("Found loop $blockedPosition")
                for ((y, row) in input.withIndex()) {
                    for ((x, ch) in row.withIndex()) {
                        val visited = visitedPositions.find { it.first == Point(y, x) }
                        if (visited != null) {
                            print(visited.second)
                        } else if (blockedPosition == Point(y, x)) {
                            print('O')
                        } else {
                            print(ch)
                        }
                    }
                    println()
                }
            }
            return Pair(visitedPositions, true)
        }
    }
    return Pair(visitedPositions, false)
}

data class Point(val y: Int, val x: Int)

fun inBounds(
    pos: Point,
    input: List<List<Char>>
): Boolean {
    return pos.y >= 0 && pos.y < input.size && pos.x >= 0 && pos.x < input[0].size
}
