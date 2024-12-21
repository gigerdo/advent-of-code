package day21

import java.io.File
import kotlin.math.abs

fun main() {
    val codes = File("input.txt").readLines()

    val keyPad = mapOf(
        '7' to Point(0, 0), '8' to Point(0, 1), '9' to Point(0, 2),
        '4' to Point(1, 0), '5' to Point(1, 1), '6' to Point(1, 2),
        '1' to Point(2, 0), '2' to Point(2, 1), '3' to Point(2, 2),
        '0' to Point(3, 1), 'A' to Point(3, 2)
    )

    val directionalPad = mapOf(
        '^' to Point(0, 1), 'A' to Point(0, 2),
        '<' to Point(1, 0), 'v' to Point(1, 1), '>' to Point(1, 2),
    )

    var totalComplexity = 0
    for (code in codes) {
        var shortestPath = Int.MAX_VALUE
        var shortestPaths = listOf<List<Char>>()
        val s0 = shortestPath(keyPad, code.toList())
        for (s0Path in s0) {
            val s1 = shortestPath(directionalPad, s0Path)
            for (s1Path in s1) {
                val s2 = shortestPath(directionalPad, s1Path)
                val shortest = s2.minBy { it.size }
                if (shortestPath > shortest.size) {
                    shortestPaths = listOf(
                        shortest,
                        s1Path,
                        s0Path,
                        code.toList()
                    )
                    shortestPath = shortest.size
                }
            }
        }

        shortestPaths.map { it.joinToString("") }.forEach { println("${it.length}: $it") }
        val complexity = shortestPath * code.filter { it != 'A' }.toInt()
        totalComplexity += complexity
        println("Complexity: $complexity")
        println()
    }

    println("part1 = $totalComplexity")
}

fun shortestPath(keypad: Map<Char, Point>, code: List<Char>): List<List<Char>> {
    var paths = listOf<List<Char>>(listOf())
    var current = keypad['A']!!
    for (ch in code) {
        val next = keypad[ch]!!
        val (yd, xd) = (next.y - current.y to next.x - current.x)
        val upDown = if (yd < 0) {
            List(abs(yd)) { '^' }
        } else {
            List(abs(yd)) { 'v' }
        }
        val leftRight = if (xd < 0) {
            List(abs(xd)) { '<' }
        } else {
            List(abs(xd)) { '>' }
        }
        val perms = permutations(upDown + leftRight)
            .filter {
                var p = current
                for (c in it) {
                    p += when (c) {
                        '^' -> Point(-1, 0)
                        'v' -> Point(1, 0)
                        '<' -> Point(0, -1)
                        '>' -> Point(0, 1)
                        else -> throw IllegalArgumentException("Invalid direction: $c")
                    }
                    if (p !in keypad.values) {
                        return@filter false
                    }
                }
                true
            }
        paths = paths.flatMap { path -> perms.map { path + it + 'A' } }.distinct()
        current = next
    }
    return paths
}

fun <T> permutations(input: List<T>): List<List<T>> {
    if (input.isEmpty()) return listOf(emptyList())

    val result = mutableListOf<List<T>>()

    for (i in input.indices) {
        val current = input[i]
        val remaining = input.toMutableList().apply { removeAt(i) }

        permutations(remaining).forEach { permutation ->
            result.add(listOf(current) + permutation)
        }
    }

    return result
}

data class Point(val y: Int, val x: Int) {
    operator fun plus(other: Point) = Point(y + other.y, x + other.x)
}
