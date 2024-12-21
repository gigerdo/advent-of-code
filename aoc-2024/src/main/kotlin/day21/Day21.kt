package day21

import java.io.File
import kotlin.collections.filter
import kotlin.collections.map
import kotlin.math.abs
import kotlin.to

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

    val part1 = listOf(keyPad) + List(2) { directionalPad }
    println("part1 = ${calculateComplexity(codes, part1)}")

    val part2 = listOf(keyPad) + List(25) { directionalPad }
    println("part2 = ${calculateComplexity(codes, part2)}")
}

private fun calculateComplexity(
    codes: List<String>,
    pads: List<Map<Char, Point>>
): Long {
    val cache = mutableMapOf<Pair<Code, Int>, Long>()
    var totalComplexity = 0L

    for (string in codes) {
        val result = calculateShortestSequenceLength(string.toList(), 0, pads, cache)
        totalComplexity += result * string.filter { it != 'A' }.toInt()
    }

    return totalComplexity
}

fun calculateShortestSequenceLength(
    code: Code,
    padIdx: Int,
    pads: List<Map<Char, Point>>,
    cache: MutableMap<Pair<Code, Int>, Long>
): Long {
    var shortestSequenceLength = 0L
    var lastChar = 'A'
    for (ch in code) {
        val pad = pads[padIdx]
        val subCodes = shortestPath(pad, ch, pad[lastChar]!!)
        if (padIdx == pads.size - 1) {
            // On the last pad we can just take the length of the shortest path
            // (we don't care about the actual path taken)
            shortestSequenceLength += subCodes.minBy { it.size }.size
        } else {
            shortestSequenceLength += subCodes
                .map {
                    cache.getOrPut(Pair(it, padIdx + 1)) {
                        calculateShortestSequenceLength(it, padIdx + 1, pads, cache)
                    }
                }
                .min()
        }
        lastChar = ch
    }
    return shortestSequenceLength
}

fun shortestPath(keypad: Map<Char, Point>, ch: Char, start: Point): List<Code> {
    val end = keypad[ch]!!
    val (yd, xd) = (end.y - start.y to end.x - start.x)
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

    return listOf(
        // We don't need all permutations here:
        // Tt is always optimal to keep moving in the same direction as long as possible.
        // So either move up/down first or left/right first.
        upDown + leftRight + 'A',
        leftRight + upDown + 'A'
    )
        .filter<List<Char>> {
            // Reject paths that go out of bounds
            var p = start
            for (c in it) {
                p += when (c) {
                    '^' -> Point(-1, 0)
                    'v' -> Point(1, 0)
                    '<' -> Point(0, -1)
                    '>' -> Point(0, 1)
                    else -> Point(0, 0)
                }
                if (p !in keypad.values) {
                    return@filter false
                }
            }
            true
        }
}

data class Point(val y: Int, val x: Int) {
    operator fun plus(other: Point) = Point(y + other.y, x + other.x)
}

typealias Code = List<Char>