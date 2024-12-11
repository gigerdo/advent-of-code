package day11

import java.io.File
import kotlin.time.measureTime

fun main() {
    val input = File("input.txt")
        .readText()
        .split(" ")
        .map { it.toLong() }

    println(measureTime { part1(input) })
    println(measureTime { part2(input) })
}

private fun part1(input: List<Long>) {
    var state = input
    for (i in 0 until 25) {
        state = state.flatMap { v ->
            if (v == 0L) {
                listOf(1L)
            } else {
                val str = v.toString()
                if (str.length % 2 == 0) {
                    listOf(
                        str.substring(0, str.length / 2).toLong(),
                        str.substring(str.length / 2).toLong()
                    )
                } else {
                    listOf(v * 2024)
                }
            }
        }
    }

    println("part1 = ${state.size}")
}

private fun part2(input: List<Long>) {
    val cache = mutableMapOf<Pair<Long, Int>, Long>()
    var result = input.map { calcSplits(it, 0, 75, cache) }.sum()
    println("part2 = $result")
}

private fun calcSplits(
    value: Long,
    step: Int,
    maxSteps: Int,
    cache: MutableMap<Pair<Long, Int>, Long>,
): Long {
    var splits = 1L
    var v = value
    for (i in step until maxSteps) {
        if (v == 0L) {
            v = 1L
        } else {
            val str = v.toString()
            if (str.length % 2 == 0) {
                v = str.substring(0, str.length / 2).toLong()
                val split = str.substring(str.length / 2).toLong()
                splits += cache.getOrPut(Pair(split, i + 1)) {
                    calcSplits(split, i + 1, maxSteps, cache)
                }
            } else {
                v = v * 2024
            }
        }
    }
    return splits
}
