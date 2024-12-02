package dev.giger.day02

import java.io.File
import kotlin.collections.zipWithNext

fun main() {
    val input = File("input.txt")
        .readLines()
        .map { line -> line.split(Regex("\\s+")).map { it.toInt() } }

    part1(input)
    part2(input)
}

private fun part1(input: List<List<Int>>) {
    val result = input
        .filter { isSafe(it) }
        .count()
    println("part1 = $result")
}

private fun part2(input: List<List<Int>>) {
    val result = input.count { ints ->
        if (isSafe(ints)) {
            true
        } else {
            (0..ints.size - 1).map {
                val copy = ints.toMutableList()
                copy.removeAt(it)
                if (isSafe(copy)) {
                    return@count true
                }
            }
            false
        }
    }
    println("part2 = $result")
}

private fun isSafe(ints: List<Int>): Boolean {
    val diffs = ints.zipWithNext()
        .map { pair -> pair.second - pair.first }
        .map { diff -> Pair(diff > 0 && diff <= 3, diff < 0 && diff >= -3) }
    return diffs.all<Pair<Boolean, Boolean>> { it.first }
            || diffs.all<Pair<Boolean, Boolean>> { it.second }
}


