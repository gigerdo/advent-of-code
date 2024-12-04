package day01

import java.io.File
import kotlin.math.abs

fun main() {
    val input = File("src/main/kotlin/day01/input.txt")
        .readLines()
        .map { line -> line.split(Regex("\\s+")).map { it.toInt() }.zipWithNext().single() }
        .unzip()

    part1(input)
    part2(input)
}

private fun part1(input: Pair<List<Int>, List<Int>>) {
    val sorted = input.first.sorted().zip(input.second.sorted())
    val result = sorted.sumOf { abs(it.first - it.second) }
    println("part1 = $result (Expected: 3246517)")
}

private fun part2(input: Pair<List<Int>, List<Int>>) {
    val rightListCount = input.second
        .groupBy { it }
        .mapValues { it.value.size }
    val result = input.first.sumOf { it * rightListCount.getOrDefault(it, 0) }
    println("part2 = $result (Expected: 29379307)")
}
