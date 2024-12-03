package dev.giger.day03

import java.io.File

fun main() {
    val input = File("input.txt").readText()

    part1(input)
    part2(input)
}

private fun part1(input: String) {
    val result = Regex("mul\\(([0-9]+),([0-9]+)\\)")
        .findAll(input)
        .map { it.destructured.toList().fold(1L) { acc, s -> acc * s.toLong() } }
        .sum()
    println("part1 = $result")
}

private fun part2(input: String) {
    val result = Regex("(mul|don't|do)\\(([0-9]*),?([0-9]*)\\)")
        .findAll(input)
        .map { it.destructured }

    var sum = 0L
    var mulEnabled = true
    for ((operator, a, b) in result) {
        when (operator) {
            "mul" -> if (mulEnabled) sum += a.toLong() * b.toLong()
            "don't" -> mulEnabled = false
            "do" -> mulEnabled = true
        }
    }
    println("part2 = $sum")
}


