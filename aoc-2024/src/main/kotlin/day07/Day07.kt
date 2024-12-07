package day07

import java.io.File

fun main() {
    val input = File("input.txt").readLines()
        .map {
            it.split(": ")
                .zipWithNext()
                .single()
                .let { (a, b) ->
                    Pair(
                        a.toLong(),
                        b.split(" ").map { it.toLong() }
                    )
                }
        }

    part1(input)
    part2(input)
}

private fun part1(input: List<Pair<Long, List<Long>>>) {
    var sum = 0L
    for (equation in input) {
        if (calculateEquation(equation.first, equation.second, 0, listOf(Long::plus, Long::times))) {
            sum += equation.first
        }
    }

    println("part1 = $sum")
}

fun calculateEquation(
    expected: Long,
    remaining: List<Long>,
    current: Long,
    operators: List<(Long, Long) -> Long>
): Boolean {
    if (current > expected) {
        return false
    }
    if (current == expected && remaining.isEmpty()) {
        return true
    }
    if (remaining.isEmpty()) {
        return false
    }
    val value = remaining.first()
    for (function in operators) {
        if (calculateEquation(expected, remaining.drop(1), function(current, value), operators)) {
            return true
        }
    }
    return false
}


private fun part2(input: List<Pair<Long, List<Long>>>) {
    var sum = 0L
    for (equation in input) {
        if (calculateEquation(
                equation.first,
                equation.second,
                0,
                listOf(Long::plus, Long::times, { a, b -> (a.toString() + b.toString()).toLong() })
            )
        ) {
            sum += equation.first
        }
    }
    println("part2 = $sum")
}
