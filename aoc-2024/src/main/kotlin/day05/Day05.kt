package day05

import java.io.File

fun main() {
    val input = File("input.txt").readText()

    val (rules, updates) = input.split("\n\n")
        .let { (rules, pages) ->
            Pair(
                rules.split("\n")
                    .map { it.split("|").zipWithNext().single() },
                pages.split("\n")
                    .map { it.split(",") }
            )
        }

    var sumPart1 = 0
    var sumPart2 = 0
    for (update in updates) {
        val sortedPages = update.sortedWith(Comparator<String> { a, b ->
            val rule = rules.find { (l, r) -> l == a && r == b || l == b && r == a }!!
            if (a == rule.first && b == rule.second) -1 else 1
        })
        val middle = sortedPages[(update.size - 1) / 2].toInt()
        if (sortedPages == update) {
            sumPart1 += middle
        } else {
            sumPart2 += middle
        }
    }
    println("part1 = $sumPart1")
    println("part2 = $sumPart2")
}

