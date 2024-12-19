package day19

import java.io.File

fun main() {
    val (patterns, designs) = File("input.txt")
        .readLines()
        .let { lines ->
            Pair(
                lines[0].split(", "),
                lines.drop(2)
            )
        }


    var part1FeasibleDesigns = 0
    var part2FeasibleCombinations = 0L
    val cache = mutableMapOf<String, Long>()
    for (design in designs) {
        val feasibleCombinations = isFeasible(design, patterns, cache)
        if (feasibleCombinations > 0) {
            part1FeasibleDesigns += 1
        }
        part2FeasibleCombinations += feasibleCombinations
    }
    println("part1 = $part1FeasibleDesigns")
    println("part2 = $part2FeasibleCombinations")
}

fun isFeasible(design: String, patterns: List<String>, cache: MutableMap<String, Long>): Long {
    if (design.isEmpty()) {
        return 1
    }

    return patterns
        .filter { design.startsWith(it) }
        .map {
            cache.getOrPut(design.substring(it.length)) {
                isFeasible(design.substring(it.length), patterns, cache)
            }
        }
        .sum()
}
