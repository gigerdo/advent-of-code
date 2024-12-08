package day08

import java.io.File

fun main() {
    val input = File("input.txt").readLines()
        .map { it.toList() }

    val antinodes = findAntinodes(input)
    println("part1 = ${antinodes}")

    val antinodes2 = findAntinodes(input, false)
    println("part2 = ${antinodes2}")
}

private fun findAntinodes(input: List<List<Char>>, part1: Boolean = true): Int {
    val inBounds = fun(p: Point) = p.y >= 0 && p.y < input.size && p.x >= 0 && p.x < input[0].size

    return input
        .mapIndexed { y, row -> row.mapIndexed { x, c -> Pair(c, Point(y, x)) } }
        .flatten()
        .filter { it.first != '.' }
        .groupBy({ it.first }, { it.second })
        .map { (_, antennas) ->
            val antinodes = mutableListOf<Point>()
            for ((i, a) in antennas.withIndex()) {
                val otherAntennas = antennas.subList(i + 1, antennas.size)
                for (b in otherAntennas) {
                    val dv = b - a
                    if (part1) {
                        val first = a - dv
                        if (inBounds(first)) antinodes += first
                        val second = b + dv
                        if (inBounds(second)) antinodes += second
                    } else {
                        var p = a
                        while (inBounds(p)) {
                            antinodes += p
                            p -= dv
                        }
                        p = b
                        while (inBounds(p)) {
                            antinodes += p
                            p += dv
                        }
                    }

                }
            }
            antinodes.toSet()
        }
        .flatten()
        .toSet()
        .size
}

data class Point(val y: Int, val x: Int) {
    operator fun plus(other: Point): Point {
        return Point(y + other.y, x + other.x)
    }

    operator fun minus(other: Point): Point {
        return Point(y - other.y, x - other.x)
    }
}