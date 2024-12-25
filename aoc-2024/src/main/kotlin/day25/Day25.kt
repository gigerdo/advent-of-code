package day25

import java.io.File
import kotlin.collections.map

fun main() {
    val (locks, keys) = File("input.txt")
        .readText()
        .split("\n\n")
        .map { it.split("\n").map { it.toList() } }
        .map {
            Pair(
                it[0].all { it == '#' },
                (0 until it[0].size)
                    .map { x -> it.map { it[x] } }
                    .map { it.count { it == '#' } - 1 }
            )
        }
        .partition { it.first }
        .toList()
        .map { it.map { it.second } }

    var matches = 0
    for (lock in locks) {
        outer@ for (key in keys) {
            for (i in 0 until lock.size) {
                if (lock[i] + key[i] > 5) {
                    continue@outer
                }
            }
            matches += 1
        }
    }

    println("part1 = $matches")
}