package day13

import java.io.File
import kotlin.math.abs
import kotlin.math.round
import kotlin.math.roundToLong

fun main() {
    val input = File("input.txt").readText()
        .split("\n\n")
        .flatMap {
            Regex("Button A: X\\+(\\d+), Y\\+(\\d+)\\nButton B: X\\+(\\d+), Y\\+(\\d+)\\nPrize: X=(\\d+), Y=(\\d+)")
                .findAll(it)
                .map { it.destructured.toList().map { it.toDouble() } }
                .toList()
        }

    var totalCostPart1 = 0L
    var totalCostPart2 = 0L
    for (values in input) {
        val ax = values[0]
        val ay = values[1]
        val bx = values[2]
        val by = values[3]
        val px = values[4]
        val py = values[5]
        run {
            val (a, b, cost) = calculateCost(ax, ay, bx, by, px, py)
            if (a < 100 && b < 100) {
                totalCostPart1 += cost
            }
        }
        run {
            val (_, _, cost) = calculateCost(ax, ay, bx, by, px + 10000000000000, py + 10000000000000)
            totalCostPart2 += cost
        }

    }

    println("part1 = $totalCostPart1")
    println("part2 = $totalCostPart2")
}

private fun calculateCost(
    ax: Double,
    ay: Double,
    bx: Double,
    by: Double,
    px: Double,
    py: Double
): Triple<Long, Long, Long> {
    // Equations:
    // ax * a + bx * b = px
    // ay * a + by * b = py
    //
    // Reorder second equation to a
    // a = (py - by * b) / ay
    //
    // Replace a in first equation
    // (py - by * b) / ay + by * b = px
    //
    // Reorder to calculate b
    // b = (px - ax * py / ay) / (bx - ax * by / ay)

    val b = (px - ax * py / ay) / (bx - ax * by / ay)
    val a = (py - by * b) / ay
    val cost = 3 * a + b

    // The floating point errors can be quite large, so the error can be > 0.0001
    if (a >= 0 && b >= 0 && abs(a - round(a)) < 0.01 && abs(b - round(b)) < 0.01) {
        return Triple(a.roundToLong(), b.roundToLong(), cost.roundToLong())
    } else {
        return Triple(0, 0, 0)
    }
}