package day14

import java.io.File
import java.lang.Thread.sleep
import kotlin.math.floor

fun main() {
    val input = File("input.txt").readLines()
        .map {
            Regex("p=(-?\\d+),(-?\\d+) v=(-?\\d+),(-?\\d+)")
                .findAll(it)
                .flatMap { it.destructured.toList() }
                .map { it.toInt() }
                .toList()
                .let { (x, y, vx, vy) -> Pair(Vec(x, y), Vec(vx, vy)) }
        }

    val quadrants = calculateRobotPosition(input, 100)
    println("part1 = $quadrants")

    hashed.clear()
    for (i in 0 until 10403 step 1) {
//        calculateRobotPosition(input, i, true)
        calculateRobotPosition(input, i, false)
    }

    // First vertical pattern: 14, repeats every 101 steps (=width)
    // First horizontal pattern: 64, repeats every 103 steps (=height)

    for (i in 14 until 10403 step 101) {
        for (j in 64 until 10403 step 103) {
            if (i == j) {
                println("part2 = $i")
                break
            }
        }
    }

}

val hashed = mutableMapOf<Int, Pair<Int, List<Vec>>>()

private fun calculateRobotPosition(input: List<Pair<Vec, Vec>>, time: Int, print: Boolean = false): Int {
    val (width, height) = Pair(101, 103)
//    val (width, height) = Pair(11, 7)
    val middleX = width / 2.0
    val middleY = height / 2.0

    var topLeft = 0
    var topRight = 0
    var bottomLeft = 0
    var bottomRight = 0

    val endPositions = mutableListOf<Vec>()
    for (robot in input) {
        val (pos, vel) = robot
        var endPosition = Vec(
            (pos.x + vel.x * time) % width,
            (pos.y + vel.y * time) % height
        )

        if (endPosition.x < 0) {
            endPosition = endPosition.copy(x = endPosition.x + width)
        }
        if (endPosition.y < 0) {
            endPosition = endPosition.copy(y = endPosition.y + height)
        }

        if (endPosition.x < floor(middleX) && endPosition.y < floor(middleY)) {
            topLeft += 1
        } else if (endPosition.x > middleX && endPosition.y < floor(middleY)) {
            topRight += 1
        } else if (endPosition.x < floor(middleX) && endPosition.y > middleY) {
            bottomLeft += 1
        } else if (endPosition.x > middleX && endPosition.y > middleY) {
            bottomRight += 1
        }
        endPositions.add(endPosition)
    }

    if (print) {
        println("---------------- $time")
        var output = ""
        for (y in 0 until height) {
            for (x in 0 until width) {
                if (endPositions.contains(Vec(x, y))) {
                    output += "#"
                } else {
                    output += " "
                }
            }
            output += "\n"
        }
        println(output)
        println("----------------")
        sleep(100)
    }


    val key = endPositions.hashCode()
    if (hashed.containsKey(key)) {
        if (endPositions == hashed[key]!!.second) {
            println("pattern starts repeating. max time = ${time}, min time = ${hashed[key]!!.first}")
        }
    }
    hashed.putIfAbsent(key, Pair(time, endPositions))

    val quadrants = topLeft * topRight * bottomLeft * bottomRight
    return quadrants
}

data class Vec(val x: Int, val y: Int)

