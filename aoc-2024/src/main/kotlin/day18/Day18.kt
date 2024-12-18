package day18

import java.io.File
import java.util.PriorityQueue

fun main() {
    val corruptedMemory = File("input.txt")
        .readLines()
        .map {
            it.split(",")
                .map { it.toInt() }
                .let { (x, y) -> Point(y, x) }
        }

    val part1 = findShortestPath(corruptedMemory.take(1024).toSet())
    println("part1 = $part1")

    for (i in 1025 until corruptedMemory.size) {
        val result = findShortestPath(corruptedMemory.take(i).toSet())
        if (result == -1) {
            val point = corruptedMemory[i - 1]
            println("part2 = ${point.x},${point.y}")
            break
        }
    }
}

private fun findShortestPath(corruptedMemory: Set<Point>): Int {
//    val (height, width) = Pair(7, 7)
    val (height, width) = Pair(71, 71)
    val start = Point(0, 0)
    val end = Point(height - 1, width - 1)

    val visited = mutableMapOf<Point, Int>()
    val queue = PriorityQueue<Node>(compareBy { it.score })
    queue.add(Node(start, 0))

    while (queue.isNotEmpty()) {
        var node = queue.remove()!!

        if (visited.getOrDefault(node.point, Int.MAX_VALUE) <= node.score) {
            continue
        } else {
            visited[node.point] = node.score
        }

        if (node.point == end) {
            println(node)
            return node.score
        }

        for (direction in listOf(Point(0, 1), Point(0, -1), Point(1, 0), Point(-1, 0))) {
            val next = node.point + direction
            if (next.y < 0 || next.y >= height || next.x < 0 || next.x >= width) continue
            if (corruptedMemory.contains(next)) continue
            queue.add(Node(next, node.score + 1))
        }
    }
    return -1
}

data class Point(val y: Int, val x: Int) {
    operator fun plus(other: Point) = Point(y + other.y, x + other.x)
}

data class Node(val point: Point, val score: Int)