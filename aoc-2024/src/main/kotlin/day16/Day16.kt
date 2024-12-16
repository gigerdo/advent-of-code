package day16

import java.io.File
import java.util.PriorityQueue

fun main() {
    val map = File("input.txt")
        .readLines()
        .map { it.toList() }

    // Find (S)tart and (E)nd
    val start = map.withIndex()
        .filter { (_, row) -> 'S' in row }
        .map { (y, row) -> Point(y, row.indexOf('S')) }
        .single()
    val end = map.withIndex()
        .filter { (_, row) -> 'E' in row }
        .map { (y, row) -> Point(y, row.indexOf('E')) }
        .single()

    // Find the shortest paths from S to E
    var shortestPathScore = 0 // For part1 we just need the score of the shortest path
    val shortestPathPoints = mutableSetOf<Point>() // For part2 we need all points of all shortest paths
    val visited = mutableMapOf<Pair<Point, Point>, Int>()
    val queue = PriorityQueue<Node>(compareBy { it.score })
    queue.add(Node(start, Point(0, 1), 0, listOf(start)))

    while (queue.isNotEmpty()) {
        var node = queue.remove()!!

        if (visited.getOrDefault(node.point to node.direction, Int.MAX_VALUE) < node.score) {
            continue
        } else {
            visited[node.point to node.direction] = node.score
        }

        if (node.point == end) {
            if (shortestPathScore == 0) {
                shortestPathScore = node.score
                shortestPathPoints.addAll(node.path)
                continue
            } else if (shortestPathScore == node.score) {
                shortestPathPoints.addAll(node.path)
                continue
            } else {
                break
            }
        }

        for (direction in listOf(Point(0, 1), Point(0, -1), Point(1, 0), Point(-1, 0))) {
            val next = node.point + direction
            if (map[next.y][next.x] == '#') continue
            var score = node.score + 1
            if (direction != node.direction) score += 1000
            queue.add(Node(next, direction, score, node.path + next))
        }
    }

    println("part1 = $shortestPathScore")
    println("part2 = ${shortestPathPoints.size}")
}

data class Point(val y: Int, val x: Int) {
    operator fun plus(other: Point) = Point(y + other.y, x + other.x)
}

data class Node(val point: Point, val direction: Point, val score: Int, val path: List<Point>)