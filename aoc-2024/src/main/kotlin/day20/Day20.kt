package day20

import java.io.File
import java.util.PriorityQueue
import kotlin.math.abs

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

    part1(start, end, map)
    part2(start, end, map)
}

private fun part1(
    start: Point,
    end: Point,
    map: List<List<Char>>
) {
    val shortestFromStart = findShortestPath(start, map)
    val shortestFromEnd = findShortestPath(end, map)

    val cheatPaths = mutableListOf<Int>()
    for ((y, row) in map.withIndex()) {
        for ((x, ch) in row.withIndex()) {
            if (x == 0 || x == row.size - 1 || y == 0 || y == map.size - 1) {
                continue
            }
            if (ch == '#') {
                listOf(
                    Pair(Point(y - 1, x), Point(y + 1, x)),
                    Pair(Point(y, x - 1), Point(y, x + 1))
                )
                    .forEach { (a, b) ->
                        if (map[a.y][a.x] != '#' && map[b.y][b.x] != '#') {
                            val ad = shortestFromStart[a]!!
                            val bd = shortestFromStart[b]!!
                            val score = if (ad < bd) {
                                ad + shortestFromEnd[b]!! + 1
                            } else {
                                bd + shortestFromEnd[a]!! + 1
                            }
                            cheatPaths.add(score)
                        }
                    }
            }
        }
    }

    val shortestDistance = shortestFromStart[end]!!
    val result = cheatPaths.map { shortestDistance - it - 1 }.filter { it >= 100 }
    println("part1 = ${result.size}")
}

private fun part2(
    start: Point,
    end: Point,
    map: List<List<Char>>
) {
    val shortestFromStart = findShortestPath(start, map)
    val shortestFromEnd = findShortestPath(end, map)
    val shortestDistance = shortestFromStart[end]!!
    val cheatPaths = mutableMapOf<Pair<Point, Point>, Int>()
    for ((y, row) in map.withIndex()) {
        for ((x, ch) in row.withIndex()) {
            if (x == 0 || x == row.size - 1 || y == 0 || y == map.size - 1) {
                continue
            }
            if (ch != '#') {
                val origin = Point(y, x)
                for (y2 in -20..20) {
                    for (x2 in -20..20) {
                        val target = origin + Point(y2, x2)
                        if (target == origin) {
                            continue
                        }
                        val d = abs(target.y - origin.y) + abs(target.x - origin.x)
                        if (target.y > 0 && target.y < map.size && target.x > 0 && target.x < map[0].size
                            && map[target.y][target.x] != '#'
                            && d <= 20
                        ) {
                            val updateCheatPaths = { a: Point, b: Point ->
                                val p = Pair(a, b)
                                val distance = shortestFromStart[a]!! + shortestFromEnd[b]!! + d
                                if (distance <= shortestDistance - 100
                                    && cheatPaths.getOrDefault(p, Int.MAX_VALUE) > distance
                                ) {
                                    cheatPaths[p] = distance
                                }
                            }
                            updateCheatPaths(origin, target)
                            updateCheatPaths(target, origin)
                        }
                    }
                }
            }
        }
    }
//    println(cheatPaths.toList().map { shortestDistance - it.second }.groupBy { it }.mapValues { it.value.size }.toSortedMap())
    println("part2 = ${cheatPaths.size}")
}

fun findShortestPath(start: Point, map: List<List<Char>>): Map<Point, Int> {
    val visited = mutableMapOf<Point, Int>()
    val queue = PriorityQueue<Node>(compareBy { it.distance })
    queue.add(Node(start, 0))

    while (queue.isNotEmpty()) {
        var node = queue.remove()!!

        if (visited.getOrDefault(node.point, Int.MAX_VALUE) <= node.distance) {
            continue
        } else {
            visited[node.point] = node.distance
        }

        for (direction in listOf(Point(0, 1), Point(0, -1), Point(1, 0), Point(-1, 0))) {
            val next = node.point + direction
            if (map[next.y][next.x] == '#') continue
            var score = node.distance + 1
            queue.add(Node(next, score))
        }
    }
    return visited
}

data class Point(val y: Int, val x: Int) {
    operator fun plus(other: Point) = Point(y + other.y, x + other.x)
}

data class Node(val point: Point, val distance: Int)