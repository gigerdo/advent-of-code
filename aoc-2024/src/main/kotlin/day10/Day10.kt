package day10

import java.io.File

fun main() {
    val input = File("input.txt")
        .readLines()
        .map { it.toList() }

    val endPositions = mutableListOf<Point>()
    input.mapIndexed { y, row ->
        row.mapIndexed { x, c ->
            if (c == '9') {
                endPositions.add(Point(y, x))
            }
        }
    }

    var distinctEndPositions = mutableMapOf<Point, Int>()
    var allTrails = mutableMapOf<Point, Int>()

    for (point in endPositions) {
        var startPoints = findStart(input, point)
        startPoints
            .distinct()
            .forEach {
                distinctEndPositions.compute(it) { _, v -> v?.plus(1) ?: 1 }
            }

        startPoints
            .groupingBy { it }
            .eachCount()
            .forEach {
                allTrails.compute(it.key) { _, v -> v?.plus(it.value) ?: it.value }
            }
    }

    println("part1 = ${distinctEndPositions.values.sum()}")
    println("part2 = ${allTrails.values.sum()}")
}

fun findStart(map: List<List<Char>>, start: Point): List<Point> {
    var pos = start
    while (true) {
        if (map[pos.y][pos.x] == '0') {
            return listOf(pos)
        }
        val neighbors = findNeighbors(map, pos)
        if (neighbors.isEmpty()) {
            return emptyList()
        } else if (neighbors.size == 1) {
            pos = neighbors.first()
        } else {
            return neighbors
                .flatMap { findStart(map, it) }
        }
    }
}

fun findNeighbors(map: List<List<Char>>, start: Point): List<Point> {
    return listOf(Point(0, 1), Point(0, -1), Point(1, 0), Point(-1, 0))
        .flatMap {
            val p = Point(start.y + it.y, start.x + it.x)
            if (p.y >= 0 && p.y < map.size
                && p.x >= 0 && p.x < map[0].size
                && map[start.y][start.x] - map[p.y][p.x] == 1
            ) {
                listOf(p)
            } else {
                emptyList()
            }
        }
}

data class Point(val y: Int, val x: Int)
