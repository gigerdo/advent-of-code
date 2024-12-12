package day12

import java.io.File

fun main() {
    val input = File("input.txt")
        .readLines()
        .map { it.toList() }

    val visited = MutableList(input.size) { MutableList(input[0].size) { false } }

    var pricePart1 = 0L
    var pricePart2 = 0L
    while (true) {
        val plot: Point? = findPlot(visited)
        if (plot == null) {
            break
        }

        val plant = input[plot.y][plot.x]

        var area = 0L
        var perimeter = 0L
        var sides = mutableMapOf<Point, List<Point>>()

        val queue = mutableListOf<Point>()
        queue.add(plot)
        visited[plot.y][plot.x] = true
        while (queue.isNotEmpty()) {
            val p = queue.removeLast()
            area += 1
            for (dir in directions) {
                val neighbor = Point(p.y + dir.y, p.x + dir.x)
                if (!inBounds(neighbor, input)) {
                    perimeter += 1
                    addToSides(sides, dir, neighbor)
                    continue
                }
                val neighborPlant = input[neighbor.y][neighbor.x]
                if (neighborPlant == plant && !visited[neighbor.y][neighbor.x]) {
                    queue.add(neighbor)
                    visited[neighbor.y][neighbor.x] = true
                } else if (neighborPlant != plant) {
                    perimeter += 1
                    addToSides(sides, dir, neighbor)
                }
            }
        }

        pricePart1 += area * perimeter

        val totalSides = directions
            .map { sides[it]!! }
            .map { it.sortedWith(compareBy({ it.y }, { it.x })) }
            .map { groupNeighboringPoints(it) }
            .map { it.size }
            .sum()
        pricePart2 += area * totalSides

        println("Plot $plant: area=[$area], perimeter=[$perimeter], sides[$totalSides]")
    }

    println("part1 = $pricePart1")
    println("part2 = $pricePart2")
}

fun findPlot(visited: List<List<Boolean>>): Point? {
    for (y in visited.indices) {
        for (x in visited[y].indices) {
            if (!visited[y][x]) {
                return Point(y, x)
            }
        }
    }
    return null
}

val directions = listOf(Point(0, 1), Point(0, -1), Point(1, 0), Point(-1, 0))

private fun inBounds(
    pos: Point,
    input: List<List<Char>>
): Boolean {
    return pos.y >= 0 && pos.y < input.size && pos.x >= 0 && pos.x < input[0].size
}

private fun addToSides(
    sides: MutableMap<Point, List<Point>>,
    dir: Point,
    neighbor: Point
) {
    sides.compute(dir) { _, v ->
        if (v == null) {
            listOf(neighbor)
        } else {
            v + neighbor
        }
    }
}

private fun groupNeighboringPoints(right: List<Point>): MutableList<MutableList<Point>> {
    val combinedSides = mutableListOf<MutableList<Point>>()
    for (point in right) {
        var found = false
        outer@ for (points in combinedSides) {
            for (item in points) {
                if (neighbors(point, item)) {
                    points.add(point)
                    found = true
                    break@outer
                }
            }
        }
        if (!found) {
            combinedSides.add(mutableListOf(point))
        }
    }
    return combinedSides
}

private fun neighbors(a: Point, b: Point): Boolean {
    return directions.any { a.y + it.y == b.y && a.x + it.x == b.x }
}

data class Point(val y: Int, val x: Int)
