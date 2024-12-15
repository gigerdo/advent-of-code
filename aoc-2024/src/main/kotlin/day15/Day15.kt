package day14

import java.io.File

fun main() {
    val (map, moves) = File("input.txt")
        .readText()
        .split("\n\n")
        .let { (map, moves) ->
            Pair(map.split("\n").map { it.toList() }, moves.toList().filter { it != '\n' })
        }

    part1(map.map { it.toMutableList() }, moves)
    println()
    part2(map, moves)
}

private fun part1(
    map: List<MutableList<Char>>,
    moves: List<Char>
) {
    var position = map.withIndex()
        .filter { (_, row) -> '@' in row }
        .map { (y, row) -> Point(y, row.indexOf('@')) }
        .single()

    for (move in moves) {
        val direction = when (move) {
            '^' -> Point(-1, 0)
            'v' -> Point(1, 0)
            '<' -> Point(0, -1)
            '>' -> Point(0, 1)
            else -> throw IllegalArgumentException("Invalid move: $move")
        }

        var newPosition = position
        while (true) {
            newPosition = Point(newPosition.y + direction.y, newPosition.x + direction.x)
            if (map[newPosition.y][newPosition.x] == '#') {
                break
            }
            if (map[newPosition.y][newPosition.x] == '.') {
                map[position.y][position.x] = '.'
                position = Point(position.y + direction.y, position.x + direction.x)
                map[newPosition.y][newPosition.x] = 'O'
                map[position.y][position.x] = '@'
                break
            }
        }
    }

    var gpsCoordinates = calculateCoordinates(map)
    println("part1 = $gpsCoordinates")
}

private fun part2(map: List<List<Char>>, moves: List<Char>) {
    val map = map.map {
        it.zip(it)
            .map { p ->
                if (p.first == 'O') '[' to ']'
                else if (p.first == '@') '@' to '.'
                else p
            }
            .flatMap { it.toList() }
            .toMutableList()
    }

    var position = map.withIndex()
        .filter { (_, row) -> '@' in row }
        .map { (y, row) -> Point(y, row.indexOf('@')) }
        .single()

    move@ for (move in moves) {
        val direction = when (move) {
            '^' -> Point(-1, 0)
            'v' -> Point(1, 0)
            '<' -> Point(0, -1)
            '>' -> Point(0, 1)
            else -> throw IllegalArgumentException("Invalid move: $move")
        }

        val newPos = position + direction

        if (map[newPos.y][newPos.x] == '#') {
            // Already next to wall
            continue
        }

        if (map[newPos.y][newPos.x] == '.') {
            // Empty space
            map[position.y][position.x] = '.'
            map[newPos.y][newPos.x] = '@'
            position = newPos
            continue
        }

        // If no empty space or wall, we are moving into a box
        var currentBoxes = mutableSetOf<Box>() // This tracks the boxes that are currently being moved
        if (map[newPos.y][newPos.x] == '[') {
            currentBoxes.add(Box(newPos))
        } else if (map[newPos.y][newPos.x] == ']') {
            currentBoxes.add(Box(newPos + Point(0, -1)))
        } else {
            throw IllegalArgumentException("Invalid element: ${map[newPos.y][newPos.x]}")
        }
        // Track all boxes that need to be moved, so we can update them later
        var boxesBeingMoved = mutableSetOf<Box>()
        while (!currentBoxes.isEmpty()) {
            boxesBeingMoved.addAll(currentBoxes)

            // For all boxes that are currently being moved, check if they can move to the next position
            val newBoxes = mutableSetOf<Box>()
            for (box in currentBoxes) {
                val left = box.left + direction
                val right = left + Point(0, 1)
                if (listOf(left, right).any { map[it.y][it.x] == '#' }) {
                    // We hit a wall, don't move at all
                    continue@move
                }
                for (point in listOf(left, right)) {
                    val newBox = if (map[point.y][point.x] == '[') {
                        Box(point)
                    } else if (map[point.y][point.x] == ']') {
                        Box(point + Point(0, -1))
                    } else {
                        null
                    }
                    // Ensure we aren't adding any boxes from the current step (For left/right case)
                    if (newBox != null && !currentBoxes.contains(newBox)) {
                        newBoxes.add(newBox)
                    }
                }
            }
            currentBoxes = newBoxes
        }

        // First clear all boxes, then add them back to avoid overwriting each other
        for (box in boxesBeingMoved) {
            map[box.left.y][box.left.x] = '.'
            map[box.left.y][box.left.x + 1] = '.'
        }
        for (box in boxesBeingMoved) {
            val new = box.left + direction
            map[new.y][new.x] = '['
            map[new.y][new.x + 1] = ']'
        }

        // Move robot
        map[position.y][position.x] = '.'
        map[newPos.y][newPos.x] = '@'
        position = newPos
    }

    var gpsCoordinates = calculateCoordinates(map)
    println("part2 = $gpsCoordinates")
}

private fun calculateCoordinates(map: List<MutableList<Char>>): Int {
    var gpsCoordinates = 0
    for ((y, rows) in map.withIndex()) {
        for ((x, ch) in rows.withIndex()) {
            print(ch)
            if (ch == 'O' || ch == '[') {
                gpsCoordinates += 100 * y + x
            }
        }
        println()
    }
    return gpsCoordinates
}

data class Point(val y: Int, val x: Int) {
    operator fun plus(other: Point) = Point(y + other.y, x + other.x)
}

data class Box(val left: Point)
