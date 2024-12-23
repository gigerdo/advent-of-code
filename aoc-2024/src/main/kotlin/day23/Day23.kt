package day23

import java.io.File
import kotlin.collections.map

fun main() {
    val input = File("input.txt")
        .readLines()
        .map { it.split("-") }

    val connections = mutableMapOf<String, MutableList<String>>()
    for ((a, b) in input) {
        connections.compute(a) { _, v -> v?.add(b); v ?: mutableListOf(b) }
        connections.compute(b) { _, v -> v?.add(a); v ?: mutableListOf(a) }
    }

    part1(connections)
    part2(connections)
}

private fun part1(connections: MutableMap<String, MutableList<String>>) {
    val result = mutableSetOf<List<String>>()
    for ((key, others) in connections) {
        for (i in 0 until others.size) {
            for (j in i + 1 until others.size) {
                val a = others[i]
                val b = others[j]
                if (a[0] == 't' || b[0] == 't' || key[0] == 't') {
                    if (connections[a]!!.contains(b)) {
                        result.add(listOf(key, a, b).sorted())
                    }
                }
            }
        }
    }

    println("part1 = ${result.size}")
}

private fun part2(connections: MutableMap<String, MutableList<String>>) {
    val result = findAllMaximalCliques(emptySet(), connections.keys, emptySet(), connections)
    val maximumClique = result.maxBy { it.size }
    val message = maximumClique.sorted().joinToString(",")
    println("part2 = $message")
}

/** https://en.wikipedia.org/wiki/Bron-Kerbosch_algorithm */
fun findAllMaximalCliques(
    r: Set<String>,
    pp: Set<String>,
    xx: Set<String>,
    connections: MutableMap<String, MutableList<String>>
): List<Set<String>> {
    var p = pp
    var x = xx
    if (p.isEmpty() && x.isEmpty()) {
        return listOf(r)
    }

    val result = mutableListOf<Set<String>>()
    for (v in p) {
        val n = connections[v]!!
        val maximalCliques = findAllMaximalCliques(r.plus(v), p.intersect(n), x.intersect(n), connections)
        result.addAll(maximalCliques)
        p = p.minus(v)
        x = x.plus(v)
    }
    return result
}
