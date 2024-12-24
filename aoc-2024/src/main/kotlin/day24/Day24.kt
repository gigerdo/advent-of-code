package day24

import java.io.File
import kotlin.collections.map

fun main() {
    val (wires, gates) = File("input.txt")
        .readText()
        .split("\n\n")
        .let { (inputs, gates) ->
            Pair(
                inputs.split("\n").map { it.split(": ") },
                gates.split("\n").map { it.split(" ") }.map { Gate(it[0], it[2], it[1], it[4]) }
            )
        }

    val result = calculateOutput(gates, wires)
    println("part1 = ${result.toLong(2)}")

    part2(wires, gates)
}

data class Gate(val i0: String, val i1: String, val op: String, val out: String)

private fun calculateOutput(
    gates: List<Gate>,
    wires: List<List<String>>
): String {
    val gatesByOutput = gates.map { it.out to it }.toMap()
    val outputs = mutableMapOf<String, Int>()
    wires.forEach { outputs.put(it[0], it[1].toInt()) }
    val result = gatesByOutput.keys
        .filter { it.startsWith("z") }
        .sortedDescending()
        .map { getOutput(it, gatesByOutput, outputs) }
        .joinToString("")
    return result
}

fun getOutput(
    output: String,
    gates: Map<String, Gate>,
    cache: MutableMap<String, Int>
): Int {
    if (cache.containsKey(output)) {
        return cache[output]!!
    }

    val gate = gates[output]!!
    val i0 = getOutput(gate.i0, gates, cache)
    val i1 = getOutput(gate.i1, gates, cache)
    val result = when (gate.op) {
        "AND" -> {
            i0.and(i1)
        }

        "OR" -> {
            i0.or(i1)
        }

        "XOR" -> {
            i0.xor(i1)
        }

        else -> throw IllegalArgumentException("Unknown operation: ${gate.op}")
    }
    cache.put(output, result)
    return result
}

private fun part2(
    wires: List<List<String>>,
    gates: List<Gate>
) {
    println()
    println("Part2 calculations")
    val x = wires
        .filter { it[0].startsWith("x") }
        .sortedByDescending { it[0] }
        .map { it[1] }
        .joinToString("")
    val y = wires
        .filter { it[0].startsWith("y") }
        .sortedByDescending { it[0] }
        .map { it[1] }
        .joinToString("")

    val gatesByOutput = gates.map { it.out to it }.toMap().toMutableMap()
    val swaps = listOf(
        "kfp" to "hbs", // fixes z09
        "z18" to "dhq", // fixes z18
        "z22" to "pdg", // fixes z22
        "z27" to "jcp" // fixes z27
    )

    swaps.forEach { (a, b) -> swapGates(gatesByOutput, a, b) }

    val z = calculateOutput(gatesByOutput.values.toList(), wires)

    println(x.padStart(48).chunked(4).joinToString(" "))
    println(y.padStart(48).chunked(4).joinToString(" "))
    println("Sum")
    println(z.padStart(48).chunked(4).joinToString(" "))
    println("Expected")
    val expected = (x.toLong(2) + y.toLong(2)).toString(2)
    println(expected.padStart(48).chunked(4).joinToString(" "))


    for (i in 0 until expected.length) {
        val index = expected.length - i - 1
        if (expected[index] != z[index]) {
            val output = "z${i.toString().padStart(2, '0')}"
            println("Error at $output")
            println()
            printWire(output, gatesByOutput, 0, wires.map { it[0] to it[1] }.toMap())
            println()
            println("Possible gates to swap")
            val input = "x${i.toString().padStart(2, '0')}"
            val l1 = gates.filter { it.i0 == input || it.i1 == input }
            println(l1)
            val l2 = l1.map { it.out }.flatMap { out -> gates.filter { it.i0 == out || it.i1 == out } }
            println(l2)
            break
        }
    }

    println()
    println("part2 = ${swaps.flatMap { it.toList() }.sorted().joinToString(",")}")
}

private fun swapGates(
    gatesByOutput: MutableMap<String, Gate>,
    string: String,
    string1: String
) {
    val kfp = gatesByOutput[string]!!
    val hbs = gatesByOutput[string1]!!
    gatesByOutput.put(string1, kfp.copy(out = string1))
    gatesByOutput.put(string, hbs.copy(out = string))
}

fun printWire(
    string: String,
    gates: Map<String, Gate>,
    indent: Int,
    wires: Map<String, String>
) {
    if (wires.contains(string)) {
        println(" ".repeat(indent) + string + "=" + wires[string])
        return
    }

    val gate = gates[string]!!
    println(" ".repeat(indent) + string + " " + gate.op)
    printWire(gate.i0, gates, indent + 1, wires)
    printWire(gate.i1, gates, indent + 1, wires)
}