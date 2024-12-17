package day17

import java.io.File
import kotlin.math.pow

fun main() {
    val input = File("input.txt")
        .readText()
        .let {
            Regex("Register A: (\\d+)\\nRegister B: (\\d+)\\nRegister C: (\\d+)\\n\\nProgram: (.*)")
                .findAll(it)
                .map { it.destructured }
                .map { (a, b, c, program) ->
                    Pair(
                        Triple(a.toLong(), b.toLong(), c.toLong()),
                        program.split(",").map { it.toInt() })
                }
                .single()
        }

    val output = runProgram(input)
    println("part1 = ${output.joinToString(",")}")

    println("part2 = ${part2(input)}")
}

private fun runProgram(input: Pair<Triple<Long, Long, Long>, List<Int>>): MutableList<Int> {
    var ip = 0
    var (a, b, c) = input.first
    val program = input.second
    val output = mutableListOf<Int>()

    val combo = fun(x: Int): Int {
        return when (x) {
            0, 1, 2, 3 -> x
            4 -> a.toInt()
            5 -> b.toInt()
            6 -> c.toInt()
            else -> throw IllegalArgumentException("Invalid combo $x")
        }
    }

    while (ip < program.size) {
        val instruction = program[ip]
        val operand = program[ip + 1]
        ip += 2

        when (instruction) {
            0 -> a = a.shr(combo(operand)) // adv
            1 -> b = b xor operand.toLong() // bxl
            2 -> b = combo(operand).mod(8L) // bst
            3 -> if (a != 0L) ip = operand //jnz
            4 -> b = b xor c // bxc
            5 -> output += combo(operand).mod(8) // out
            6 -> b = a.shr(combo(operand)) // bdv
            7 -> c = a.shr(combo(operand)) //cdv
        }
    }
    return output
}

private fun part2(input: Pair<Triple<Long, Long, Long>, List<Int>>): Long {
    val program = input.second

    // Program steps:
    // 2,4 -> b = a % 8
    // 1,2 -> b = b xor 2
    // 7,5 -> c = a / 2^c
    // 4,5 -> b = b xor c
    // 1,3 -> b = b xor 3
    // 5,5 -> out b
    // 0,3 -> a = a / 2^3
    // 3,0 -> Start from the beginning

    // Iteration is only dependent on a
    // Every iteration, a is divided by 8
    // So to get 16 outputs, the program must be between 8^15 - 8^16
    val len = program.size
    val start = 8.0.pow(len - 1).toLong()

    // The program uses  a%8 to calculate the output, so only 3 bits of a are relevant for each output
    // So we can search each 3bit number individually to find the number that produces the correct matching output

    // Start in binary:
    // 001 000 000 000 000 000 000 000 000 000 000 000 000 000 000 000
    // ^15 ^14 ^13 ...                                         ^1  ^0
    // Should match the program in reverse
    // ^0  ^3  ^3  ^0 etc.

    var numbers = listOf<Long>(start)
    for (i in len - 1 downTo 0) {
        val newNumbers = mutableSetOf<Long>()
        for (base in numbers) {
            outer@ for (d in 0L until 8L) {
                val a = base + d.shl(i * 3)

                val output = runProgram(Pair(Triple(a, 0, 0), program))

                // Check all outputs up to i are correct
                for (j in len - 1 downTo i) {
                    if (output[j] != program[j]) {
                        continue@outer
                    }
                }
                if (false) {
                    println(
                        "${"%2d".format(i)}: " +
                            a.toString(2).padStart(len * 3, '0').chunked(3).joinToString(" ") +
                            ": $output"
                    )
                }

                // Any number that has the correct output so far, we use to check the next 3 bits
                newNumbers += a

            }
        }
        numbers = newNumbers.toList()
    }

    return numbers.first()
}
