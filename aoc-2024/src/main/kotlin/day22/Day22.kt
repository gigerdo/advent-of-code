package day22

import java.io.File
import kotlin.collections.map

fun main() {
    val secrets = File("input.txt")
        .readLines()
        .map { it.toInt() }

    var totalSecrets = 0L
    val totalBananas = mutableMapOf<Sequence, Int>()
    for (secret in secrets) {
        val (newSecret, priceSequences) = generateSecret(secret)
        totalSecrets += newSecret

        // For part 2, we just count the total bananas for every possible sequence
        for ((seq, price) in priceSequences) {
            totalBananas.compute(seq) { _, v -> v?.plus(price) ?: price }
        }
    }
    println("part1 = $totalSecrets")
    println("part2 = ${totalBananas.values.max()}")
}

fun generateSecret(initialSecret: Int): Pair<Int, Map<Sequence, Int>> {
    val prices = mutableListOf<Int>()
    val diff = mutableListOf<Int>()
    var lastPrice = 0

    var current = initialSecret
    (0 until 2000).forEach { n ->
        val currentPrice = current.toString().last().digitToInt()
        prices.add(currentPrice)
        diff.add(currentPrice - lastPrice)
        lastPrice = currentPrice
        current = current.shl(6).xor(current).and(16777216 - 1)
        current = current.shr(5).xor(current).and(16777216 - 1)
        current = current.shl(11).xor(current).and(16777216 - 1)
    }

    val priceSequences = mutableMapOf<Sequence, Int>()
    for (n in 4 until diff.size) {
        val seq = diff.subList(n - 3, n + 1).toList()
        priceSequences.putIfAbsent(seq, prices[n])
    }

    return Pair(current, priceSequences)
}

typealias Sequence = List<Int>