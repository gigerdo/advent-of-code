package day09

import java.io.File

fun main() {
    val input = File("input.txt").readText().toList().map { it.toString().toInt() }

    part1(input)
    part2(input)
}

private fun part1(input: List<Int>) {
    val diskSize = input.sum()
    val disk = MutableList(diskSize) { -1 }
    var idx = 0
    val grouped = (input + 0).chunked(2)
    for ((fileId, value) in grouped.withIndex()) {
        val (fileSize, freeSpace) = value
        for (i in 0 until fileSize) {
            disk[idx] = fileId
            idx += 1
        }
        idx += freeSpace
    }

    var leftIdx = 0
    var rightIdx = diskSize - 1
    while (leftIdx < rightIdx) {
        while (disk[leftIdx] != -1) {
            leftIdx += 1
        }
        while (disk[rightIdx] == -1) {
            rightIdx -= 1
        }
        if (leftIdx < rightIdx) {
            disk[leftIdx] = disk[rightIdx]
            disk[rightIdx] = -1
        }
    }

    var sum = 0L
    for ((index, i) in disk.withIndex()) {
        if (i == -1) {
            break
        }
        sum += index * i
    }

    println("part1 = $sum")
}

private fun part2(input: List<Int>) {
    var idx = 0
    val entries = mutableListOf<Pair<Int, Int>>()
    for (size in input) {
        entries += Pair(idx, size)
        idx += size
    }

    val (files, freeSpaces) = (entries + Pair(input.size, 0))
        .chunked(2)
        .map { it.zipWithNext().single() }
        .unzip()
        .let { Pair(it.first, it.second.toMutableList()) }

    val diskSize = input.sum()
    val disk = MutableList(diskSize) { -1 }

    for ((fileId, file) in files.withIndex().reversed()) {
        val (filePos, fileSize) = file
        var foundSpace = false
        for ((spaceIdx, space) in freeSpaces.withIndex()) {
            val (spacePos, spaceSize) = space
            if (spacePos > filePos) {
                break
            }
            if (fileSize <= spaceSize) {
                for (i in 0 until fileSize) {
                    disk[spacePos + i] = fileId
                }
                freeSpaces[spaceIdx] = Pair(spacePos + fileSize, spaceSize - fileSize)
                foundSpace = true
                break
            }
        }
        if (!foundSpace) {
            for (i in 0 until fileSize) {
                disk[filePos + i] = fileId
            }
        }
    }

    var sum = 0L
    for ((index, i) in disk.withIndex()) {
        if (i == -1) {
            continue
        }
        sum += index * i
    }

    println("part2 = $sum")
}
