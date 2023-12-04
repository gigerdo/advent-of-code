package dev.giger.aoc2023.day04;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

public class Day04 {
	public static void main(String[] args) throws IOException {
		var lines = Files.readAllLines(Path.of("aoc-2023/src/main/resources/day04/input.txt"));

		var scratchCards = new ArrayList<Integer>();
		var result = lines.stream()
			.map(line -> {
				var lineSplit = split(line, "[:|]");
				var winningNumbers = toSet(split(lineSplit.get(1), " +"));
				var myNumbers = toSet(split(lineSplit.get(2), " +"));

				var myWinningNumbers = new HashSet<>(myNumbers);
				myWinningNumbers.retainAll(winningNumbers);
				scratchCards.add(myWinningNumbers.size());

				if (myWinningNumbers.isEmpty()) {
					return 0.0;
				}
				return Math.pow(2, myWinningNumbers.size() - 1);
			})
			.mapToInt(Double::intValue)
			.sum();

		System.out.println("Part 1");
		System.out.println(result);

		var cardsCounter = new ArrayList<Integer>(scratchCards.size());
		for (int i = 0; i < scratchCards.size(); i++) {
			cardsCounter.add(1);
		}
		for (int i = 0; i < scratchCards.size(); i++) {
			var card = scratchCards.get(i);
			var counter = cardsCounter.get(i);
			for (int j = 0; j < card; j++) {
				int idx = i + j + 1;
				if (idx >= cardsCounter.size()) {
					break;
				}
				cardsCounter.set(idx, cardsCounter.get(idx) + counter);
			}
		}
		var result2 = cardsCounter.stream().reduce(0, Integer::sum);

		System.out.println("Part 2");
		System.out.println(result2);
	}

	private static Set<Integer> toSet(List<String> split) {
		return split.stream()
			.map(Integer::valueOf)
			.collect(Collectors.toSet());
	}

	private static List<String> split(String in, String separator) {
		return Arrays.stream(in.trim().split(separator)).toList();
	}

}