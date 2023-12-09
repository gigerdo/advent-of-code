package dev.giger.aoc2023.day09;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Day09 {
	public static void main(String[] args) throws IOException {
		var lines = Files.readAllLines(Path.of("aoc-2023/src/main/resources/day09/input.txt"));

		var part1 = 0L;
		var part2 = 0L;
		for (String line : lines) {
			var numbers = split(line, " ").stream().map(Long::valueOf).toList();

			var derivations = new ArrayList<List<Long>>();
			derivations.add(new ArrayList<>(numbers));

			var allZero = true;
			while (true) {
				var toDerive = derivations.get(derivations.size() - 1);
				var derived = new ArrayList<Long>();
				for (int i = 1; i < toDerive.size(); i++) {
					var a = toDerive.get(i - 1);
					var b = toDerive.get(i);
					long diff = b - a;
					derived.add(diff);
					allZero = allZero && diff == 0L;
				}
				derivations.add(derived);
				if (allZero) {
					break;
				}
				allZero = true;
			}

			for (int i = derivations.size() - 2; i >= 0; i--) {
				var previousDerivation = derivations.get(i + 1);
				var currentDerivation = derivations.get(i);

				// Part 1
				var pdLastValue = tail(previousDerivation, 0);
				var cdLastValue = tail(currentDerivation, 0);
				currentDerivation.add(cdLastValue + pdLastValue);

				// Part 2
				var beforeFirst = currentDerivation.get(0) - previousDerivation.get(0);
				currentDerivation.add(0, beforeFirst);
			}

			part1 += tail(derivations.get(0), 0);
			part2 += derivations.get(0).get(0);
		}

		System.out.println("Part 1");
		System.out.println(part1);

		System.out.println("Part 2");
		System.out.println(part2);
	}

	public static <T> T tail(List<T> in, int offset) {
		return in.get(in.size() - 1 - offset);
	}

	private static List<String> split(String in, String separator) {
		return Arrays.stream(in.strip().split(separator)).toList();
	}
}