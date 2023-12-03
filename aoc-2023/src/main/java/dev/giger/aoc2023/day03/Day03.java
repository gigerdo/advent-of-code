package dev.giger.aoc2023.day03;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.stream.IntStream;

public class Day03 {
	static List<String> digits = IntStream.range(0, 10).mapToObj(String::valueOf).toList();

	public static void main(String[] args) throws IOException {
		var lines = Files.readAllLines(Path.of("aoc-2023/src/main/resources/day03/input.txt"));
		var map = lines.stream()
			.map(line -> split(line, ""))
			.toList();

		var sum = 0;
		var currentNumber = "";
		var gears = new HashMap<Position, List<Integer>>();
		for (int y = 0; y < map.size(); y++) {
			var line = map.get(y);
			for (int x = 0; x < line.size(); x++) {
				var element = line.get(x);
				if (digits.contains(element)) {
					currentNumber += element;
				}
				if (!currentNumber.isEmpty()) {
					if (x == line.size() - 1 || // End of line
						!digits.contains(line.get(x + 1)) // Next char not a number
					) {
						var isPartNumber = touchesSpecialCharacter(currentNumber, y, x, map, gears);
						if (isPartNumber) {
							var numberValue = Integer.parseInt(currentNumber);
							sum += numberValue;
						}
						currentNumber = "";
					}
				}
			}
		}

		System.out.println("Part 1");
		System.out.println(sum);

		System.out.println("Part 2");
		var result = gears.values().stream()
			.filter(v -> v.size() == 2)
			.map(v -> v.get(0) * v.get(1))
			.mapToInt(i -> i)
			.sum();
		System.out.println(result);
	}

	private static boolean touchesSpecialCharacter(String currentNumber, int y, int x, List<List<String>> map, HashMap<Position, List<Integer>> gears) {
		// Check all neighbors
		var hasSpecialNeighour = false;
		var addedToGears = false;
		for (int offset = 0; offset < currentNumber.length(); offset++) {
			for (int i = -1; i <= 1; i++) {
				for (int j = -1; j <= 1; j++) {
					var yToCheck = y + i;
					var xToCheck = x - offset + j;
					if (outOfBounds(yToCheck, xToCheck, map)) {
						continue;
					}
					var elementToCheck = map.get(yToCheck).get(xToCheck);
					if (digits.contains(elementToCheck)) {
						continue;
					}
					if (elementToCheck.equals(".")) {
						continue;
					}
					if (elementToCheck.equals("*")) {
						if (!addedToGears) {
							addedToGears = true;
							gears.compute(new Position(yToCheck, xToCheck), (k, v) -> {
								var value = Integer.parseInt(currentNumber);
								if (v == null) {
									return List.of(value);
								}
								var newList = new ArrayList<>(v);
								newList.add(value);
								return newList;
							});
						}
					}
					hasSpecialNeighour = true;
				}
			}
		}
		return hasSpecialNeighour;
	}

	private static boolean outOfBounds(int yToCheck, int xToCheck, List<List<String>> map) {
		if (yToCheck < 0 || yToCheck >= map.size()) {
			return true;
		}
		if (xToCheck < 0 || xToCheck >= map.get(0).size()) {
			return true;
		}
		return false;
	}

	private static List<String> split(String in, String separator) {
		return Arrays.stream(in.trim().split(separator)).toList();
	}

	record Position(
		int y,
		int x
	) {
	}
}