package dev.giger.aoc2023.day14;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;

public class Day14 {

	public static void main(String[] args) throws IOException {
		var lines = Files.readAllLines(Path.of("aoc-2023/src/main/resources/day14/input.txt"));
		var map = new ArrayList<>(lines.stream().map(l -> new ArrayList<>(split(l, ""))).toList());

		var states = new HashMap<String, Integer>();
		var skipped = false;
		var part1 = 0L;
		for (int i = 0; i < 1_000_000_000; i++) {
			System.out.println(Instant.now() + ": " + i);
			moveNorth(map);
			if (i == 0) {
				part1 = calculateLoad(map);
			}

			moveWest(map);
			moveSouth(map);
			moveEast(map);

			var key = map.stream().flatMap(Collection::stream).collect(Collectors.joining());
			if (states.containsKey(key) && !skipped) {
				var previousI = states.get(key);
				var diff = i - previousI;
				System.out.println("Found repetition " + previousI + "-" + i + " -" + diff);
				i = previousI + ((1_000_000_000 - previousI) / diff) * diff;
				System.out.println("New i " + i);
				skipped = true;
			} else {
				states.put(key, i);
			}
		}


		var sum = calculateLoad(map);

		System.out.println("Part 1");
		System.out.println(part1);

		System.out.println("Part 2");
		System.out.println(sum);
	}

	private static long calculateLoad(ArrayList<ArrayList<String>> map) {
		var sum = 0L;
		for (int y = 0; y < map.size(); y++) {
			for (int x = 0; x < map.get(0).size(); x++) {
				String el = map.get(y).get(x);
				System.out.print(el);
				if (el.equals("O")) {
					sum += map.size() - y;
				}
			}
			System.out.println();
		}
		return sum;
	}

	private static void moveNorth(ArrayList<ArrayList<String>> map) {
		for (int x = 0; x < map.get(0).size(); x++) {
			var hasEmptySpace = false;
			var firstEmptyIndex = 0;
			for (int y = 0; y < map.size(); y++) {
				String element = map.get(y).get(x);
				if (element.equals("O")) {
					if (hasEmptySpace) {
						// Move to top
						map.get(y).set(x, ".");
						map.get(firstEmptyIndex).set(x, "O");
						firstEmptyIndex += 1;
					}
				} else if (element.equals("#")) {
					hasEmptySpace = false;
				} else if (element.equals(".")) {
					if (!hasEmptySpace) {
						firstEmptyIndex = y;
						hasEmptySpace = true;
					}
				}
			}
		}
	}

	private static void moveSouth(ArrayList<ArrayList<String>> map) {
		for (int x = 0; x < map.get(0).size(); x++) {
			var hasEmptySpace = false;
			var firstEmptyIndex = 0;
			for (int y = map.size() - 1; y >= 0; y--) {
				String element = map.get(y).get(x);
				if (element.equals("O")) {
					if (hasEmptySpace) {
						// Move to top
						map.get(y).set(x, ".");
						map.get(firstEmptyIndex).set(x, "O");
						firstEmptyIndex -= 1;
					}
				} else if (element.equals("#")) {
					hasEmptySpace = false;
				} else if (element.equals(".")) {
					if (!hasEmptySpace) {
						firstEmptyIndex = y;
						hasEmptySpace = true;
					}
				}
			}
		}
	}

	private static void moveWest(ArrayList<ArrayList<String>> map) {
		for (int y = 0; y < map.size(); y++) {
			var hasEmptySpace = false;
			var firstEmptyIndex = 0;
			for (int x = 0; x < map.get(0).size(); x++) {
				String element = map.get(y).get(x);
				if (element.equals("O")) {
					if (hasEmptySpace) {
						// Move to top
						map.get(y).set(x, ".");
						map.get(y).set(firstEmptyIndex, "O");
						firstEmptyIndex += 1;
					}
				} else if (element.equals("#")) {
					hasEmptySpace = false;
				} else if (element.equals(".")) {
					if (!hasEmptySpace) {
						firstEmptyIndex = x;
						hasEmptySpace = true;
					}
				}
			}
		}
	}

	private static void moveEast(ArrayList<ArrayList<String>> map) {
		for (int y = 0; y < map.size(); y++) {
			var hasEmptySpace = false;
			var firstEmptyIndex = 0;
			for (int x = map.get(0).size() - 1; x >= 0; x--) {
				String element = map.get(y).get(x);
				if (element.equals("O")) {
					if (hasEmptySpace) {
						// Move to top
						map.get(y).set(x, ".");
						map.get(y).set(firstEmptyIndex, "O");
						firstEmptyIndex -= 1;
					}
				} else if (element.equals("#")) {
					hasEmptySpace = false;
				} else if (element.equals(".")) {
					if (!hasEmptySpace) {
						firstEmptyIndex = x;
						hasEmptySpace = true;
					}
				}
			}
		}
	}

	private static List<String> split(String in, String separator) {
		return Arrays.stream(in.strip().split(separator)).toList();
	}

}