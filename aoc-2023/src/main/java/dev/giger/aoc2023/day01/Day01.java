package dev.giger.aoc2023.day01;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public class Day01 {
	public static void main(String[] args) throws IOException {
		var codes = Files.readAllLines(Path.of("aoc-2023/src/main/resources/day01/input.txt"));

		part1(codes);

		part2(codes);
	}

	private static void part1(List<String> codes) {
		System.out.println("Part 1");
		var result = codes.stream()
			.map(c -> {
				var digits = c.chars()
					.filter(i -> i >= '0' && i <= '9')
					.mapToObj(i -> String.valueOf((char) i))
					.toArray(String[]::new);
				return digits[0] + digits[digits.length - 1];
			})
			.mapToInt(Integer::valueOf)
			.sum();
		System.out.println(result);
	}

	private static void part2(List<String> codes) {
		System.out.println("Part 2");
		var numbers = List.of("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine");
		var digits = List.of("0", "1", "2", "3", "4", "5", "6", "7", "8", "9");
		var result = codes.stream()
			.map(c -> {
				var allNumbers = new ArrayList<String>();
				for (int i = 0; i < c.length(); i++) {
					var remaining = c.substring(i);
					var isNumber = numbers.stream().filter(remaining::startsWith).findAny();
					isNumber.map(n -> allNumbers.add(String.valueOf(numbers.indexOf(n))));
					var isDigit = digits.stream().filter(remaining::startsWith).findAny();
					isDigit.map(allNumbers::add);
				}
				return allNumbers.get(0) + allNumbers.get(allNumbers.size() - 1);
			})
			.mapToInt(Integer::valueOf)
			.sum();
		System.out.println(result);
	}
}
