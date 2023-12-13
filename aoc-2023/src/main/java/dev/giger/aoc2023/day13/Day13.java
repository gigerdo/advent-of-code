package dev.giger.aoc2023.day13;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;

public class Day13 {

	public static void main(String[] args) throws IOException {
		var input = Files.readString(Path.of("aoc-2023/src/main/resources/day13/input.txt"));

		var sum1 = 0L;
		var sum2 = 0L;
		for (String block : split(input, "\n\n")) {
			var lines = split(block, "\n");

			// Rows
			for (int i = 1; i < lines.size(); i++) {
				var mirrorDistance = 0;
				for (int j = 0; j < i; j++) {
					int aboveIdx = i - j - 1;
					int belowIdx = i + j;
					if (belowIdx >= lines.size()) {
						break;
					}
					mirrorDistance += calcDistance(lines.get(belowIdx), lines.get(aboveIdx));
				}
				if (mirrorDistance == 0) {
					sum1 += i * 100L;
				}
				if (mirrorDistance == 1) {
					sum2 += i * 100L;
				}
			}

			// Cols
			int width = lines.get(0).length();
			for (int c = 1; c < width; c++) {
				var mirrorDistance = 0;
				for (int k = 0; k < c; k++) {
					int beforeIdx = c - k - 1;
					int afterIdx = c + k;
					if (afterIdx >= width) {
						break;
					}
					String before = getColumn(lines, beforeIdx);
					String after = getColumn(lines, afterIdx);
					mirrorDistance += calcDistance(before, after);
				}
				if (mirrorDistance == 0) {
					sum1 += c;
				}
				if (mirrorDistance == 1) {
					sum2 += c;
				}
			}
		}
		System.out.println("Part 1");
		System.out.println(sum1);

		System.out.println("Part 2");
		System.out.println(sum2);
	}

	private static int calcDistance(String a, String b) {
		var total = 0;
		if (a.length() != b.length()) {
			throw new RuntimeException();
		}
		for (int i = 0; i < a.length(); i++) {
			if (a.charAt(i) != b.charAt(i)) {
				total += 1;
			}
		}
		return total;
	}

	private static String getColumn(List<String> lines, int idx) {
		var builder = new StringBuilder();
		for (String line : lines) {
			builder.append(line.charAt(idx));
		}
		return builder.toString();
	}

	private static List<String> split(String in, String separator) {
		return Arrays.stream(in.strip().split(separator)).toList();
	}

}