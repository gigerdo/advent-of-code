package dev.giger.aoc2023.day02;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

public class Day02 {
	public static void main(String[] args) throws IOException {
		var lines = Files.readAllLines(Path.of("aoc-2023/src/main/resources/day02/input.txt"));

		// Output to calculate
		var possibleGames = new HashSet<Integer>();
		var powers = new ArrayList<Integer>();

		for (String line : lines) {
			var lineSplit = split(line, ":");

			var id = Integer.valueOf(split(lineSplit.get(0), " ").get(1));
			possibleGames.add(id);

			var maxRed = 0;
			var maxGreen = 0;
			var maxBlue = 0;
			for (String subset : split(lineSplit.get(1), ";")) {
				var red = 0;
				var green = 0;
				var blue = 0;
				for (String color : split(subset, ",")) {
					var colorSplit = split(color, " ");
					var amount = Integer.parseInt(colorSplit.get(0));
					var colorName = colorSplit.get(1);
					switch (colorName) {
						case "red" -> red += amount;
						case "green" -> green += amount;
						case "blue" -> blue += amount;
					}
				}

				// Part 1
				if (red > 12 || green > 13 || blue > 14) {
					possibleGames.remove(id);
				}

				// Part 2
				maxRed = Math.max(maxRed, red);
				maxGreen = Math.max(maxGreen, green);
				maxBlue = Math.max(maxBlue, blue);
			}
			powers.add(maxRed * maxGreen * maxBlue);
		}

		System.out.println("Part 1");
		System.out.println(possibleGames.stream().mapToInt(i -> i).sum());

		System.out.println("Part 2");
		System.out.println(powers.stream().mapToInt(i -> i).sum());
	}

	private static List<String> split(String in, String separator) {
		return Arrays.stream(in.trim().split(separator)).toList();
	}
}