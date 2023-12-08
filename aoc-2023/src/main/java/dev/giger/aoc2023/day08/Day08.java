package dev.giger.aoc2023.day08;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.function.Function;

public class Day08 {
	public static void main(String[] args) throws IOException {
		var lines = Files.readAllLines(Path.of("aoc-2023/src/main/resources/day08/input.txt"));
		var steps = split(lines.get(0), "");

		var graph = new HashMap<String, List<String>>();
		for (String line : lines.subList(2, lines.size())) {
			var lineSplit = split(line, "(\\s=\\s\\(|,\\s|\\))");
			var source = lineSplit.get(0);
			var left = lineSplit.get(1);
			var right = lineSplit.get(2);

			graph.put(source, List.of(left, right));
		}

		{
			var stepCounter = countSteps("AAA", graph, steps, p -> p.equals("ZZZ"));
			System.out.println("Part 1");
			System.out.println(stepCounter);
		}

		{
			var result = graph.keySet().stream()
				.filter(k -> k.endsWith("A"))
				.map(p -> countSteps(p, graph, steps, q -> q.endsWith("Z")))
				.mapToLong(i -> i)
				.reduce(1L, Day08::lcm);
			System.out.println("Part 2");
			System.out.println(result);
		}
	}

	private static int countSteps(String startPosition, HashMap<String, List<String>> graph, List<String> steps, Function<String, Boolean> foundTarget) {
		var currentPosition = startPosition;
		var stepCounter = 0;
		while (!foundTarget.apply(currentPosition)) {
			var nextPositions = graph.get(currentPosition);

			var step = steps.get(stepCounter % steps.size());
			stepCounter += 1;

			if (step.equals("L")) {
				currentPosition = nextPositions.get(0);
			} else if (step.equals("R")) {
				currentPosition = nextPositions.get(1);
			} else {
				throw new RuntimeException();
			}
		}
		return stepCounter;
	}

	public static long lcm(long number1, long number2) {
		if (number1 == 0 || number2 == 0)
			return 0;
		else {
			long gcd = gcd(number1, number2);
			return Math.abs(number1 * number2) / gcd;
		}
	}

	public static long gcd(long number1, long number2) {
		if (number1 == 0 || number2 == 0) {
			return number1 + number2;
		} else {
			long absNumber1 = Math.abs(number1);
			long absNumber2 = Math.abs(number2);
			long biggerValue = Math.max(absNumber1, absNumber2);
			long smallerValue = Math.min(absNumber1, absNumber2);
			return gcd(biggerValue % smallerValue, smallerValue);
		}
	}

	private static List<String> split(String in, String separator) {
		return Arrays.stream(in.strip().split(separator)).toList();
	}
}