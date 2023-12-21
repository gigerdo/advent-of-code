package dev.giger.aoc2023.day21;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Day21 {

	public static void main(String[] args) throws IOException {
		var lines = Files.readAllLines(Path.of("aoc-2023/src/main/resources/day21/input.txt"));
		var map = lines.stream().map(l -> Arrays.stream(l.split("")).toList()).toList();

		Pos startPosition = null;
		for (int y = 0; y < lines.size(); y++) {
			for (int x = 0; x < lines.get(0).length(); x++) {
				if (map.get(y).get(x).equals("S")) {
					startPosition = new Pos(y, x);
				}
			}
		}

		part1(startPosition, map);
		part2(startPosition, map);
	}

	private static void part1(Pos startPosition, List<List<String>> map) {
		var queue = new ArrayList<Step>();
		queue.add(new Step(startPosition, 0));

		var visited = new HashSet<Step>();

		var maxSteps = 64;
		var result = new HashSet<Pos>();
		while (!queue.isEmpty()) {
			Step node = queue.remove(0);

			if (node.distance == maxSteps) {
				result.add(node.pos);
				continue;
			}

			var nextNodes = Stream.of(
					new Pos(node.pos.y, node.pos.x + 1),
					new Pos(node.pos.y, node.pos.x - 1),
					new Pos(node.pos.y + 1, node.pos.x),
					new Pos(node.pos.y - 1, node.pos.x)
				)
				.filter(p -> p.y >= 0 && p.y < map.size() && p.x >= 0 && p.x < map.get(0).size())
				.filter(p -> !map.get(p.y).get(p.x).equals("#"))
				.map(p -> new Step(p, node.distance + 1))
				.filter(n -> n.distance <= maxSteps)
				.filter(n -> !visited.contains(n))
				.toList();

			visited.addAll(nextNodes);
			queue.addAll(nextNodes);
		}

//		printPath(map, result);
		System.out.println("Part 1");
		System.out.println(result.size());
	}

	private static void part2(Pos startPosition, List<List<String>> map) {
		var height = map.size();

		Set<Pos> visited = new HashSet<>();
		visited.add(startPosition);

		var maxSteps = 26_501_365;

		for (int i = 1; i < maxSteps; i++) {
			visited = visited.stream()
				.flatMap(v -> {
					return Stream.of(
							new Pos(v.y, v.x + 1),
							new Pos(v.y, v.x - 1),
							new Pos(v.y + 1, v.x),
							new Pos(v.y - 1, v.x)
						)
						.filter(p -> !lookup(map, p).equals("#"));
				})
				.collect(Collectors.toSet());

			if (i % height == maxSteps % height) {
				System.out.println("Repetition");
				System.out.println("step (x): " + i);
				System.out.println("found (y): " + visited.size());
			}
			if (i == 350) {
				break;
			}
		}

		// Interpolate repetitions as a quadratic curve
		// https://math.stackexchange.com/questions/680646/get-polynomial-function-from-3-points

		// First three repetitions
		double x0 = 65;
		double y0 = 3703;
		double x1 = 196;
		double y1 = 32712;
		double x2 = 327;
		double y2 = 90559;

		double a = (x0 * (y2 - y1) + x1 * (y0 - y2) + x2 * (y1 - y0)) / ((x0 - x1) * (x0 - x2) * (x1 - x2));
		double b = (y1 - y0) / (x1 - x0) - a * (x0 + x1);
		double c = (y0 - a * Math.pow(x0, 2) - b * x0);
		var x = maxSteps;
		var result = Math.round(a * Math.pow(x, 2) + b * x + c);

		System.out.println("Part 2");
		System.out.println(result);
		System.out.println(result == 590104708070703L);
	}

	private static String lookup(List<List<String>> map, Pos p) {
		int y = p.y % map.size();
		if (y < 0) {
			y = map.size() + y;
		}
		int x = p.x % map.get(0).size();
		if (x < 0) {
			x = map.get(0).size() + x;
		}
		return map.get(y).get(x);
	}

	private static void print(List<List<String>> map, HashSet<Pos> result) {
		for (int y = 0; y < map.size(); y++) {
			for (int x = 0; x < map.get(0).size(); x++) {
				if (result.contains(new Pos(y, x))) {
					System.out.print("O");
				} else {
					System.out.print(map.get(y).get(x));
				}
			}
			System.out.println();
		}
	}

	record Step(
		Pos pos,
		int distance
	) {
	}

	record Pos(
		int y,
		int x
	) {
	}
}