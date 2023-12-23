package dev.giger.aoc2023.day23;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Stream;

public class Day23 {
	public static void main(String[] args) throws IOException {
		var lines = Files.readAllLines(Path.of("aoc-2023/src/main/resources/day23/input.txt"));

		var start = new Pos(0, 1);
		var end = new Pos(lines.size() - 1, lines.get(0).length() - 2);

		part1(start, end, lines);

		part2(start, end, lines);
	}

	private static void part1(Pos start, Pos end, List<String> lines) {
		var longestDistance = new HashMap<Pos, Step>();
		var queue = new PriorityQueue<Step>(Comparator.comparingInt(s -> -s.distance));
		Step startStep = new Step(start, new Pos(1, 0), 0, List.of());
		queue.add(startStep);
		while (!queue.isEmpty()) {
			Step step = queue.remove();

			if (step.distance < longestDistance.getOrDefault(step.p, startStep).distance) {
				continue;
			}

			var nextSteps = Stream.of(
					new Pos(0, 1),
					new Pos(0, -1),
					new Pos(1, 0),
					new Pos(-1, 0)
				)
				.map(d -> move(step, d))
				.filter(s -> canMove(s, lines, false))
				.filter(s -> s.distance > longestDistance.getOrDefault(s.p, startStep).distance)
				.toList();
			for (Step nextStep : nextSteps) {
				longestDistance.put(nextStep.p, nextStep);
			}
			queue.addAll(nextSteps);
		}

		System.out.println("Part 1");
		System.out.println(longestDistance.get(end).distance);
	}

	private static void part2(Pos start, Pos end, List<String> lines) {
		var newGraph = new HashMap<Pos, Set<Edge>>();
		newGraph.put(start, new HashSet<>());
		newGraph.put(end, new HashSet<>());

		var visited = new HashSet<Pos>();
		var queue = new ArrayList<Step>();
		// direction is misused as last vertex pos
		queue.add(new Step(start, start, 0, List.of()));
		while (!queue.isEmpty()) {
			Step remove = queue.remove(0);
			if (visited.contains(remove.p)) {
				continue;
			}

			var newStart = new Step(remove.p, remove.p, 0, List.of());
			var nextSteps = nextSteps(lines, newStart);
			for (Step nextStep : nextSteps) {
				var n = nextSteps(lines, nextStep);
				while (true) {
					var nn = nextSteps(lines, n.get(0));
					if (nn.size() > 1) {
						break;
					}
					if (nn.size() == 0) {
						break;
					}
					n = nn;
				}
				for (Step step : n) {
					var a = step.d;
					var b = step.p;

					newGraph.putIfAbsent(a, new HashSet<>());
					newGraph.putIfAbsent(b, new HashSet<>());
					newGraph.get(a).add(new Edge(b, step.distance));
					newGraph.get(b).add(new Edge(a, step.distance));

					queue.add(step);
				}
			}

			visited.add(remove.p);
		}

//		newGraph.entrySet().forEach(System.out::println);

		var queue2 = new PriorityQueue<Step>(Comparator.comparing(i -> -i.distance));
		queue2.add(new Step(start, null, 0, List.of()));
		var max = 0;
		while (!queue2.isEmpty()) {
			Step remove = queue2.remove();

			if (remove.p.equals(end)) {
				if (remove.distance > max) {
					max = remove.distance;
				}
				continue;
			}

			var next = newGraph.get(remove.p).stream()
				.filter(e -> !remove.path.contains(e.target))
				.map(e -> new Step(
					e.target,
					null,
					remove.distance + e.distance,
					append(remove.path, remove.p))
				)
				.toList();
			queue2.addAll(next);
		}

		System.out.println("Part 2");
		System.out.println(max);
	}

	private static List<Step> nextSteps(List<String> lines, Step remove) {
		return Stream.of(
				new Pos(0, 1),
				new Pos(0, -1),
				new Pos(1, 0),
				new Pos(-1, 0)
			)
			.map(d -> move(remove, d))
			.map(s -> new Step(s.p, remove.d, s.distance, s.path))
			.filter(s -> canMove(s, lines, true))
			.toList();
	}

	private static List<Pos> append(List<Pos> path, Pos target) {
		ArrayList<Pos> newPath = new ArrayList<>(path);
		newPath.add(target);
		return newPath;
	}

	private static boolean canMove(Step step, List<String> lines, boolean climbSlopes) {
		if (step.path.contains(step.p)) {
			return false;
		}

		if (step.p.y < 0) return false;
		if (step.p.y >= lines.size()) return false;
		var c = lines.get(step.p.y).charAt(step.p.x);

		if (c == '#') return false;
		if (c == '.') return true;

		if (climbSlopes) return true;
		if (c == '>' && step.d.equals(new Pos(0, 1))) return true;
		if (c == '<' && step.d.equals(new Pos(0, -1))) return true;
		if (c == '^' && step.d.equals(new Pos(-1, 0))) return true;
		if (c == 'v' && step.d.equals(new Pos(1, 0))) return true;

		return false;
	}

	private static Step move(Step step, Pos d) {
		var newPath = new ArrayList<>(step.path);
		newPath.add(step.p);
		return new Step(
			new Pos(step.p.y + d.y, step.p.x + d.x),
			d,
			step.distance + 1,
			newPath
		);
	}

	record Step(
		Pos p,
		Pos d,
		int distance,
		List<Pos> path
	) {
	}

	record Pos(
		int y,
		int x
	) {
	}

	record Edge(
		Pos target,
		int distance
	) {
	}
}
