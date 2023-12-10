package dev.giger.aoc2023.day10;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.stream.Stream;

public class Day10 {
	public static void main(String[] args) throws IOException {
		var lines = Files.readAllLines(Path.of("aoc-2023/src/main/resources/day10/input.txt"));

		var height = lines.size();
		var width = lines.get(0).length();


		Pos startPos = getStartPos(lines);

		var previousStep = startPos;
		var nextStep = new Pos(startPos.y - 1, startPos.x, -1, 0);
		var counter = 1;
		var path = new ArrayList<Pos>();
		path.add(previousStep);

		while (!nextStep.equals(startPos)) {
			path.add(nextStep);
			System.out.println(previousStep + "(" + at(lines, previousStep) + ") -> " + nextStep + "(" + at(lines, nextStep) + ")");
			var pst = previousStep;
			var nst = nextStep;
			var neighbours = findNeighbours(lines, nextStep);
			nextStep = neighbours.stream()
				.filter(n -> !n.equals(pst))
				.findFirst()
				.orElseThrow();
			previousStep = nst;
			counter += 1;
		}

		System.out.println("Part 1");
		System.out.println(counter / 2);

		var insideBlocks = new HashSet<Pos>();
		for (Pos pos : path) {
			var inside = getInside(lines, pos).stream()
				.filter(i -> !path.contains(i))
				.toList();
			insideBlocks.addAll(inside);
		}

		var visited = new HashSet<Pos>();
		var queue = new ArrayList<Pos>();
		queue.addAll(insideBlocks);
		while (!queue.isEmpty()) {
			var e = queue.remove(0);
			visited.add(e);
			insideBlocks.add(e);
			var m = Stream.of(
					new Pos(e.y - 1, e.x, 0, 0),
					new Pos(e.y + 1, e.x, 0, 0),
					new Pos(e.y, e.x - 1, 0, 0),
					new Pos(e.y, e.x + 1, 0, 0)
				)
				.filter(p -> !path.contains(p))
				.filter(p -> !visited.contains(p))
				.toList();
			queue.addAll(m);
		}

		for (int y = 0; y < height; y++) {
			for (int x = 0; x < width; x++) {
				Pos p = new Pos(y, x, 0, 0);
				String at = at(lines, p);
				if (path.contains(p)) {
					switch (at) {
						case "L" -> {
							System.out.print("┗");
						}
						case "7" -> {
							System.out.print("┓");
						}
						case "J" -> {
							System.out.print("┛");
						}
						case "F" -> {
							System.out.print("┏");
						}
						case "|" -> {
							System.out.print("┃");
						}
						case "-" -> {
							System.out.print("━");
						}
						default -> {
							System.out.print(at);
						}
					}
				} else if (insideBlocks.contains(p)) {
					System.out.print("█");
				} else {
					System.out.print(".");
				}
			}
			System.out.println();
		}

		System.out.println("Part 2");
		System.out.println(insideBlocks.size());

	}

	private static List<Pos> getInside(List<String> lines, Pos pos) {
		var pipe = at(lines, pos);
		pipe = pipe.replace("S", "L");
		// | - L J 7 F
		switch (pipe) {
			case "|" -> {
				if (pos.dirY == 1) { //Down
					return List.of(new Pos(pos.y, pos.x - 1, 0, 0));
				} else { //Up
					return List.of(new Pos(pos.y, pos.x + 1, 0, 0));
				}
			}
			case "-" -> {
				if (pos.dirX == 1) { //Right
					return List.of(new Pos(pos.y + 1, pos.x, 0, 0));
				} else { //Left
					return List.of(new Pos(pos.y - 1, pos.x, 0, 0));
				}
			}
			case "L" -> {
				if (pos.dirX == -1) { // up
					return List.of();
				} else { // right
					return List.of(
						new Pos(pos.y + 1, pos.x, 0, 0),
						new Pos(pos.y, pos.x - 1, 0, 0)
					);
				}
			}
			case "J" -> {
				if (pos.dirX == 1) { // up
					return List.of(
						new Pos(pos.y + 1, pos.x, 0, 0),
						new Pos(pos.y, pos.x + 1, 0, 0)
					);
				} else { // left
					return List.of();
				}
			}
			case "7" -> {
				if (pos.dirY == -1) { // left
					return List.of(
						new Pos(pos.y - 1, pos.x, 0, 0),
						new Pos(pos.y, pos.x + 1, 0, 0)
					);
				} else { // down
					return List.of();
				}
			}
			case "F" -> {
				if (pos.dirY == -1) { // right
					return List.of();
				} else { // down
					return List.of(
						new Pos(pos.y - 1, pos.x, 0, 0),
						new Pos(pos.y, pos.x - 1, 0, 0)
					);
				}
			}
		}
		throw new RuntimeException();
	}

	private static Pos getStartPos(List<String> lines) {
		for (int y = 0; y < lines.size(); y++) {
			var sPos = lines.get(y).indexOf("S");
			if (sPos != -1) {
				return new Pos(y, sPos, 0, -1);
			}
		}
		throw new RuntimeException();
	}

	private static List<Pos> findNeighbours(List<String> lines, Pos p) {
		var pipe = at(lines, p);
		// | - L J 7 F . S
		switch (pipe) {
			case "|" -> {
				return List.of(
					new Pos(p.y - 1, p.x, -1, 0),
					new Pos(p.y + 1, p.x, 1, 0)
				);
			}
			case "-" -> {
				return List.of(
					new Pos(p.y, p.x - 1, 0, -1),
					new Pos(p.y, p.x + 1, 0, 1)
				);
			}
			case "L" -> {
				return List.of(
					new Pos(p.y - 1, p.x, -1, 0),
					new Pos(p.y, p.x + 1, 0, 1)
				);
			}
			case "J" -> {
				return List.of(
					new Pos(p.y - 1, p.x, -1, 0),
					new Pos(p.y, p.x - 1, 0, -1)
				);
			}
			case "7" -> {
				return List.of(
					new Pos(p.y, p.x - 1, 0, -1),
					new Pos(p.y + 1, p.x, 1, 0)
				);
			}
			case "F" -> {
				return List.of(
					new Pos(p.y + 1, p.x, 1, 0),
					new Pos(p.y, p.x + 1, 0, 1)
				);
			}
			case ".", "S" -> {
				return List.of();
			}
		}
		throw new RuntimeException();
	}

	private static String at(List<String> lines, Pos p) {
		return String.valueOf(lines.get(p.y).charAt(p.x));
	}

	record Pos(
		int y,
		int x,
		int dirY,
		int dirX
	) {
		@Override
		public boolean equals(Object o) {
			if (this == o) return true;
			if (o == null || getClass() != o.getClass()) return false;

			Pos pos = (Pos) o;

			if (y != pos.y) return false;
			return x == pos.x;
		}

		@Override
		public int hashCode() {
			int result = y;
			result = 31 * result + x;
			return result;
		}
	}
}