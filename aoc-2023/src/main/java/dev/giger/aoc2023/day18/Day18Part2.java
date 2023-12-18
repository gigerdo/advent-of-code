package dev.giger.aoc2023.day18;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;

public class Day18Part2 {

	public static void main(String[] args) throws IOException {
		var lines = Files.readAllLines(Path.of("aoc-2023/src/main/resources/day18/input.txt"));

		var paths = new ArrayList<Line>();


		var minX = 0L;
		var maxX = 0L;
		var minY = 0L;
		var maxY = 0L;

		Pos startPos = new Pos(0, 0);
		var pos = startPos;
		var previousDir = "U";
		for (String line : lines) {
			if (pos.y < minY) {
				minY = pos.y;
			}
			if (pos.y > maxY) {
				maxY = pos.y;
			}
			if (pos.x < minX) {
				minX = pos.x;
			}
			if (pos.x > maxX) {
				maxX = pos.x;
			}

			String[] s = line.split(" ");
			var color = s[2];
			var distance = Integer.parseInt(color.substring(2, 7), 16);

			System.out.println(distance);
			var directionEncoded = color.substring(7, 8);
			var direction = "";
			switch (directionEncoded) {
				case "0" -> direction = "R";
				case "1" -> direction = "D";
				case "2" -> direction = "L";
				case "3" -> direction = "U";
			}

			var dir = new Pos(0, 0);
			if (direction.equals("R")) {
				dir = new Pos(0, 1);
			} else if (direction.equals("D")) {
				dir = new Pos(1, 0);
			} else if (direction.equals("L")) {
				dir = new Pos(0, -1);
			} else if (direction.equals("U")) {
				dir = new Pos(-1, 0);
			}

			var end = new Pos(
				pos.y + distance * dir.y,
				pos.x + distance * dir.x
			);
			var flipsInside = direction.equals("U") || direction.equals("D");
			if (
				previousDir.equals("R") && direction.equals("D")
					|| previousDir.equals("L") && direction.equals("D")
					|| previousDir.equals("D") && direction.equals("R")
					|| previousDir.equals("D") && direction.equals("L")
			) {
				var beginning = pos;
				int index = paths.size() - 1;
				var l = paths.get(index);
				paths.set(index, l.removeOne());
				paths.add(new Line(beginning, end, dir, distance + 1, flipsInside));
			} else {
				var beginning = new Pos(pos.y + dir.y, pos.x + dir.x);
				paths.add(new Line(beginning, end, dir, distance, flipsInside));
			}
			pos = end;
			previousDir = direction;
		}
		System.out.println(minY + "-" + maxY);
		System.out.println(minX + "-" + maxX);

		var totalInside = 0L;
		for (long i = minY; i <= maxY; i++) {
			var y = i;
			var sp = paths.stream()
				.filter(p -> p.onY(y))
				.sorted(Comparator.comparingInt(Line::minX))
				.toList();

			var inside = false;
			var previousEnd = 0;
			for (Line line : sp) {
				if (inside) {
					if (line.flipsInside) {
						inside = false;
						totalInside += line.maxX() - previousEnd - 1;
					}
					if (line.horizontal()) {
						// Remove all horizontal lines from the inside, as the path is counted again later
						// to avoid counting it twice
						totalInside -= line.length();
					}
				} else {
					if (line.flipsInside) {
						inside = true;
						previousEnd = line.maxX();
					}
				}
			}
		}

		// Add path itself
		for (Line path : paths) {
			totalInside += path.length();
		}

		System.out.println(totalInside);
	}

	record Pos(
		int y,
		int x
	) {
	}

	record Line(
		Pos start,
		Pos end,
		Pos dir,
		long distance,
		boolean flipsInside
	) {
		public boolean onY(long y) {
			if (start.y == end.y && start.y == y) {
				return true;
			}
			if (start.y < end.y && start.y <= y && end.y >= y) {
				return true;
			}
			if (start.y > end.y && start.y >= y && end.y <= y) {
				return true;
			}

			return false;
		}

		public int minX() {
			return Math.min(start.x, end.x);
		}

		public int maxX() {
			return Math.max(start.x, end.x);
		}

		public boolean horizontal() {
			return start.x != end.x;
		}

		public Line removeOne() {
			return new Line(start,
				new Pos(end.y - dir.y, end.x - dir.x),
				dir,
				distance - 1,
				flipsInside
			);
		}

		public long length() {
			int d = Math.abs(end.x - start.x) + Math.abs(end.y - start.y) + 1;
			if (d != distance) {
				throw new RuntimeException();
			}
			return d;
		}
	}
}