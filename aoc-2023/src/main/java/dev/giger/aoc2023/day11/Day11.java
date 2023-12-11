package dev.giger.aoc2023.day11;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Day11 {
	public static void main(String[] args) throws IOException {
		var lines = Files.readAllLines(Path.of("aoc-2023/src/main/resources/day11/input.txt"));
		var map = new ArrayList<>(lines.stream().map(l -> new ArrayList<>(Arrays.asList(l.split("")))).toList());
		var height = map.size();
		var width = map.get(0).size();

		var galaxies = new ArrayList<Pos>();
		var rows = new ArrayList<Integer>(height);
		var cols = new ArrayList<Integer>(width);
		for (int i = 0; i < width; i++) {
			cols.add(0);
		}
		for (int y = 0; y < height; y++) {
			var currentRow = 0;
			for (int x = 0; x < width; x++) {
				if (map.get(y).get(x).equals("#")) {
					cols.set(x, cols.get(x) + 1);
					currentRow += 1;
					galaxies.add(new Pos(y, x));
				}
			}
			rows.add(currentRow);
		}

		// Find distances
		var sum1 = 0L;
		var sum2 = 0L;
		for (int i = 0; i < galaxies.size(); i++) {
			var e = galaxies.get(i);
			for (int j = i; j < galaxies.size(); j++) {
				Pos neighbor = galaxies.get(j);
				sum1 += getDistance(e, neighbor, rows, cols, 1);
				sum2 += getDistance(e, neighbor, rows, cols, 999_999);
			}
		}

		System.out.println("Part 1");
		System.out.println(sum1);

		System.out.println("Part 2");
		System.out.println(sum2);
	}

	private static long getDistance(Pos e, Pos neighbor, ArrayList<Integer> rows, ArrayList<Integer> cols, long offset) {
		var minY = Math.min(e.y, neighbor.y);
		var maxY = Math.max(e.y, neighbor.y);
		var minX = Math.min(e.x, neighbor.x);
		var maxX = Math.max(e.x, neighbor.x);

		var yDist = maxY - minY;
		for (int i = 0; i < rows.size(); i++) {
			if (rows.get(i) == 0 && i > minY && i < maxY) {
				yDist += offset;
			}
		}

		var xDist = maxX - minX;
		for (int i = 0; i < cols.size(); i++) {
			if (cols.get(i) == 0 && i > minX && i < maxX) {
				xDist += offset;
			}
		}
		return yDist + xDist;
	}

	private static String at(List<String> lines, int y, int x) {
		return String.valueOf(lines.get(y).charAt(x));
	}

	record Pos(
		long y,
		long x
	) {
	}
}