package dev.giger.aoc2023.day18;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;

public class Day18 {

	public static void main(String[] args) throws IOException {
		var lines = Files.readAllLines(Path.of("aoc-2023/src/main/resources/day18/input.txt"));

		var width = 500;
		var height = 400;
		var map = new ArrayList<ArrayList<String>>(height);
		for (int i = 0; i < height; i++) {
			var row = new ArrayList<String>(width);
			for (int i1 = 0; i1 < width; i1++) {
				row.add(".");
			}
			map.add(row);
		}

		Pos startPos = new Pos(130, 50);
		var pos = startPos;
		var previousDir = "";
		for (String line : lines) {
			String[] s = line.split(" ");
			var direction = s[0];
			var distance = Integer.parseInt(s[1]);

			var dir = new Pos(0, 0);
			var c = "#";
			if (direction.equals("R")) {
				dir = new Pos(0, 1);
			} else if (direction.equals("D")) {
				dir = new Pos(1, 0);
				c = "|";
			} else if (direction.equals("L")) {
				dir = new Pos(0, -1);
			} else if (direction.equals("U")) {
				dir = new Pos(-1, 0);
				c = "|";
			}

			if (
				previousDir.equals("R") && direction.equals("D")
					|| previousDir.equals("U") && direction.equals("R")
					|| previousDir.equals("L") && direction.equals("D")
					|| previousDir.equals("U") && direction.equals("L")
			) {
				map.get(pos.y).set(pos.x, "|");
			}
			for (var i = 0; i < distance; i++) {
				pos = new Pos(pos.y + dir.y, pos.x + dir.x);

				if (i == distance - 1) {
					map.get(pos.y).set(pos.x, "#");
				} else {
					map.get(pos.y).set(pos.x, c);
				}
			}
			previousDir = direction;
		}
		map.get(startPos.y).set(startPos.x, "|");

		var totalInside = 0;
		for (int i = 0; i < height; i++) {
			var inside = false;
			for (int x = 0; x < width; x++) {
				String s = map.get(i).get(x);
				if (s.equals("|")) {
					inside = !inside;
					totalInside += 1;
					System.out.print(s);
				} else if (s.equals("#")) {
					totalInside += 1;
					System.out.print(s);
				} else if (inside) {
					totalInside += 1;
					System.out.print("@");
				} else {
					System.out.print(s);
				}
			}
			System.out.println();
		}

		System.out.println(totalInside);
	}

	record Pos(
		int y,
		int x
	) {
	}
}