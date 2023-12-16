package dev.giger.aoc2023.day16;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Day16 {

	public static void main(String[] args) throws IOException {
		var lines = Files.readAllLines(Path.of("aoc-2023/src/main/resources/day16/input.txt"));
		var height = lines.size();
		var width = lines.get(0).length();

		var startRays = new ArrayList<Ray>();
		for (int i = 0; i < height; i++) {
			startRays.add(new Ray(i, 0, 0, 1));
			startRays.add(new Ray(i, width - 1, 0, -1));
		}
		for (int i = 0; i < width; i++) {
			startRays.add(new Ray(0, i, 1, 0));
			startRays.add(new Ray(height - 1, i, -1, 0));
		}

		var part1 = 0L;
		var part2 = 0L;
		for (Ray startRay : startRays) {
			var map = lines.stream()
				.map(l ->
					IntStream.range(0, l.length())
						.mapToObj(i -> new ArrayList<Ray>())
						.collect(Collectors.toCollection(ArrayList::new))
				)
				.collect(Collectors.toCollection(ArrayList::new));

			var rays = new ArrayList<Ray>();
			rays.add(startRay);
			while (!rays.isEmpty()) {
				var ray = rays.remove(0);
				while (true) {
					if (ray.x < 0 || ray.x >= width || ray.y < 0 || ray.y >= height) {
						break;
					}
					var currentRays = map.get(ray.y).get(ray.x);
					if (currentRays.contains(ray)) {
						break;
					}
					currentRays.add(ray);
					var tile = lines.get(ray.y).charAt(ray.x);
					if (tile == '|' && ray.dirY == 0) {
						ray = new Ray(ray.y - 1, ray.x, -1, 0);
						rays.add(new Ray(ray.y + 1, ray.x, 1, 0));
					} else if (tile == '-' && ray.dirX == 0) {
						ray = new Ray(ray.y, ray.x - 1, 0, -1);
						rays.add(new Ray(ray.y, ray.x + 1, 0, 1));
					} else {
						var dirY = ray.dirY;
						var dirX = ray.dirX;
						if (tile == '/') {
							if (ray.dirY == -1) {
								dirY = 0;
								dirX = 1;
							} else if (ray.dirY == 1) {
								dirY = 0;
								dirX = -1;
							} else if (ray.dirX == 1) {
								dirY = -1;
								dirX = 0;
							} else if (ray.dirX == -1) {
								dirY = 1;
								dirX = 0;
							}
						} else if (tile == '\\') {
							if (ray.dirY == -1) {
								dirY = 0;
								dirX = -1;
							} else if (ray.dirY == 1) {
								dirY = 0;
								dirX = 1;
							} else if (ray.dirX == 1) {
								dirY = 1;
								dirX = 0;
							} else if (ray.dirX == -1) {
								dirY = -1;
								dirX = 0;
							}
						}

						ray = new Ray(ray.y + dirY, ray.x + dirX, dirY, dirX);
					}
				}
			}

			if (false) {
				// Print map
				for (int y = 0; y < map.size(); y++) {
					for (int x = 0; x < map.get(0).size(); x++) {
						System.out.print(map.get(y).get(x).size());
					}
					System.out.println();
				}
			}

			var result = map.stream().flatMap(Collection::stream).filter(n -> n.size() > 0).count();
			if (startRay.equals(new Ray(0, 0, 0, 1))) {
				part1 = result;
			}
			if (result > part2) {
				part2 = result;
			}
		}

		System.out.println("Part 1");
		System.out.println(part1);

		System.out.println("Part 2");
		System.out.println(part2);
	}

	record Ray(
		int y,
		int x,
		int dirY,
		int dirX

	) {
	}
}