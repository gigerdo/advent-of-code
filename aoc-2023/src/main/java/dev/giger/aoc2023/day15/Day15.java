package dev.giger.aoc2023.day15;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Day15 {

	public static void main(String[] args) throws IOException {
		var input = Files.readString(Path.of("aoc-2023/src/main/resources/day15/input.txt"));
		var steps = split(input, ",");

		var sum = 0L;
		var boxes = new ArrayList<List<Lens>>();
		for (int i = 0; i < 256; i++) {
			boxes.add(new ArrayList<>());
		}
		for (String step : steps) {
			if (step.contains("=")) {
				var s = split(step, "=");
				var label = s.get(0);
				var idx = Integer.valueOf(s.get(1));

				var boxId = getHash(label);
				var box = boxes.get(boxId);

				var lensId = -1;
				for (int i = 0; i < box.size(); i++) {
					if (box.get(i).label.equals(label)) {
						lensId = i;
						break;
					}
				}
				Lens lens = new Lens(label, idx);
				if (lensId > -1) {
					box.set(lensId, lens);
				} else {
					box.add(lens);
				}
			} else {
				var s = split(step, "-");
				var label = s.get(0);
				var boxId = getHash(label);
				var box = boxes.get(boxId);
				box.removeIf(l -> l.label.equals(label));
			}

			var hash = getHash(step);
			sum += hash;
		}

		var sum2 = 0;
		for (int i = 0; i < boxes.size(); i++) {
			var lenses = boxes.get(i);
			for (int l = 0; l < lenses.size(); l++) {
				sum2 += (i + 1) * (l + 1) * lenses.get(l).focalLength;
			}
		}

		System.out.println("Part 1");
		System.out.println(sum);

		System.out.println("Part 2");
		System.out.println(sum2);
	}

	private static int getHash(String step) {
		var hash = 0;
		for (int i = 0; i < step.length(); i++) {
			var c = step.charAt(i);
			hash += c;
			hash *= 17;
			hash %= 256;
		}
		return hash;
	}

	private static List<String> split(String in, String separator) {
		return Arrays.stream(in.strip().split(separator)).toList();
	}

	record Lens(
		String label,
		Integer focalLength
	) {
	}
}