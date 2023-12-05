package dev.giger.aoc2023.day05;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;

public class Day05 {
	public static void main(String[] args) throws IOException {
		var input = Files.readString(Path.of("aoc-2023/src/main/resources/day05/input.txt"));
		var seedsSplit = split(input, "(seeds:|seed-to-soil map:)");
		var seeds = split(seedsSplit.get(1), " ").stream().map(Long::valueOf).toList();

		var mapsSplit = split(seedsSplit.get(2), "(soil-to-fertilizer map:|fertilizer-to-water map:|water-to-light map:|light-to-temperature map:|temperature-to-humidity map:|humidity-to-location map:)");
		var maps = mapsSplit.stream()
			.map(m -> {
				return split(m, "\n").stream()
					.map(l -> {
						var values = split(l, " ").stream().map(Long::valueOf).toList();
						return new MapEntry(
							values.get(0),
							values.get(1),
							values.get(2)
						);
					})
					.toList();
			})
			.toList();

		// Part 1
		var min = Long.MAX_VALUE;
		for (Long seed : seeds) {
			var currentMapping = seed;
			mapLoop:
			for (List<MapEntry> map : maps) {
				for (MapEntry mapEntry : map) {
					if (currentMapping >= mapEntry.source
						&& currentMapping < mapEntry.source + mapEntry.length
					) {
						currentMapping = currentMapping - mapEntry.source + mapEntry.destination;
						continue mapLoop;
					}
				}
			}

			if (currentMapping < min) {
				min = currentMapping;
			}

		}
		System.out.println("Part 1");
		System.out.println(min);


		// Part 2
		var minRange = Long.MAX_VALUE;
		for (int i = 0; i < seeds.size() / 2; i++) {
			var start = seeds.get(2 * i);
			var length = seeds.get(2 * i + 1);
			var currentMapping = List.of(new Range(start, length, false));
			for (List<MapEntry> map : maps) {
				// Mark all entries as unmapped before applying the next map
				currentMapping = currentMapping.stream().map(Range::unmapped).toList();
				// Apply all mappings, the mapped-flag ensures every range is only mapped once
				for (MapEntry mapEntry : map) {
					currentMapping = currentMapping.stream()
						.flatMap(sourceRange -> sourceRange.map(mapEntry).stream())
						.toList();
				}
			}

			for (Range r : currentMapping) {
				if (r.start < minRange) {
					minRange = r.start;
				}
			}
		}

		System.out.println("Part 2");
		System.out.println(minRange);
	}

	private static List<String> split(String in, String separator) {
		return Arrays.stream(in.strip().split(separator)).toList();
	}

	record MapEntry(
		long destination,
		long source,
		long length
	) {
		Range getSourceRange() {
			return new Range(source, length, false);
		}

		Range getDestinationRange() {
			return new Range(destination, length, false);
		}
	}

	record Range(
		long start,
		long length,
		boolean mapped
	) {
		long end() {
			return start + length;
		}

		public List<Range> map(MapEntry mapEntry) {
			var source = mapEntry.getSourceRange();
			var destination = mapEntry.getDestinationRange();

			// if already mapped, don't apply any more mappings
			if (this.mapped) {
				return List.of(this);
			}

			if (end() <= source.start() || start() >= source.end()) {
				return List.of(this);
			}
			// Full overlap
			if (start() < source.start() && end() > source.end()) {
				return List.of(
					new Range(
						start(),
						source.start() - start(),
						false
					),
					new Range(
						destination.start,
						destination.length,
						true
					),
					new Range(
						source.end(),
						end() - source.end(),
						false
					)
				);
			}

			// Fully inside
			if (start() >= source.start() && end() <= source.end()) {
				return List.of(
					new Range(
						start() - source.start() + destination.start(),
						length(),
						true
					)
				);
			}

			// Partial overlap at the end
			if (start() >= source.start() && end() > source.end()) {
				return List.of(
					new Range(
						start() - source.start() + destination.start(),
						source.end() - start(),
						true
					),
					new Range(
						source.end(),
						end() - source.end(),
						false
					)
				);
			}

			// Partial overlap at the start
			if (start() < source.start() && end() > source.start() && end() <= source.end()) {
				return List.of(
					new Range(
						start(),
						source.start() - start(),
						false
					),
					new Range(
						destination.start(),
						end() - source.start(),
						true
					)
				);
			}

			// Safety check in case I made any mistakes above :)
			throw new RuntimeException("Missed case");
		}

		public Range unmapped() {
			return new Range(
				start,
				length,
				false
			);
		}
	}

}