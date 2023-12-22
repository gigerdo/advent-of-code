package dev.giger.aoc2023.day22;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

public class Day22 {

	public static void main(String[] args) throws IOException {
		testIntersectionCode();

		var lines = Files.readAllLines(Path.of("aoc-2023/src/main/resources/day22/input.txt"));

		var id = 0;
		var bricks = new ArrayList<Brick>();
		for (String l : lines) {
			List<String> split = split(l, "~");
			Brick brick = new Brick(
				"b" + id++,
				parse(split.get(0)),
				parse(split.get(1))
			);
			bricks.add(brick);
		}

		var supportedBy = new HashMap<String, List<Brick>>();
		settleDown(bricks, supportedBy);

		var brickMap = bricks.stream().collect(Collectors.toMap(i -> i.id, i -> i));
		var supports = bricks.stream().collect(Collectors.toMap(i -> i.id, i -> new ArrayList<Brick>()));

		// Filter out actually supported bricks (Remove supporting bricks that don't touch the brick)
		for (var e : supportedBy.entrySet()) {
			var brick = brickMap.get(e.getKey());
			var supportingBricks = e.getValue().stream()
				.filter(b -> maxZ(b) + 1 == minZ(brick))
				.toList();
			supportedBy.put(e.getKey(), supportingBricks);
			supportingBricks.forEach(b -> supports.get(b.id).add(brick));
		}

		var disintegrateable = supports.values().stream()
			.filter(
				// Brick doesn't support any other brick
				s -> s.size() == 0
					// Any brick supported by this brick, is also supported by at least one other brick
					|| s.stream().allMatch(sb -> supportedBy.getOrDefault(sb.id, List.of()).size() >= 2)
			)
			.count();

		System.out.println("Part 1");
//		bricks.stream().sorted(Comparator.comparingLong(b -> -b.a.z)).forEach(System.out::println);
//		System.out.println("\nSupported by");
//		supportedBy.forEach((a, b) -> System.out.println(a + ": " + b));
//		System.out.println("\nSupports:");
//		supports.forEach((a, b) -> System.out.println(a + ": " + b));
		System.out.println(disintegrateable);

		System.out.println("Part 2");
		var total = 0L;
		for (Brick brick : bricks.stream().sorted(Comparator.comparingLong(b -> -maxZ(b))).toList()) {
			var removedBricks = new HashSet<String>();
			var queue = new ArrayList<Brick>();
			queue.add(brick);
			removedBricks.add(brick.id);

			while (!queue.isEmpty()) {
				Brick remove = queue.remove(0);
				ArrayList<Brick> supportedBricks = supports.get(remove.id);
				var supportedBricksToFall = supportedBricks.stream()
					.filter(sb ->
						supportedBy.getOrDefault(sb.id, List.of()).stream()
							// Filter out already removed bricks
							.filter(sbb -> !removedBricks.contains(sbb.id))
							.count() == 0 // If no supporting bricks are left, remove this brick as well
					)
					.toList();
				queue.addAll(supportedBricksToFall);
				removedBricks.addAll(supportedBricksToFall.stream().map(b -> b.id).toList());
			}
			total += removedBricks.size() - 1;
		}
		System.out.println(total);
	}

	private static void settleDown(ArrayList<Brick> bricks, HashMap<String, List<Brick>> supportedBy) {
		bricks.sort(Comparator.comparing(Day22::minZ));
		for (int i = 0; i < bricks.size(); i++) {
			var brick = bricks.get(i);

			var blockingBricks = new ArrayList<Brick>();
			for (int i1 = 0; i1 < i; i1++) {
				var anotherBrick = bricks.get(i1);
				if (intersectXY(brick, anotherBrick)) {
					blockingBricks.add(anotherBrick);
				}
			}
			var highestBlockingBrick = blockingBricks.stream()
				.map(Day22::maxZ)
				.reduce(0L, Math::max);
			var newZ = highestBlockingBrick + 1;
			bricks.set(i, brick.movedToZ(newZ));
			supportedBy.put(
				brick.id,
				blockingBricks
			);
		}
	}

	private static long maxZ(Brick anotherBrick) {
		return Math.max(anotherBrick.a.z, anotherBrick.b.z);
	}

	private static long minZ(Brick brick) {
		return Math.min(brick.a.z, brick.b.z);
	}

	private static boolean intersectXY(Brick brick, Brick anotherBrick) {
		if (brick.column()) {
			return onBrick(brick.a, anotherBrick);
		} else if (brick.horizontal()) {
			for (long i = Math.min(brick.a.x, brick.b.x); i <= Math.max(brick.a.x, brick.b.x); i++) {
				if (onBrick(new Pos(i, brick.a.y, 0), anotherBrick)) {
					return true;
				}
			}
			return false;
		} else if (brick.vertical()) {
			for (long i = Math.min(brick.a.y, brick.b.y); i <= Math.max(brick.a.y, brick.b.y); i++) {
				if (onBrick(new Pos(brick.a.x, i, 0), anotherBrick)) {
					return true;
				}
			}
			return false;
		}
		throw new RuntimeException(brick.toString());
	}

	private static boolean onBrick(Pos p, Brick br) {
		return p.x <= Math.max(br.a.x, br.b.x) && p.x >= Math.min(br.a.x, br.b.x) &&
			p.y <= Math.max(br.a.y, br.b.y) && p.y >= Math.min(br.a.y, br.b.y);
	}

	private static List<String> split(String in, String separator) {
		return Arrays.stream(in.strip().split(separator)).toList();
	}

	record Brick(
		String id,
		Pos a,
		Pos b
	) {

		public boolean horizontal() {
			return a.y == b.y && a.z == b.z;
		}

		public boolean vertical() {
			return a.x == b.x && a.z == b.z;
		}

		public boolean column() {
			return !horizontal() && !vertical();
		}

		public Brick movedToZ(Long newZ) {
			var diff = minZ(this) - newZ;
			return new Brick(
				id,
				new Pos(a.x, a.y, a.z - diff),
				new Pos(b.x, b.y, b.z - diff)
			);
		}


	}

	record Pos(
		long x,
		long y,
		long z
	) {

	}

	static Pos parse(String s) {
		List<String> split = split(s, ",");
		return new Pos(
			Long.parseLong(split.get(0)),
			Long.parseLong(split.get(1)),
			Long.parseLong(split.get(2))
		);
	}

	private static void testIntersectionCode() {
		// Intersection tests
		var a = new Brick("q", new Pos(5, 0, 3), new Pos(10, 0, 3));
		var b = new Brick("q", new Pos(11, 0, 3), new Pos(10, 0, 3));
		var c = new Brick("q", new Pos(11, 0, 3), new Pos(11, 0, 3));
		var d = new Brick("q", new Pos(11, 0, 3), new Pos(11, 10, 3));
		var e = new Brick("q", new Pos(11, 10, 10), new Pos(11, 10, 3));
		test(intersectXY(a, a));
		test(intersectXY(a, b));
		test(intersectXY(b, c));
		test(intersectXY(d, c));
		test(!intersectXY(a, d));
		test(intersectXY(e, d));
	}

	private static void test(boolean b) {
		if (!b) {
			throw new RuntimeException();
		}
	}
}