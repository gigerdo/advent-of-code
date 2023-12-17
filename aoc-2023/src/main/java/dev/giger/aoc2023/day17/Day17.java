package dev.giger.aoc2023.day17;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Day17 {

	public static void main(String[] args) throws IOException {
		var lines = Files.readAllLines(Path.of("aoc-2023/src/main/resources/day17/input.txt"));
		var map = lines.stream().map(l -> Arrays.stream(l.split("")).map(Integer::valueOf).toList()).toList();
		var height = lines.size();
		var width = lines.get(0).length();

		var shortestDistance = new ArrayList<ArrayList<HashMap<Pos, Integer>>>();
		for (int y = 0; y < height; y++) {
			var row = new ArrayList<HashMap<Pos, Integer>>();
			for (int x = 0; x < width; x++) {
				var m = new HashMap<Pos, Integer>();
				// Each direction is considered a separate node
				m.put(new Pos(-1, 0), Integer.MAX_VALUE);
				m.put(new Pos(1, 0), Integer.MAX_VALUE);
				m.put(new Pos(0, -1), Integer.MAX_VALUE);
				m.put(new Pos(0, 1), Integer.MAX_VALUE);
				row.add(m);
			}
			shortestDistance.add(row);
		}

		var queue = new PriorityQueue<Node>(Comparator.comparingInt(k -> k.cost));
		queue.add(new Node(0, 0, 0, 1, 0, List.of()));
		queue.add(new Node(0, 0, 1, 0, 0, List.of()));
		shortestDistance.get(0).get(0).put(new Pos(0, 1), 0);
		shortestDistance.get(0).get(0).put(new Pos(1, 0), 0);

		while (!queue.isEmpty()) {
			Node node = queue.remove();
			if (node.y == height - 1 && node.x == width - 1) {
				printPath(node, width, height);
				System.out.println("Result cost:");
				System.out.println(node.cost);
				break;
			}
			if (node.cost > shortestDistance.get(node.y).get(node.x).get(node.getDir())) {
				continue;
			}

			var leftDirY = -1 * node.dirX;
			var leftDirX = node.dirY;
			var rightDirY = node.dirX;
			var rightDirX = -1 * node.dirY;

			// Collect all next nodes
//			var nextNodes = IntStream.rangeClosed(1, 3) // Part1
			var nextNodes = IntStream.rangeClosed(4, 10) // Part2
				.mapToObj(i -> Stream.of(
					// Left
					move(node, leftDirY, leftDirX, i, map),
					// Right
					move(node, rightDirY, rightDirX, i, map)
				))
				.flatMap(i -> i)
				.filter(Objects::nonNull)
				.filter(n -> n.cost < shortestDistance.get(n.y).get(n.x).get(n.getDir()))
				.toList();

			for (Node nextNode : nextNodes) {
				shortestDistance.get(nextNode.y).get(nextNode.x).put(nextNode.getDir(), nextNode.cost);
			}

			queue.addAll(nextNodes);
		}
	}

	private static Node move(Node node, int dirY, int dirX, int i, List<List<Integer>> costs) {
		var y = node.y;
		var x = node.x;
		var cost = node.cost;
		var path = new ArrayList<>(node.path);
		for (int i1 = 0; i1 < i; i1++) {
			y += dirY;
			x += dirX;
			if (y < 0 || y >= costs.size() || x < 0 || x >= costs.get(0).size()) {
				return null;
			}
			cost += costs.get(y).get(x);
			path.add(new Pos(y, x));
		}

		return new Node(y, x, dirY, dirX, cost, path);
	}

	private static void printPath(Node p, int width, int height) {
		for (int y = 0; y < height; y++) {
			for (int x = 0; x < width; x++) {
				if (p.path.contains(new Pos(y, x))) {
					System.out.print("#");
				} else {
					System.out.print(".");
				}
			}
			System.out.println();
		}
	}

	record Node(
		int y,
		int x,
		int dirY,
		int dirX,
		int cost,
		List<Pos> path
	) {
		public Pos getDir() {
			return new Pos(dirY, dirX);
		}
	}

	record Pos(
		int y,
		int x
	) {
	}
}