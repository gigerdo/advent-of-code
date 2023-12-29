package dev.giger.aoc2023.day25;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

public class Day25 {
	public static void main(String[] args) throws IOException {
		var input = Files.readAllLines(Path.of("aoc-2023/src/main/resources/day25/input.txt"));
		var graph = input.stream()
			.map(i -> Arrays.stream(i.replace(":", "").split(" ")).toList())
			.collect(Collectors.toMap(
				i -> i.get(0),
				i -> new HashSet<>(i.subList(1, i.size()))
			));

		var bdGraph = new HashMap<String, Set<String>>();

		// Ensure bidirectional entries are set.
		for (var entry : graph.entrySet()) {
			bdGraph.putIfAbsent(entry.getKey(), new HashSet<>());
			bdGraph.get(entry.getKey()).addAll(entry.getValue());
			for (String target : entry.getValue()) {
				bdGraph.putIfAbsent(target, new HashSet<>());
				bdGraph.get(target).add(entry.getKey());
			}
		}

		var counts = new HashMap<Edge, Integer>();
		for (String node : bdGraph.keySet()) {
			var result = shortestPaths(node, bdGraph);
			for (List<String> value : result.values()) {
				for (int i = 1; i < value.size(); i++) {
					String a = value.get(i - 1);
					String b = value.get(i);
					counts.compute(
						Edge.of(a, b),
						(k, v) -> {
							if (v == null) {
								return 1;
							} else {
								return v + 1;
							}
						}
					);
				}
			}
		}

		var top3Edges = counts.entrySet().stream()
			.sorted(Comparator.comparing(i -> -i.getValue()))
			.limit(3)
			.toList();

		var splitGraph = new HashMap<>(bdGraph);
		for (var top3Edge : top3Edges) {
			var e = top3Edge.getKey();
			splitGraph.get(e.a).remove(e.b);
			splitGraph.get(e.b).remove(e.a);
		}
		Map<String, List<String>> result2 = shortestPaths(top3Edges.get(0).getKey().a, splitGraph);
		int groupSize = result2.size();
		int otherGroup = bdGraph.keySet().size() - groupSize;

		System.out.println("Part 1");
		System.out.println(groupSize * otherGroup);
	}

	static Map<String, List<String>> shortestPaths(String start, HashMap<String, Set<String>> bdGraph) {
		var map = new HashMap<String, List<String>>();

		var queue = new PriorityQueue<List<String>>(Comparator.comparing(List::size));
		queue.add(List.of(start));
		while (!queue.isEmpty()) {
			List<String> remove = queue.remove();
			var currentNode = remove.get(remove.size() - 1);
			var neighbors = bdGraph.get(currentNode);
			for (String neighbor : neighbors) {
				int size = Integer.MAX_VALUE;
				List<String> nn = map.get(neighbor);
				if (nn != null) {
					size = nn.size();
				}
				if (remove.size() + 1 < size) {
					ArrayList<String> newList = new ArrayList<>(remove);
					newList.add(neighbor);
					queue.add(newList);
					map.put(neighbor, newList);
				}
			}
		}
		return map;
	}

	record Edge(
		String a,
		String b
	) {

		public static Edge of(String a, String b) {
			if (a.compareTo(b) > 0) {
				return new Edge(a, b);
			} else {
				return new Edge(b, a);
			}
		}
	}
}
