package dev.giger.aoc2023.day19;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class Day19 {

	public static void main(String[] args) throws IOException {
		var input = Files.readString(Path.of("aoc-2023/src/main/resources/day19/input.txt"));

		// Parse input
		var parts = split(input, "\n\n");
		var workflows = split(parts.get(0), "\n").stream()
			.map(w -> {
				// bnv{a>2193:ssr,x>3256:fh,m>386:A,R}
				var r = regex(w, "([a-z]+)\\{(([a-z]+[><][0-9]+:[a-zA-Z]+,)+)([a-zA-Z]+)\\}");
				String name = r.group(1);
				List<Check> checks = split(r.group(2), ",").stream()
					.map(c -> {
						var cr = regex(c, "([a-z]+)([<>])([0-9]+)\\:([a-zA-Z]+)");
						var category = cr.group(1);
						var operator = cr.group(2);
						var rating = Integer.parseInt(cr.group(3));
						var sendTo = cr.group(4);
						return new Check(category, operator, rating, sendTo);
					})
					.toList();
				String fallback = r.group(4);
				return new Workflow(name, checks, fallback);
			})
			.collect(Collectors.toMap(i -> i.name, i -> i));

		var ratings = split(parts.get(1), "\n").stream()
			.map(in -> {
				// {x=787,m=2655,a=1222,s=2876}
				var r = regex(in, "\\{x=([0-9]+),m=([0-9]+),a=([0-9]+),s=([0-9]+)\\}");
				var x = Integer.parseInt(r.group(1));
				var m = Integer.parseInt(r.group(2));
				var a = Integer.parseInt(r.group(3));
				var s = Integer.parseInt(r.group(4));

				return new Rating(x, m, a, s);
			})
			.toList();


		// Part 1
		var totalRating = 0L;
		for (Rating rating : ratings) {
			var workflowName = "in";
			while (true) {
				var workflow = workflows.get(workflowName);
				String nextWorkflowName = null;
				for (Check check : workflow.checks) {
					var categoryRating = rating.getCategory(check.category);
					if (check.operator.equals(">")) {
						if (categoryRating > check.rating) {
							nextWorkflowName = check.sendTo;
							break;
						}
					} else {
						if (categoryRating < check.rating) {
							nextWorkflowName = check.sendTo;
							break;
						}
					}
				}
				if (nextWorkflowName == null) {
					nextWorkflowName = workflow.fallback;
				}
				if (nextWorkflowName.equals("R")) {
					break;
				}
				if (nextWorkflowName.equals("A")) {
					totalRating += rating.total();
					break;
				}
				workflowName = nextWorkflowName;
			}
		}
		System.out.println("Part 1");
		System.out.println(totalRating);


		// Part 2
		var queue = new ArrayList<AccumulatedCheck>();
		queue.add(new AccumulatedCheck(
			new HashMap<>(Map.of("x", 1, "m", 1, "a", 1, "s", 1)),
			new HashMap<>(Map.of("x", 4001, "m", 4001, "a", 4001, "s", 4001)),
			"in"
		));
		var totalCombinations = 0L;
		while (!queue.isEmpty()) {
			AccumulatedCheck current = queue.remove(0);

			if (current.workflowName.equals("A")) {
				var combinations = 1L;
				for (String key : current.min.keySet()) {
					var min = current.min.get(key);
					var max = current.max.get(key);
					combinations *= max - min;
				}
				totalCombinations += combinations;
				continue;
			} else if (current.workflowName.equals("R")) {
				// Ignore
				continue;
			}

			var workflow = workflows.get(current.workflowName);
			for (Check check : workflow.checks) {
				if (check.operator.equals(">")) {
					var newCheck = current.withMin(check.category, check.rating + 1, check.sendTo);
					queue.add(newCheck);
					current = current.withMax(check.category, check.rating + 1, current.workflowName);
				}
				if (check.operator.equals("<")) {
					var newCheck = current.withMax(check.category, check.rating, check.sendTo);
					queue.add(newCheck);
					current = current.withMin(check.category, check.rating, current.workflowName);
				}
			}
			var newCheck = current.withWorkflow(workflow.fallback);
			queue.add(newCheck);
		}

		System.out.println("Part 2");
		System.out.println(totalCombinations);
		System.out.println(totalCombinations == 121964982771486L);
	}

	private static Matcher regex(String in, String regex1) {
		var regex = Pattern.compile(regex1);
		var r = regex.matcher(in);
		if (!r.matches()) {
			throw new RuntimeException();
		}
		return r;
	}

	private static List<String> split(String in, String separator) {
		return Arrays.stream(in.strip().split(separator)).toList();
	}

	record Workflow(String name, List<Check> checks, String fallback) {
	}

	record Check(String category, String operator, int rating, String sendTo) {
	}

	record Rating(int x, int m, int a, int s) {
		public long getCategory(String category) {
			switch (category) {
				case "x" -> {
					return x;
				}
				case "m" -> {
					return m;
				}
				case "a" -> {
					return a;
				}
				case "s" -> {
					return s;
				}
			}
			throw new RuntimeException();
		}

		public long total() {
			return x + m + a + s;
		}
	}

	record AccumulatedCheck(
		Map<String, Integer> min,
		Map<String, Integer> max,
		String workflowName
	) {
		public AccumulatedCheck withMin(String category, int l, String workflow) {
			var newMin = new HashMap<>(min);
			newMin.computeIfPresent(category, (c, prev) -> {
				if (prev < l) {
					return l;
				} else {
					return prev;
				}
			});
			return new AccumulatedCheck(newMin, max, workflow);
		}

		public AccumulatedCheck withMax(String category, int l, String workflow) {
			var newMax = new HashMap<>(max);
			newMax.computeIfPresent(category, (c, prev) -> {
				if (prev > l) {
					return l;
				} else {
					return prev;
				}
			});
			return new AccumulatedCheck(min, newMax, workflow);
		}

		public AccumulatedCheck withWorkflow(String workflow) {
			return new AccumulatedCheck(
				min, max, workflow
			);
		}
	}
}