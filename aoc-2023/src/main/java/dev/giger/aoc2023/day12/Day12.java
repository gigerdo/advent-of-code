package dev.giger.aoc2023.day12;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Day12 {

	static Map<Memo, Long> memos = new HashMap<>();

	public static void main(String[] args) throws IOException {
		var lines = Files.readAllLines(Path.of("aoc-2023/src/main/resources/day12/input.txt"));

		var sum1 = 0L;
		var sum2 = 0L;
		var counter = 0;
		for (String line : lines) {
			var lineSplit = split(line, " ");
			var springs = lineSplit.get(0);
			var groups = split(lineSplit.get(1), ",").stream().map(Integer::valueOf).toList();

			// Part 1
			long tmp1 = recurseIt(springs, groups);
			sum1 += tmp1;

			// Part2
			var springs2 = String.join("?", List.of(springs, springs, springs, springs, springs));
			var groups2 = Stream.of(groups, groups, groups, groups, groups).flatMap(Collection::stream).toList();
			long tmp2 = recurseIt(springs2, groups2);
			sum2 += tmp2;

			System.out.println(counter + " - P1: " + tmp2 + " - P2: " + tmp2);
			counter++;
		}

		System.out.println("Part 1");
		System.out.println(sum1);

		System.out.println("Part 2");
		System.out.println(sum2);
	}

	private static long recurseIt(String cs, List<Integer> groups) {
		Memo key = new Memo(cs, groups);
		if (memos.containsKey(key)) {
			return memos.get(key);
		}

		if (cs.isEmpty() && groups.isEmpty()) {
			return 1;
		}
		if (cs.isEmpty()) {
			return 0;
		}
		if (groups.isEmpty() && !cs.contains("#")) {
			return 1;
		}
		if (groups.isEmpty() && cs.contains("#")) {
			return 0;
		}
		int qS = cs.indexOf('?');
		int hS = cs.indexOf('#');
		var startIdx = Math.min(qS == -1 ? 99999 : qS, hS == -1 ? 99999 : hS);
		if (startIdx == 99999) {
			return 0;
		}

		var groupSize = groups.get(0);

		var expectedPart = IntStream.range(0, groupSize).mapToObj(i -> "#").collect(Collectors.joining()) + ".";

		var matches = true;
		for (int i = 0; i < expectedPart.length(); i++) {
			var idx = startIdx + i;
			if (idx == cs.length() && expectedPart.charAt(i) == '.') {
				break;
			}
			if (idx >= cs.length()) {
				matches = false;
				break;
			}
			if (cs.charAt(idx) != '?' && cs.charAt(idx) != expectedPart.charAt(i)) {
				matches = false;
				break;
			}
		}

		long total = 0;
		if (matches) {
			var ep = expectedPart;
			if (cs.length() == startIdx + expectedPart.length() - 1) {
				ep = ep.substring(0, expectedPart.length() - 1);
			}
			var subgroups = groups.subList(1, groups.size());
			var substring = cs.substring(startIdx + ep.length());
			var cutoff = cs.substring(0, startIdx);
			total += recurseIt(substring, subgroups);
		}
		if (cs.charAt(startIdx) == '?') {
			// Put '.'
			var cutoff = cs.substring(0, startIdx);
			total += recurseIt(cs.substring(startIdx + 1), groups);
		}

		memos.put(key, total);
		return total;
	}

	private static boolean groupsMatch(String cs, List<Integer> groups) {
		var result = getGroups(cs);
		if (cs.contains("?")) {
			if (result.size() > groups.size()) {
				return false;
			}
			for (int i = 0; i < result.size() - 1; i++) {
				if (!result.get(i).equals(groups.get(i))) {
					return false;
				}
			}
			if (result.size() > 1 && result.get(result.size() - 1) > groups.get(result.size() - 1)) {
				return false;
			}
			return true;
		}
		return result.equals(groups);
	}

	private static ArrayList<Integer> getGroups(String cs) {
		var result = new ArrayList<Integer>();
		var counter = 0;
		for (int i = 0; i < cs.length(); i++) {
			if (cs.charAt(i) == '#') {
				counter += 1;
			} else if (cs.charAt(i) == '.') {
				if (counter > 0) {
					result.add(counter);
				}
				counter = 0;
			} else if (cs.charAt(i) == '?') {
				break;
			}
		}
		if (counter > 0) {
			result.add(counter);
		}
		return result;
	}

	private static List<String> split(String in, String separator) {
		return Arrays.stream(in.strip().split(separator)).toList();
	}

	record Memo(
		String in,
		List<Integer> groups
	) {
	}
}