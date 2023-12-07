package dev.giger.aoc2023.day07;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

public class Day07 {
	public static void main(String[] args) throws IOException {
		var lines = Files.readAllLines(Path.of("aoc-2023/src/main/resources/day07/input.txt"));

		List<String> order = List.of("J", "2", "3", "4", "5", "6", "7", "8", "9", "T", "Q", "K", "A");
		var cardComparator = Comparator.<String>comparingInt(order::indexOf);
		var comparator = Comparator.comparing(Hand::strength)
			.thenComparing(h -> h.hand.get(0), cardComparator)
			.thenComparing(h -> h.hand.get(1), cardComparator)
			.thenComparing(h -> h.hand.get(2), cardComparator)
			.thenComparing(h -> h.hand.get(3), cardComparator)
			.thenComparing(h -> h.hand.get(4), cardComparator);
		var hands = new TreeSet<>(comparator);
		for (String line : lines) {
			var lineSplit = split(line, " ");
			var hand = split(lineSplit.get(0), "");
			var bid = Integer.parseInt(lineSplit.get(1));

			// Five of a kind -> 6
			// Four of a kind -> 5
			// Full house -> 4
			// Three of a kind -> 3
			// Two pair -> 2
			// One pair -> 1
			// High card -> 0
			var sameCards = new HashMap<String, Integer>();
			for (String s : hand) {
				sameCards.compute(s, (k, v) -> {
					if (v == null) {
						return 1;
					}
					return v + 1;
				});
			}

			//var strength = getStrength(sameCards);
			var strength = getMaxStrength(sameCards);

			hands.add(new Hand(hand, bid, strength));
		}

		var index = 1L;
		var result = 0L;
		for (Hand hand : hands) {
			System.out.println(index + "; " + hand.hand() + "; " + hand.strength + "; " + hand.bid);
			result += index * hand.bid();
			index += 1;
		}
		System.out.println("Part 2");
		System.out.println(result);
	}

	private static int getMaxStrength(Map<String, Integer> cards) {
		if (cards.containsKey("J")) {
			// Special case of only Js
			if (cards.get("J") == 5) {
				return getStrength(cards);
			}

			var js = cards.get("J");
			var maxStrength = 0;
			for (var e : cards.entrySet()) {
				if (e.getKey().equals("J")) {
					continue;
				}
				var q = new HashMap<>(cards);
				if (js - 1 > 0) {
					q.put("J", js - 1);
				} else {
					q.remove("J");
				}
				q.put(e.getKey(), e.getValue() + 1);
				var strength = getMaxStrength(q);
				maxStrength = Math.max(maxStrength, strength);
			}
			return maxStrength;
		} else {
			return getStrength(cards);
		}
	}

	private static int getStrength(Map<String, Integer> sameCards) {
		var strength = 0;
		if (sameCards.containsValue(5)) {
			strength = 6;
		} else if (sameCards.containsValue(4)) {
			strength = 5;
		} else if (sameCards.containsValue(3) && sameCards.containsValue(2)) {
			strength = 4;
		} else if (sameCards.containsValue(3)) {
			strength = 3;
		} else if (sameCards.values().stream().filter(v -> v == 2).count() == 2) {
			strength = 2;
		} else if (sameCards.containsValue(2)) {
			strength = 1;
		}
		return strength;
	}

	private static List<String> split(String in, String separator) {
		return Arrays.stream(in.strip().split(separator)).toList();
	}

	record Hand(
		List<String> hand,
		int bid,
		int strength
	) {
	}
}