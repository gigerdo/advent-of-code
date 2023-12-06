package dev.giger.aoc2023.day06;

import java.util.List;

public class Day06 {
	public static void main(String[] args) {
		// Input
		// Time:        47     98     66     98
		// Distance:   400   1213   1011   1540

		// Quadratic formula
		// ax^2 + bx + c = 0
		// x = (-b +- sqrt(b^2 - 4ac)) / (2a)

		// t: time, d: distance
		// (t - x) * x = d
		// -x^2 + tx - d = 0
		// a = -1
		// b = t
		// c = -d

		var races = List.of(
			// Sample
//			new Race(7, 9),
//			new Race(15, 40),
//			new Race(30, 200)

			// Part1
//			new Race(47, 400),
//			new Race(98, 1213),
//			new Race(66, 1011),
//			new Race(98, 1540)

			// Part 2
			new Race(47986698L, 400121310111540L)
		);

		var result = 1L;
		for (Race race : races) {
			var a = -1;
			var b = race.time();
			var c = -race.distance();
			var x1 = (-b + Math.sqrt(Math.pow(b, 2) - 4 * a * c)) / (2 * a);
			var x2 = (-b - Math.sqrt(Math.pow(b, 2) - 4 * a * c)) / (2 * a);

			x1 = Math.ceil(x1);
			if (Math.abs(x2 - Math.round(x2)) < 0.000001) {
				x2 -= 1;
			} else {
				x2 = Math.ceil(x2);
			}
			result = result * (long) (x2 - x1);
		}
		System.out.println("result = " + result);
	}

	record Race(
		long time,
		long distance
	) {
	}

}