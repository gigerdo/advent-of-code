package dev.giger.aoc2023.day20;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

import static dev.giger.aoc2023.day20.Day20.Value.HIGH;
import static dev.giger.aoc2023.day20.Day20.Value.LOW;

public class Day20 {
	public static void main(String[] args) throws IOException {
		var input = Files.readAllLines(Path.of("aoc-2023/src/main/resources/day20/input.txt"));

		var modules = input.stream()
			.map(module -> {
				var s = split(module, " -> ");
				var moduleName = s.get(0);
				String type;
				if (moduleName.equals("broadcaster")) {
					type = "broadcaster";
				} else {
					type = moduleName.charAt(0) + "";
					moduleName = moduleName.substring(1);
				}
				var targets = split(s.get(1), ", ");

				return new Module(
					moduleName,
					type,
					targets,
					new HashMap<>()
				);
			})
			.collect(Collectors.toMap(i -> i.name, i -> i));

		modules.put("rx",
			new Module(
				"rx",
				"output",
				List.of(),
				new HashMap<>()
			));

		// Initialize state
		for (var module : modules.values()) {
			if (module.type.equals("%")) {
				module.state.put("status", LOW);
			} else if (module.type.equals("&")) {
				modules.values().stream()
					.filter(m -> m.targets.contains(module.name))
					.map(m -> m.name)
					.forEach(mn -> module.state.put(mn, LOW));
			}
		}

		var highPulses = 0L;
		var lowPulses = 0L;
		// Start processing
		for (int i = 0; i < 1000; i++) {
			var queue = new LinkedList<Pulse>();
			queue.add(new Pulse("button", "broadcaster", LOW));

			while (!queue.isEmpty()) {
				var pulse = queue.removeFirst();
				if (pulse.value == HIGH) {
					highPulses += 1;
				} else {
					lowPulses += 1;
				}
				// For part2
				if (pulse.target.equals("zr") && pulse.value == HIGH) {
					System.out.println("ZR HIGH - " + pulse + " / " + (i + 1));
				}
				var module = modules.get(pulse.target);
				switch (module.type) {
					case "broadcaster" -> {
						for (String target : module.targets) {
							queue.add(new Pulse(module.name, target, pulse.value));
						}
					}
					case "%" -> {
						if (pulse.value == LOW) {
							var newVal = module.state.computeIfPresent("status", (k, v) -> v.flip());
							for (String target : module.targets) {
								queue.add(new Pulse(module.name, target, newVal));
							}
						}
					}
					case "&" -> {
						module.state.put(pulse.source, pulse.value);
						Value newValue;
						if (module.state.values().stream().allMatch(v -> v == HIGH)) {
							newValue = LOW;
						} else {
							newValue = HIGH;
						}
						for (String target : module.targets) {
							queue.add(new Pulse(module.name, target, newValue));
						}
					}
					case "output" -> {
						module.state.put("status", pulse.value);
					}
				}
			}
		}

		System.out.println("Part 1");

		System.out.println(highPulses * lowPulses);

		/// Found by running for ~10k steps
		var gc = 3853L;
		var xf = 4073L;
		var cm = 4091L;
		var sz = 4093L;
		System.out.println("Part 2");
		System.out.println(gc * xf * cm * sz);
	}

	private static List<String> split(String in, String separator) {
		return Arrays.stream(in.strip().split(separator)).toList();
	}

	record Module(
		String name,
		String type,
		List<String> targets,
		Map<String, Value> state
	) {
	}

	record Pulse(
		String source,
		String target,
		Value value
	) {
	}

	enum Value {
		HIGH,
		LOW;

		public Value flip() {
			if (this == HIGH) return LOW;
			if (this == LOW) return HIGH;
			throw new RuntimeException();
		}
	}

	public static long lcm(long a, long b) {
		if (a == 0 || b == 0) {
			return 0;
		}
		long max = Math.max(Math.abs(a), Math.abs(b));
		long min = Math.min(Math.abs(a), Math.abs(b));
		long lcm = max;
		while (lcm % min != 0) {
			lcm += max;
		}
		return lcm;
	}
}
