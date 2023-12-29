package dev.giger.aoc2023.day24;

import com.microsoft.z3.Context;
import com.microsoft.z3.IntNum;
import com.microsoft.z3.Solver;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;

public class Day24 {
	public static void main(String[] args) throws IOException {
		var input = Files.readAllLines(Path.of("aoc-2023/src/main/resources/day24/input.txt"));

		var lines = input.stream()
			.map(i -> {
				var numbers = Arrays.stream(i.replace(" @", ",").split(", "))
					.map(String::strip)
					.map(Long::valueOf)
					.toList();
				return new Line(
					new Vec3(
						numbers.get(0),
						numbers.get(1),
						numbers.get(2)
					),
					new Vec3(
						numbers.get(3),
						numbers.get(4),
						numbers.get(5)
					)
				);
			})
			.toList();

		var intersections = 0;
		for (int i = 0; i < lines.size(); i++) {
			Line a = lines.get(i);
			for (int j = i + 1; j < lines.size(); j++) {
				Line b = lines.get(j);
				if (intersects(a, b)) {
					intersections += 1;
				}
			}
		}

		System.out.println("Part 1");
		System.out.println(intersections);

		System.out.println("Part 2");
		Context ctx = new Context();

		var px = ctx.mkIntConst("px");
		var py = ctx.mkIntConst("py");
		var pz = ctx.mkIntConst("pz");
		var vx = ctx.mkIntConst("vx");
		var vy = ctx.mkIntConst("vy");
		var vz = ctx.mkIntConst("vz");

		Solver s = ctx.mkSolver();

		for (int i = 0; i < 3; i++) {
			Line line = lines.get(i);
			var t = ctx.mkIntConst("t" + i);

			s.add(ctx.mkGt(t, ctx.mkInt(1)));
			s.add(ctx.mkEq(
				ctx.mkAdd(px, ctx.mkMul(t, vx)),
				ctx.mkAdd(ctx.mkInt((long) line.p.x), ctx.mkMul(t, ctx.mkInt((long) line.v.x)))
			));
			s.add(ctx.mkEq(
				ctx.mkAdd(py, ctx.mkMul(t, vy)),
				ctx.mkAdd(ctx.mkInt((long) line.p.y), ctx.mkMul(t, ctx.mkInt((long) line.v.y)))
			));
			s.add(ctx.mkEq(
				ctx.mkAdd(pz, ctx.mkMul(t, vz)),
				ctx.mkAdd(ctx.mkInt((long) line.p.z), ctx.mkMul(t, ctx.mkInt((long) line.v.z)))
			));
		}
		s.check();

//		System.out.println(s.getModel());

		var solPx = ((IntNum) s.getModel().getConstInterp(px)).getInt64();
		var solPy = ((IntNum) s.getModel().getConstInterp(py)).getInt64();
		var solPz = ((IntNum) s.getModel().getConstInterp(pz)).getInt64();

		System.out.println(solPx + solPy + solPz);

		// Solution:
		//
		//(define-fun t0 () Int
		//  132725062611)
		// (define-fun t2 () Int
		//  408488334369)
		//(define-fun t1 () Int
		//  203007258383)
		//(define-fun vx () Int
		//  (- 32))
		//(define-fun vy () Int
		//  161)
		//(define-fun vz () Int
		//  251)
		//(define-fun pz () Int
		//  129299668674521)
		//(define-fun py () Int
		//  196475852174059)
		//(define-fun px () Int
		//  315844328917588)
		ctx.close();
	}


	private static boolean intersects(Line a, Line b) {
		double s = (a.v.x * b.p.y + a.p.x * a.v.y - a.v.y * b.p.x - a.v.x * a.p.y) / (b.v.x * a.v.y - b.v.y * a.v.x);
		double t = (b.p.x + s * b.v.x - a.p.x) / a.v.x;
		if (s < 1 || t < 1) {
			return false;
		}
		Vec3 intersection = a.mult(t);
		double min = 200000000000000.0;
		double max = 400000000000000.0;
//		double min = 7;
//		double max = 27;
		return intersection.x >= min && intersection.x <= max
			&& intersection.y >= min && intersection.y <= max;
	}

	record Vec3(
		double x,
		double y,
		double z
	) {
	}

	record Line(
		Vec3 p,
		Vec3 v
	) {
		public Vec3 mult(double t) {
			return new Vec3(
				p.x + t * v.x,
				p.y + t * v.y,
				p.z + t * v.z
			);
		}
	}
}
