;(def input (slurp "sample.txt"))
(def input (slurp "input.txt"))

(def elves
  (as-> input input
        (clojure.string/split input #"\n\n")
        (map #(clojure.string/split %1 #"\n") input)
        (map (fn [elf] (map #(Integer/parseInt %) elf)) input)
        (map #(reduce + %) input)))

(def max-calories (apply max elves))

(println "Part 1" max-calories)

(def top-three-elves (take-last 3 (sort elves)))

(println "Part 2" top-three-elves (reduce + top-three-elves))