(require 'clojure.set)

;(def input (slurp "sample.txt"))
(def input (slurp "input.txt"))
;(def input (slurp "sample2.txt"))

(def rucksacks (clojure.string/split input #"\n"))

(def split-rucksacks (map #(split-at (/ (count %) 2) %) rucksacks))
(def common-items (map #(clojure.set/intersection (set (first %)) (set (second %))) split-rucksacks))

(defn calculate-priority [item]
  (cond
    (Character/isUpperCase (char item)) (+ (- (int item) (int \A)) 27)
    :else (+ (- (int item) (int \a)) 1)))
(def priorities (map #(map calculate-priority %) common-items))
(def summed-priorities (map #(reduce + %) priorities))
(def total-sum (reduce + summed-priorities))

(println "Part 1" total-sum)

(def groups (map #(map set %) (partition 3 rucksacks)))
(def common-items-per-group (map #(reduce clojure.set/intersection %) groups))
(def total-sum-2
  (->> common-items-per-group
       (map #(map calculate-priority %))
       (map #(reduce + %))
       (reduce +)))

(println "Part 2" total-sum-2)