;(def input (slurp "sample.txt"))
(def input (slurp "input.txt"))

(def section-assignment-pairs
  (->> (clojure.string/split input #"\n")
       (map (fn [line]
              (->> (clojure.string/split line #",")
                   (map #(clojure.string/split % #"-"))
                   (map #(map (fn [x] (Integer/parseInt x)) %)))))))

(defn is-fully-contained-in [range-a range-b]
  (cond
    (and
      (>= (first range-a) (first range-b))
      (<= (second range-a) (second range-b))) true
    :else false))

(defn check-contains [range-a range-b]
  (or (is-fully-contained-in range-a range-b) (is-fully-contained-in range-b range-a)))

(def fully-contained (map #(check-contains (first %) (second %)) section-assignment-pairs))

(def sum-of-fully-contained (count (filter identity fully-contained)))

(println "Part 1" sum-of-fully-contained)

(defn ranges-overlap [range-a range-b]
  (let [a-start (first range-a)
        a-end (second range-a)
        b-start (first range-b)
        b-end (second range-b)]
    (cond
      (and (<= a-start b-start) (>= a-end b-start)) true
      (and (<= a-start b-end) (>= a-end b-end)) true
      (and (>= a-start b-start) (<= a-end b-end)) true
      :else false)))

(def num-overlapping-ranges
  (->> section-assignment-pairs
       (map #(ranges-overlap (first %) (second %)))
       (filter identity)
       (count)))

(println "Part 2" num-overlapping-ranges)