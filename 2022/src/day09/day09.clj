;(def input (slurp "sample.txt"))
;(def input (slurp "sample2.txt"))
(def input (slurp "aoc_2022_day09_large-1.in"))

(def input
  (->> (clojure.string/split input #"\n")
       (map #(clojure.string/split (clojure.string/trim %) #" "))))
;(println input)

(defn parse-movement [movement]
  (let [dir (first movement)
        steps (Integer/parseInt (second movement))]
    (cond
      (= dir "U") [[1 0] steps]
      (= dir "D") [[-1 0] steps]
      (= dir "R") [[0 1] steps]
      (= dir "L") [[0 -1] steps]
      :else (throw (Exception. (.concat "Unknown dir: " dir))))))

; Expand into individual steps. Example: [U 2] -> [[1 0] [1 0]]
(defn expand-input [movement]
  (let [[dir steps] (parse-movement movement)]
    (reduce (fn [agg next] (conj agg dir)) [] (range steps))))
(def steps (->> (mapcat expand-input input)))
;(println steps)

(defn move-head [head movement]
  [(+ (first head) (first movement)) (+ (second head) (second movement))])

(defn move-tail [head tail]
  (let [head-y (first head)
        head-x (second head)
        tail-y (first tail)
        tail-x (second tail)
        diff-y (- head-y tail-y)
        diff-x (- head-x tail-x)
        dist-y (abs diff-y)
        dist-x (abs diff-x)
        dir-y (Integer/signum diff-y)
        dir-x (Integer/signum diff-x)]
    (cond
      (and (<= dist-y 1) (<= dist-x 1)) tail
      :else [(+ tail-y dir-y) (+ tail-x dir-x)])))

(defn move-rope [head tail tail-positions movement]
  (let [new-head (move-head head movement)
        new-tail (move-tail new-head tail)]
    [new-head new-tail (conj! tail-positions (.toString new-tail))]))

; head, tail, visited-tail-positions
(def starting-pos [[0 0] [0 0] (transient (hash-set (.toString [0 0])))])

(println "Result")

; Apply all steps in order, keeping track of all visited tail positions
(def result
  (time
    (count
      (get
        (reduce
          (fn [agg next]
            (move-rope
              (first agg)
              (second agg)
              (get-in agg [2])
              next))
          starting-pos
          steps)
        2))))

(println "Part 1" result)

(defn move-knot [agg knot]
  (let [previous-knot (first agg)
        result-knots (second agg)
        moved-knot (move-tail previous-knot knot)]
    [moved-knot (conj result-knots moved-knot)]))

; Same as part1 but update each knot based on the previous knot
(defn move-long-rope [head knots tail-positions movement]
  (let [new-head (move-head head movement)
        new-knots (second (reduce move-knot [new-head []] knots))
        new-tail (last new-knots)]
    [new-head new-knots (conj! tail-positions (.toString new-tail))]))

(defn move-pos [pos d]
  [(- 30 (+ (first pos) d)) (+ (second pos) d)])
(defn visualize [agg]
  (println "")
  (let [field (vec (repeat 30 (vec (repeat 30 "."))))
        head (first agg)
        knots (second agg)
        updated-field (reduce
                        (fn [agg next]
                          (update-in agg (move-pos next 15) (constantly "X")))
                        field
                        knots)
        updated-field-head (update-in updated-field (move-pos head 15) (constantly "H"))]
    (doseq [line updated-field-head] (println line))))

; Apply all steps in order, keeping track of all visited tail positions
(def starting-pos-long-rope [[0 0] (repeat 9 [0 0]) (transient (hash-set (.toString [0 0])))])
(def result-2
  (time
    (count
      (get
        (reduce
          (fn [agg next]
            ;(visualize agg)
            (move-long-rope
              (first agg)
              (second agg)
              (get-in agg [2])
              next))
          starting-pos-long-rope
          steps)
        2))))
;(visualize result-2)
; Visualize tail-path
;(visualize [[0 0] (get-in result-2 [2])])


(println "Part 2" result-2)