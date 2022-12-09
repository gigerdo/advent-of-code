;(def input (slurp "sample.txt"))
(def input (slurp "input.txt"))

(def input
  (->> (clojure.string/split input #"\n")
       (map #(clojure.string/split (clojure.string/trim %) #""))
       (map #(vec (map (fn [x] (Integer/parseInt x)) %)))
       (vec)))
;(println input)

(def height (count input))
(def width (count (first input)))

; Setup board with all sides already checked
(def checked
  (vec (map
         (fn [y]
           (vec (map
                  (fn [x]
                    (cond
                      (= x 0) true
                      (= y 0) true
                      (= x (- width 1)) true
                      (= y (- height 1)) true
                      :else false))
                  (range width)
                  ))
           )
         (range height))))
;(println checked)

(defn check-spot [checked-spots pos]
  (update-in checked-spots pos (constantly true)))

(defn get-spot [checked-spots pos]
  (get-in checked-spots pos))

(defn valid-pos? [pos]
  (let [x (second pos)
        y (first pos)]
    (cond
      (< x 0) false
      (>= x width) false
      (< y 0) false
      (>= y height) false
      :else true)))

(defn check-next [checked-spots pos increment-fn tree-line]
  (let [next-pos (increment-fn pos)
        next-value (get-spot input next-pos)]
    ;(println pos next-pos current-value next-value)
    (cond
      (not (valid-pos? next-pos)) checked-spots
      (> next-value tree-line) (recur (check-spot checked-spots next-pos) next-pos increment-fn next-value)
      :else (recur checked-spots next-pos increment-fn tree-line))))

; Check each direction individually, starting from the border
; Generates a new map with each tree marked true if visible
(def left (map (fn [y] [[y 0] (fn [p] (update-in p [1] + 1))]) (range height)))
(def right (map (fn [y] [[y (- width 1)] (fn [p] (update-in p [1] - 1))]) (range height)))
(def top (map (fn [x] [[0 x] (fn [p] (update-in p [0] + 1))]) (range width)))
(def bottom (map (fn [x] [[(- height 1) x] (fn [p] (update-in p [0] - 1))]) (range width)))
(def directions (concat left right top bottom))
(def result (reduce (fn [agg next] (check-next agg (first next) (second next) (get-spot input (first next)))) checked directions))

; Count all visible trees in result
(def count-trees
  (reduce (fn [agg line]
            (+ agg (reduce
                     (fn [inneragg element]
                       (cond (= element true) (+ inneragg 1)
                             :else inneragg))
                     0
                     line)))
          0
          result))

(println "Part 1" count-trees)

; Start from specific tree, count all visible trees in one dir
(defn count-visible-trees-in-dir [pos increment-fn tree-line visible]
  (let [next-pos (increment-fn pos)
        next-value (get-spot input next-pos)]
    (cond
      (not (valid-pos? next-pos)) visible
      (< next-value tree-line) (recur next-pos increment-fn tree-line (+ visible 1))
      :else (+ visible 1))))

; Check all dirs for one tree, calculate score
(defn visible-trees [pos]
  (let [current-height (get-spot input pos)
        visible-right (count-visible-trees-in-dir pos (fn [p] (update-in p [1] + 1)) current-height 0)
        visible-left (count-visible-trees-in-dir pos (fn [p] (update-in p [1] - 1)) current-height 0)
        visible-top (count-visible-trees-in-dir pos (fn [p] (update-in p [0] + 1)) current-height 0)
        visible-bottom (count-visible-trees-in-dir pos (fn [p] (update-in p [0] - 1)) current-height 0)]
    (* visible-right visible-left visible-bottom visible-top)))

; Check all trees
(def scenic-scores
  (map
    (fn [y] (map
              (fn [x] (visible-trees [y x]))
              (range width)))
    (range height)))

; Find max score
(def top-scenic-score (reduce (fn [agg line] (max (reduce max 0 line) agg)) 0 scenic-scores))

(println "Part 2" top-scenic-score)

