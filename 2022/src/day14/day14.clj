(require 'clojure.set)

;(def input (slurp "sample.txt"))
(def input (slurp "input.txt"))


(def rocks (->> (clojure.string/split input #"\n")
                (map #(clojure.string/split % #" -> "))
                (map (fn [path]
                       (map (fn [point]
                              (map #(Integer/parseInt %) (clojure.string/split point #",")))
                            path)))
                (vec)))

(defn print-grid [grid]
  (doseq [line grid]
    (doseq [char line]
      (print char))
    (println)))

; Flatten into a->b paths
(def straight-paths (mapcat #(partition 2 1 %) rocks))

; Calculate min/max positions
(def minmax
  (reduce
    (fn [agg next]
      (let [x (first next)
            y (second next)]
        (cond
          (> x (get agg :xmax)) (update agg :xmax (constantly x))
          (< x (get agg :xmin)) (update agg :xmin (constantly x))
          (> y (get agg :ymax)) (update agg :ymax (constantly y))
          (< y (get agg :ymin)) (update agg :ymin (constantly y))
          :else agg)))
    {:xmin 999 :xmax 0 :ymin 999 :ymax 0}
    (mapcat identity straight-paths)))
(println minmax)

(defn set-pt [grid minmax point val]
  (let [y (second point)
        x (first point)
        min-x (get minmax :xmin)
        real-x (- x min-x)]
    (update-in grid [y real-x] (constantly val))))

(defn get-pt [grid minmax point]
  (let [y (second point)
        x (first point)
        real-x (- x (get minmax :xmin))]
    (get-in grid [y real-x])))

(defn print-line [grid minmax from to val]
  (let [dir (cond
              (= (first from) (first to)) 1
              :else 0)
        from-val (get (vec from) dir)
        to-val (get (vec to) dir)]
    (reduce
      (fn [agg next]
        ;(println "to-update" (update (vec from) dir (constantly next)))
        (set-pt agg minmax (update (vec from) dir (constantly next)) val))
      grid
      (range (min from-val to-val) (+ (max from-val to-val) 1)))))

; Add lines to grid
(defn build-grid [minmax]
  (let [height (+ 1 (get minmax :ymax))
        xmin (get minmax :xmin)
        xmax (get minmax :xmax)
        width (- xmax xmin)
        grid (vec (repeat height (vec (repeat width "."))))]
    (reduce
      (fn [agg next]
        (let [from (first next)
              to (second next)]
          (print-line agg minmax from to "#")))
      grid
      straight-paths)))

(def grid-with-rocks (build-grid minmax))

(print-grid grid-with-rocks)

(defn out-of-bounds? [pos minmax]
  (let [x (first pos)
        y (second pos)
        max-y (get minmax :ymax)
        max-x (get minmax :xmax)
        min-x (get minmax :xmin)]
    ;(println minmax max-y max-x min-x)
    (cond
      (>= x max-x) true
      (>= y max-y) true
      (<= x min-x) true
      :else false))
  )

(defn simulate-sand [grid minmax sand-origin current-pos units-of-sand]
  (let [below (update current-pos 1 + 1)
        left (update below 0 - 1)
        right (update below 0 + 1)]
    (cond
      (out-of-bounds? current-pos minmax) [grid units-of-sand]
      (= (get-pt grid minmax below) ".") (recur grid minmax sand-origin below units-of-sand)
      (= (get-pt grid minmax left) ".") (recur grid minmax sand-origin left units-of-sand)
      (= (get-pt grid minmax right) ".") (recur grid minmax sand-origin right units-of-sand)
      :else (recur (set-pt grid minmax current-pos "o") minmax sand-origin sand-origin (+ units-of-sand 1)))))

(def sand-origin [500 0])
(def result (simulate-sand grid-with-rocks minmax sand-origin sand-origin 0))

(print-grid (first result))

(println "Part 1" (second result))

(defn simulate-sand-2 [grid minmax sand-origin current-pos units-of-sand]
  (let [below (update current-pos 1 + 1)
        left (update below 0 - 1)
        right (update below 0 + 1)]
    (cond
      (= (second current-pos) (get minmax :ymax)) (recur (set-pt grid minmax current-pos "o") minmax sand-origin sand-origin (+ units-of-sand 1))
      (= (get-pt grid minmax below) ".") (recur grid minmax sand-origin below units-of-sand)
      (= (get-pt grid minmax left) ".") (recur grid minmax sand-origin left units-of-sand)
      (= (get-pt grid minmax right) ".") (recur grid minmax sand-origin right units-of-sand)
      (= current-pos sand-origin) [(set-pt grid minmax current-pos "o") (+ units-of-sand 1)]
      :else (recur (set-pt grid minmax current-pos "o") minmax sand-origin sand-origin (+ units-of-sand 1)))))

(def minmax-2 {:xmin (- (get minmax :xmin) 180)
               :xmax (+ (get minmax :xmax) 150)
               :ymin 0
               :ymax (+ (get minmax :ymax) 1)})
(def grid-2 (build-grid minmax-2))
(def result-2 (simulate-sand-2 grid-2 minmax-2 sand-origin sand-origin 0))
(print-grid (first result-2))

(println "Part 2" (second result-2))