(require 'clojure.set)

;(def input (slurp "sample.txt"))
(def input (slurp "input.txt"))

(def elevation (->> (clojure.string/split input #"\n")
                    (map #(clojure.string/split % #""))
                    (vec)))

(defn find-value [grid value]
  (->> (map-indexed (fn [idx line] [idx (.indexOf line value)]) grid)
       (filter #(<= 0 (second %)))
       (flatten)))

(defn get-elevation [grid pos]
  (let [grid-char (get-in grid pos)]
    (cond
      (= grid-char "S") (int \a)
      (= grid-char "E") (int \z)
      :else (int (.charAt (get-in grid pos) 0)))))

(defn get-neighbors [grid pos going-backwards]
  (let [y (first pos)
        x (second pos)
        height (count grid)
        width (count (first grid))
        pos-elevation (get-elevation grid pos)]
    (->> [
          [(- y 1) x]
          [(+ y 1) x]
          [y (- x 1)]
          [y (+ x 1)]]
         (filter
           (fn [v]
             (let [new-y (first v)
                   new-x (second v)]
               (cond
                 (< new-y 0) false
                 (< new-x 0) false
                 (>= new-y height) false
                 (>= new-x width) false
                 :else true)))
           )
         (filter
           (fn [neighbor]
             (let [neighbor-elevation (get-elevation grid neighbor)]
               ;(println pos pos-elevation neighbor neighbor-elevation)
               (cond
                 (and (not going-backwards) (<= neighbor-elevation (+ pos-elevation 1))) true
                 (and going-backwards (<= pos-elevation (+ neighbor-elevation 1))) true
                 :else false)))))))

(defn search [grid start queue dist going-backwards]
  (cond
    (zero? (count queue)) dist
    :else (let [pos (reduce (fn [agg next]
                              (cond (< (get-in dist next) (get-in dist agg)) next
                                    :else agg))
                            (first queue)
                            queue)
                pos-dist (get-in dist pos)
                neighbors (get-neighbors grid pos going-backwards)
                [updated-dist neighbors-to-add] (reduce (fn [agg next]
                                                          (let [current-dist (get-in dist next)
                                                                new-dist (+ 1 pos-dist)]
                                                            (cond
                                                              (< new-dist current-dist) [(update-in (first agg) next (constantly new-dist)) (conj (second agg) next)]
                                                              :else agg))) [dist []] neighbors)
                updated-queue (clojure.set/union (disj queue pos) (set neighbors-to-add))]
            ;(println pos updated-queue)
            ;(doseq [line dist] (println line))
            (recur grid start updated-queue updated-dist going-backwards))))

(defn search-shortest-path [grid start going-backwards]
  (let [height (count grid)
        width (count (first grid))
        queue (set [start])
        dist (update-in (vec (repeat height (vec (repeat width 99999999999)))) start (constantly 0))]
    ;(println dist queue width height)
    (search grid start queue dist going-backwards)))

(def start-pos (find-value elevation "S"))
(def end-pos (find-value elevation "E"))
(println start-pos end-pos)

(println "Part 1" (get-in (search-shortest-path elevation start-pos false) end-pos))

(def distances-from-end (search-shortest-path elevation end-pos true))
(def distances-to-a
  (->> elevation
       (map-indexed
         (fn [y line]
           (map-indexed (fn [x val] (cond (= val "a") (get-in distances-from-end [y x])))
                        line)))
       (flatten)
       (filter #(not (nil? %)))))
(println "Part 2" (reduce min distances-to-a))