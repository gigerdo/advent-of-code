;(def input (slurp "sample.txt"))
(def input (slurp "input.txt"))

(def input
  (->> (clojure.string/split input #"\n")
       (map #(clojure.string/split % #" "))
       (vec)))
;(println input)

(def cycles-to-check [20 60 100 140 180 220])

(defn check-signal-strength [agg]
  (let [current-cycle (get agg :cycles)
        register (get agg :register)
        signal-strength (* register current-cycle)]
    ;(println current-cycle register signal-strength)
    (cond
      (.contains cycles-to-check current-cycle) (update agg :signal-strengths conj signal-strength)
      :else agg)))

(defn draw-pixel [agg]
  (let [cycle (get agg :cycles)
        register (get agg :register)
        crt-position (mod cycle 40)]
    (if
      (= crt-position 0) (println))
    (cond
      (and (>= register (- crt-position 1)) (<= register (+ crt-position 1))) (print "#")
      :else (print "."))
    agg))

(defn update-cycle [agg]
  (-> agg
      (draw-pixel)
      (update :cycles + 1)
      (check-signal-strength)))

(def initial-state {:register 1 :cycles 0 :signal-strengths []})
(def result
  (->
    (reduce
      (fn [state instruction]
        (cond
          (= (first instruction) "noop") (update-cycle state)
          (= (first instruction) "addx") (-> state
                                             (update-cycle)
                                             (update-cycle)
                                             (update :register + (Integer/parseInt (second instruction))))))
      initial-state
      input)
    (get :signal-strengths)))
(println)

(println "Part 1" (reduce + 0 result))

(println "Part 2" "see output above")