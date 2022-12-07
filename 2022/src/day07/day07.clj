;(def input (slurp "sample.txt"))
(def input (slurp "input.txt"))

(def input
  (->> (clojure.string/split input #"\$")
       (map #(clojure.string/split (clojure.string/trim %) #"\n"))
       (map #(map (fn [line] (clojure.string/split line #" ")) %))))

(def data [["/"] {"/" {}}])

(defn change-dir [state target-dir]
  ;(println "change-dir" target-dir)
  (cond
    (= target-dir "/") [["/"] (second state)]
    (= target-dir "..") [(vec (drop-last 1 (first state))) (second state)]
    :else [(conj (first state) target-dir) (second state)]))

(defn append-to-current-dir [state dir-contents]
  ;(println "append-dir" dir-contents)
  (reduce
    (fn [agg next-content]
      ;(println "adding-single-subelement" (first agg) next-content)
      (update-in
        agg
        (cons 1 (first agg))
        (fn [current-element]
          ;(println "current-element" current-element)
          (cond
            (= (first next-content) "dir") (merge current-element {(second next-content) {}})
            :else (merge current-element {(second next-content) (first next-content)})))))
    state
    dir-contents))

(defn apply-command [current-state command]
  (let [cmd-input (first command)
        cmd-output (rest command)]
    ;(println "Current-state" current-state)
    (cond
      (= (first cmd-input) "cd") (change-dir current-state (second cmd-input))
      (= (first cmd-input) "ls") (append-to-current-dir current-state cmd-output)
      :else (do (println "Ignored command" cmd-input) current-state))))

(def result
  (reduce
    (fn [agg next-command]
      (apply-command agg next-command))
    data
    input))


(defn sum-tree [tree]
  (reduce
    (fn [agg next]
      (let [size (cond
                   (map? (second next)) (let [[sub-size dirs] (sum-tree (second next))]
                                          [sub-size (conj dirs [(first next) sub-size])])
                   :else [(Integer/parseInt (second next)) {}])]
        ;(println "size" size agg)
        [(+ (first agg) (first size)) (concat (second agg) (second size))]))
    [0 {}]
    (seq tree)))

(def summed-trees (sum-tree (second result)))

(def sum-below-10k
  (->> (seq (second summed-trees))
       (map second)
       (filter #(< % 100000))
       (reduce +)))

(println "Part 1" sum-below-10k)

(def used-space (first summed-trees))
(def unused-space (- 70000000 used-space))
(def space-to-free (- 30000000 unused-space))

(def size-of-directory-to-delete
  (->> (seq (second summed-trees))
       (map second)
       (sort)
       (filter #(> % space-to-free))
       (first)))

(println "Part 2" size-of-directory-to-delete)
