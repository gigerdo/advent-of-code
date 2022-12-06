;(def input (slurp "sample.txt"))
(def input (slurp "input.txt"))

(def input (clojure.string/trim-newline input))

(defn unique-chars [in]
  (= (count (set in)) (count in)))

(defn find-first-unique [sliding-window]
  (first
    (filter #(second %)
            (map-indexed
              (fn [index group] [index (unique-chars group)])
              sliding-window))))

(def sliding-window (partition 4 1 input))
(def result (find-first-unique sliding-window))

(println "Part 1" result (+ (first result) 4))

(def sliding-window-14 (partition 14 1 input))
(def result-2 (find-first-unique sliding-window-14))

(println "Part 2" result-2 (+ (first result-2) 14))