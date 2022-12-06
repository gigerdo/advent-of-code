;(def input (slurp "sample.txt"))
(def input (slurp "input.txt"))
;(def input (slurp "aoc_2022_day05_large_input.txt"))


(def input-parts (clojure.string/split input #"\n\n"))

(def crates-raw (clojure.string/split (first input-parts) #"\n"))

(def num-stacks (Integer/parseInt
                  (last
                    (clojure.string/split (last crates-raw) #" "))))

(defn extract-crate-stack [line stack-index]
  (let [location (+ 1 (* stack-index 4))]
    (get line location)))

(def crates-to-parse (reverse (drop-last crates-raw)))

(map
  (fn [stack-id]
    (map #(extract-crate-stack % stack-id) crates-to-parse))
  (range num-stacks))

(def crates
  (into [] (map
             (fn [stack-id]
               (remove #(or (= % \space) (= % nil))
                       (map #(extract-crate-stack % stack-id) crates-to-parse)))
             (range num-stacks))))

(def moves
  (->> (clojure.string/split (second input-parts) #"\n")
       (map #(clojure.string/split % #" "))
       (map (fn [line]
              (let [[_ a _ b _ c] line]
                (list
                  (Integer/parseInt a)
                  (- (Integer/parseInt b) 1)
                  (- (Integer/parseInt c) 1)))))))

(defn make-move [crates input]
  (let [[num from to] input]
    (let [from-list (get crates from)
          crates-to-move (take-last num from-list)
          new-from-list (drop-last num from-list)
          new-to-list (concat (get crates to) (reverse crates-to-move))]
      (assoc
        (assoc crates from new-from-list)
        to new-to-list))))

(def result
  (reduce (fn [acc next-value]
            ;(println acc)
            (make-move acc next-value)) crates moves))

(println "Part 1" (clojure.string/join "" (map #(last %) result)))

(defn make-move-9001 [crates input]
  (let [[num from to] input]
    (let [from-list (get crates from)
          crates-to-move (take-last num from-list)
          new-from-list (drop-last num from-list)
          new-to-list (concat (get crates to) crates-to-move)]
      (assoc
        (assoc crates from new-from-list)
        to new-to-list))))

(def result-2
  (reduce (fn [acc next-value] (make-move-9001 acc next-value)) crates moves))

(println "Part 2" (clojure.string/join "" (map #(last %) result-2)))