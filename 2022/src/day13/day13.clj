(require 'clojure.set)

;(def input (slurp "sample.txt"))
(def input (slurp "input.txt"))

(def packets (->> (clojure.string/split input #"\n\n")
                  (map #(clojure.string/split % #"\n"))
                  (map (fn [pair]
                         (map (fn [packet]
                                (eval (read-string (clojure.string/replace packet #"," " "))))
                              pair)))
                  (vec)))

(defn in-order? [left right]
  ;(println left right)
  (cond
    ; Check if one list is shorter than the other
    (and (empty? left) (not (empty? right))) :smaller
    (and (not (empty? left)) (empty? right)) :greater
    (and (empty? left) (empty? right)) :equal
    :else
    (let [left-val (first left)
          left-rest (drop 1 left)
          right-val (first right)
          right-rest (drop 1 right)]
      (let [comparison (cond
                         ; Check if elements are equal or further checking is necessary
                         (and (integer? left-val) (integer? right-val)) (cond
                                                                          (< left-val right-val) :smaller
                                                                          (= left-val right-val) :equal
                                                                          :else :greater)
                         (integer? left-val) (in-order? [left-val] right-val)
                         (integer? right-val) (in-order? left-val [right-val])
                         :else (in-order? left-val right-val))]
        (cond
          (= comparison :equal) (recur left-rest right-rest)
          :else comparison)))))

(def result
  (map
    (fn [next]
      (in-order? (first next) (second next)))
    packets))

(def sum-of-indices
  (->> result
       (map-indexed (fn [idx val] [(+ idx 1) val]))
       (filter #(= (second %) :smaller))
       (map first)
       (reduce +)))

(println "Part 1" sum-of-indices)

(def divider-1 [[2]])
(def divider-2 [[6]])
(def all-packets (concat (mapcat set packets) [divider-1 divider-2]))

(def sorted-packets
  (sort
    (fn [a b] (let [result (in-order? a b)]
                (cond (= result :smaller) -1
                      (= result :greater) 1
                      :else 0)))
    all-packets))

(def sum-of-indices-2
  (->> sorted-packets
       (map-indexed (fn [idx val] [(+ idx 1) val]))
       (filter (fn [x]
                 ;(println x (in-order? (second x) divider-1))
                 (or (= (in-order? (second x) divider-1) :equal) (= (in-order? (second x) divider-2) :equal))))
       (map first)
       (reduce *)))

(println "Part 2" sum-of-indices-2)