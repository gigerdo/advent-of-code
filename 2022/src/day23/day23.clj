(ns day23.day23
  (:require [clojure.set]
            [instaparse.core :as insta]))

;(def input (slurp "sample.txt"))
(def input (slurp "input.txt"))

(def parse
  (insta/parser
    "<S> = line+
    line = tile+ <newline>
    <newline> = #'\n'\n
    <tile> = #'[.#]'"))
(def parsed-input (parse input))
(def transformed-input
  (insta/transform
    {:line (fn [& x] (vec x))}
    parsed-input))
;(println transformed-input)

(def elves
  (->>
    (map-indexed
      (fn [y line]
        (map-indexed
          (fn [x el]
            (cond
              (= el "#") [y x]
              :else nil))
          line))
      transformed-input)
    (mapcat identity)
    (filter #(not (nil? %)))
    (set)))

;(println elves)

(defn find-min-max [elves]
  (let [iter
        (fn [rem min-y max-y min-x max-x]
          (cond
            (empty? rem) {:min-y min-y :max-y max-y :min-x min-x :max-x max-x}
            :else
            (let [el (first rem)
                  y (first el)
                  x (second el)
                  nmin-y (min min-y y)
                  nmax-y (max max-y y)
                  nmin-x (min min-x x)
                  nmax-x (max max-x x)]
              (recur (drop 1 rem) nmin-y nmax-y nmin-x nmax-x))))]
    (iter elves 9999 -9999 9999 -9999)))

(defn print-elves [elves]
  (let [minmax (find-min-max elves)]
    (doseq [y (range (get minmax :min-y) (inc (get minmax :max-y)))]
      (doseq [x (range (get minmax :min-x) (inc (get minmax :max-x)))]
        (cond
          (contains? elves [y x]) (print "#")
          :else (print ".")))
      (println)))
  (println))

(defn contains-elves? [elves pos dirs]
  (reduce
    (fn [agg next] (or agg next))
    false
    (map
      (fn [dir]
        (cond
          (= dir "NW") (contains? elves (update (update pos 0 - 1) 1 - 1))
          (= dir "N") (contains? elves (update pos 0 - 1))
          (= dir "NE") (contains? elves (update (update pos 0 - 1) 1 + 1))
          (= dir "E") (contains? elves (update pos 1 + 1))
          (= dir "SE") (contains? elves (update (update pos 0 + 1) 1 + 1))
          (= dir "S") (contains? elves (update pos 0 + 1))
          (= dir "SW") (contains? elves (update (update pos 0 + 1) 1 - 1))
          (= dir "W") (contains? elves (update pos 1 - 1))))
      dirs)))

(defn move-in-dir [elves pos dir]
  (cond
    (not (contains-elves? elves pos ["NW" "N" "NE" "E" "SE" "S" "SW" "W"])) pos
    (and (= dir 0) (not (contains-elves? elves pos ["N" "NE" "NW"]))) (update pos 0 - 1)
    (and (= dir 1) (not (contains-elves? elves pos ["S" "SE" "SW"]))) (update pos 0 + 1)
    (and (= dir 2) (not (contains-elves? elves pos ["W" "NW" "SW"]))) (update pos 1 - 1)
    (and (= dir 3) (not (contains-elves? elves pos ["E" "NE" "SE"]))) (update pos 1 + 1)
    :else nil))


(defn try-all-directions [elves pos start-dir]
  (let [new-pos (reduce
                  (fn [agg next]
                    (cond
                      (nil? agg) (move-in-dir elves pos (mod next 4))
                      :else agg))
                  nil
                  (range start-dir (+ start-dir 4)))]
    (cond
      (nil? new-pos) pos
      :else new-pos)))

(defn move [elves round start-direction]
  (cond
    (= round 10) elves
    :else
    (let [new-pos (map
                    (fn [pos]
                      [pos (try-all-directions elves pos start-direction)])
                    elves)
          new-pos-freqs (frequencies (map second new-pos))
          filtered-new-pos (map
                             (fn [x]
                               (cond
                                 (> (get new-pos-freqs (second x)) 1) (first x)
                                 :else (second x)))
                             new-pos)
          next-direction (mod (+ start-direction 1) 4)]
      ;(print-elves elves)
      (recur (set filtered-new-pos) (+ round 1) next-direction))))


(def result (move elves 0 0))

(defn count-ground [elves]
  (let [minmax (find-min-max elves)
        ground-count (for [y (range (get minmax :min-y) (inc (get minmax :max-y)))]
                       (for [x (range (get minmax :min-x) (inc (get minmax :max-x)))]
                         (cond
                           (contains? elves [y x]) 0
                           :else 1)))]
    (reduce + (flatten ground-count))))
;(print-elves result)
(println "Part 1" (count-ground result))

(defn move-2 [elves round start-direction]
  (let [new-pos (map
                  (fn [pos]
                    [pos (try-all-directions elves pos start-direction)])
                  elves)
        new-pos-freqs (frequencies (map second new-pos))
        filtered-new-pos (set (map
                                (fn [x]
                                  (cond
                                    (> (get new-pos-freqs (second x)) 1) (first x)
                                    :else (second x)))
                                new-pos))
        next-direction (mod (+ start-direction 1) 4)]
    ;(print-elves elves)
    (cond
      (= elves filtered-new-pos) [elves (+ round 1)]
      :else
      (recur filtered-new-pos (+ round 1) next-direction))))

(def result-2 (move-2 elves 0 0))

(println "Part 2" (second result-2))