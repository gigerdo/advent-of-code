(ns day15.day15
  (:require [instaparse.core :as insta]))

;(def input (slurp "src/day15/sample.txt"))
(def input (slurp "src/day15/input.txt"))

(def parse
  (insta/parser
    "<S> = <'Sensor at x='> number <', y='> number <': closest beacon is at x='> number <', y='> number
     <number> = #'[-]?[0-9]+'"))



(def positions (->> (clojure.string/split input #"\n")
                    (map parse)
                    (map #(map (fn [x] (Integer/parseInt x)) %))
                    (map vec)
                    (map (fn [x] [[(get x 0) (get x 1)] [(get x 2) (get x 3)]]))
                    ))

(println positions)

(defn find-radius [sensor beacon]
  (let [x1 (first sensor)
        x2 (first beacon)
        y1 (second sensor)
        y2 (second beacon)
        distance (+ (abs (- x1 x2)) (abs (- y1 y2)))]
    ;(println sensor beacon distance)
    distance))

(defn find-overlapping-range-at [sensor beacon y]
  (let [radius (find-radius sensor beacon)
        sensor-y (second sensor)
        local-y (abs (- y sensor-y))]
    (cond
      (or (< y (- sensor-y radius)) (> y (+ sensor-y radius))) nil
      :else
      (let [
            sensor-width (+ (* 2 radius) 1)
            width-at-y (- sensor-width (* local-y 2))
            offset (/ (- width-at-y 1) 2)
            sensor-x (first sensor)
            x-start (- sensor-x offset)
            x-end (+ sensor-x offset)]
        ;(println width-at-y offset sensor-x)
        ;(println sensor [x-start x-end] sensor-width local-y)
        [x-start x-end]))))

(defn find-overlapping-ranges [y]
  (->>
    (map #(find-overlapping-range-at (first %) (second %) y) positions)
    (filter #(not (nil? %)))))

(defn overlaps? [a b]
  (let [ax1 (first a)
        ax2 (second a)
        bx1 (first b)
        bx2 (second b)]
    (cond
      (and (>= ax2 bx1) (<= ax2 bx2)) true
      (and (<= ax1 bx1) (>= ax2 bx2)) true
      (and (>= ax1 bx1) (<= ax1 bx2)) true
      :else false)))

(defn combine [a b]
  (let [ax1 (first a)
        ax2 (second a)
        bx1 (first b)
        bx2 (second b)
        min-x1 (min ax1 bx1)
        max-x2 (max ax2 bx2)]
    [min-x1 max-x2]))

(defn merge-single-step [ranges]
  (let [first (first ranges)
        rest (drop 1 ranges)
        overlapping (reduce
                      (fn [agg next]
                        (cond
                          (overlaps? first next) (conj agg next)
                          :else agg))
                      []
                      rest)
        rest-without-overlapping (filter
                                   (fn [x] (not (some (fn [y] (= x y)) overlapping)))
                                   rest)
        combined-first (reduce combine first overlapping)]
    (cond
      (zero? (count rest-without-overlapping)) [combined-first]
      :else (conj (merge-single-step rest-without-overlapping) combined-first))))

(defn merge-ranges [ranges]
  (let [new-ranges (merge-single-step ranges)]
    (cond
      (= (count ranges) (count new-ranges)) new-ranges
      :else (merge-ranges new-ranges))))

;(def result (merge-ranges (find-overlapping-ranges 10)))
(def result (merge-ranges (find-overlapping-ranges 2000000)))

(def result-count (->> result
                       (map #(- (second %) (first %)))
                       (reduce +)))

(println "Part 1" result result-count)

(defn range-difference [a b]
  (cond (empty? a) a
        :else
        (let [ax1 (first a)
              ax2 (second a)
              bx1 (first b)
              bx2 (second b)]
          (cond
            (not (overlaps? a b)) a
            (and (>= bx2 ax2) (<= bx1 ax1)) []
            (and (<= bx1 ax1) (>= bx2 ax1)) [(+ bx2 1) ax2]
            (and (<= bx1 ax2) (>= bx2 ax2)) [ax1 (- bx1 1)]
            :else a))))

(defn check-line [y start-range]
  (let [ranges (find-overlapping-ranges y)
        merged-ranges (merge-ranges ranges)
        result (reduce
                 (fn [agg next] (let [r (range-difference agg next)]
                                  ;(println "diff" agg next r)
                                  r))

                 start-range
                 merged-ranges)]
    ;(println "y=" y "x=" free-spots)
    (cond
      (not (empty? (flatten result))) (do (println y (flatten result)) [(flatten result) y])
      :else [])))

;(def result-2 (map #(check-line % [0 20]) (range 0 20)))
(def result-2 (map #(check-line % [0 4000000]) (range 3000000 4000001)))
;(+ (second result-2) (* (first result-2) 4000000))
(println "Part 2" (flatten result-2))