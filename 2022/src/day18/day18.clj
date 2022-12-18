(ns day18.day18
  (:require [clojure.set]))

;(def input (slurp "sample.txt"))
(def input (slurp "input.txt"))

(def blocks (->> (clojure.string/split (clojure.string/trim input) #"\n")
                 (map (fn [x] (clojure.string/split x #",")))
                 (map (fn [x] (map (fn [y] (Integer/parseInt y)) x)))))

(def block-set (into #{} blocks))

(defn get-block [pos]
  (cond
    (contains? block-set pos) 0
    :else 1))

(def surface (reduce
               (fn [agg next]
                 (let [x (nth next 0)
                       y (nth next 1)
                       z (nth next 2)
                       top (get-block [x y (+ z 1)])
                       bottom (get-block [x y (- z 1)])
                       left (get-block [(- x 1) y z])
                       right (get-block [(+ x 1) y z])
                       front (get-block [x (+ y 1) z])
                       back (get-block [x (- y 1) z])
                       sides (+ top bottom left right front back)]
                   (+ agg sides)))
               0
               blocks))

(println "Part 1" surface)

(defn get-block-2 [pos]
  (cond
    (contains? block-set pos) 1
    :else 0))

(defn in-bounds? [p]
  (let [x (nth p 0)
        y (nth p 1)
        z (nth p 2)]
    (cond
      (< x -1) false
      (< y -1) false
      (< z -1) false
      (> x 20) false
      (> y 20) false
      (> z 20) false
      :else true)))

(defn fill [visited to-visit surface]
  (cond
    (empty? to-visit) surface
    :else
    (let [next (first to-visit)
          rest (disj to-visit next)
          x (nth next 0)
          y (nth next 1)
          z (nth next 2)
          top [x y (+ z 1)]
          bottom [x y (- z 1)]
          left [(- x 1) y z]
          right [(+ x 1) y z]
          front [x (+ y 1) z]
          back [x (- y 1) z]
          directions (filter in-bounds? [top bottom left right front back])
          visible-blocks (reduce + (map get-block-2 directions))
          not-yet-visited (filter #(and (zero? (get-block-2 %)) (not (contains? visited %))) directions)
          new-to-visit (reduce conj rest not-yet-visited)
          new-visited (conj visited next)]
      (recur new-visited new-to-visit (+ surface visible-blocks)))))

(println "Part 2" (fill (hash-set) (hash-set [0 0 0]) 0))