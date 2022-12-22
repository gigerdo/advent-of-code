(ns day22.day22
  (:require [clojure.set]
            [instaparse.core :as insta]))

;(def input (slurp "sample.txt"))
(def input (slurp "input.txt"))

(def parse
  (insta/parser
    "<S> = map <newline> path
    map = line+
    line = tile+ <newline>
    <tile> = #'[ .#]'
    path = direction+ <newline>
    <direction> = (number|rotation)
    rotation = #'[RL]'
    <newline> = #'\n'
    number = #'[-]?[0-9]+'"))

(def parsed-input (parse input))

(def transformed-input
  (insta/transform
    {:map      (fn [& lines] (vec lines))
     :path     (fn [& x] (vec x))
     :line     (fn [& x] (vec x))
     :number   (fn [x] (Integer/parseInt x))
     :rotation (fn [x] x)}
    parsed-input))
;(println transformed-input)

(defn increase-pos [pos dir height width]
  (let [y (get pos 0)
        x (get pos 1)
        diry (get dir 0)
        dirx (get dir 1)
        newy (mod (+ y diry) height)
        newx (mod (+ x dirx) width)]
    (assoc pos 0 newy 1 newx)))

(defn wrap-pos [grid directions pos]
  (let [height (count grid)
        width (reduce (fn [agg x] (max agg (count x))) 0 grid)
        dir (get directions (get pos 2))
        new-pos (increase-pos pos dir height width)
        block (get-in grid [(first new-pos) (second new-pos)])
        empty-block (or (= block " ") (nil? block))]
    (cond
      empty-block (wrap-pos grid directions new-pos)
      :else new-pos)))

(defn print-grid [grid pos]
  (doseq [line (update-in grid [(first pos) (second pos)] (constantly "p"))]
    (doseq [e line]
      (print e))
    (println)))
(defn move-on-grid [grid path directions pos]
  ;(println pos path)
  ;(print-grid grid pos)
  (cond
    (zero? (count path)) pos
    :else
    (let [move (first path)
          remaining-path (drop 1 path)
          [new-pos new-path]
          (cond
            (= move "L") [(update pos 2 #(mod (- % 1) 4)) remaining-path]
            (= move "R") [(update pos 2 #(mod (+ % 1) 4)) remaining-path]
            (= move 0) [pos remaining-path]
            :else
            (let [
                  new-pos (wrap-pos grid directions pos)
                  block (get-in grid [(first new-pos) (second new-pos)])]
              (cond
                (= block "#") [pos remaining-path]
                :else [new-pos (conj remaining-path (- move 1))])))]
      (recur grid new-path directions new-pos))))

(def grid (first transformed-input))
(def start (.indexOf (get grid 0) "."))

(def directions [[0 1]
                 [1 0]
                 [0 -1]
                 [-1 0]])

(def result (move-on-grid grid (second transformed-input) directions [0 start 0]))

(def password (+ (* 1000 (+ (first result) 1)) (* 4 (+ (second result) 1)) (nth result 2)))
(println "Part 1" result password)

(defn in-range [min-y max-y min-x max-x y x]
  (cond
    (and (>= y min-y) (< y max-y) (>= x min-x) (< x max-x)) true
    :else false))
(defn wrap-pos-cubes [grid directions pos]
  ;(println pos)
  (let [y (get pos 0)
        x (get pos 1)
        dir (get directions (get pos 2))
        diry (get dir 0)
        dirx (get dir 1)
        newy (+ y diry)
        newx (+ x dirx)]
    (cond
      ; C UP
      (in-range -1 0 50 100 newy newx) (assoc pos 0 (+ newx 100) 1 0 2 0)
      ; D UP
      (in-range -1 0 100 150 newy newx) (assoc pos 0 199 1 (- x 100) 2 3)
      ; D RIGHT
      (in-range 0 50 150 151 newy newx) (assoc pos 0 (- 149 newy) 1 99 2 2)
      ; D DOWN
      (in-range 50 51 100 150 newy newx) (assoc pos 0 (- x 50) 1 99 2 2)
      ; B RIGHT
      (in-range 50 100 100 151 newy newx) (assoc pos 0 49 1 (+ y 50) 2 3)
      ; A RIGHT
      (in-range 100 150 100 151 newy newx) (assoc pos 0 (- 49 (- newy 100)) 1 149 2 2)
      ; A DOWN
      (in-range 150 151 50 100 newy newx) (assoc pos 0 (+ newx 100) 1 49 2 2)
      ; F RIGHT
      (in-range 150 200 50 51 newy newx) (assoc pos 0 149 1 (- newy 100) 2 3)
      ; F DOWN
      (in-range 200 201 0 150 newy newx) (assoc pos 0 0 1 (+ newx 100) 2 1)
      ; F LEFT
      (in-range 150 200 -1 0 newy newx) (assoc pos 0 0 1 (- newy 100) 2 1)
      ; E LEFT
      (in-range 100 150 -1 0 newy newx) (assoc pos 0 (- 49 (- newy 100)) 1 50 2 0)
      ; E UP
      (in-range 99 100 0 50 newy newx) (assoc pos 0 (+ newx 50) 1 50 2 0)
      ; B LEFT
      (in-range 50 100 49 50 newy newx) (assoc pos 0 100 1 (- newy 50) 2 1)
      ; C LEFT
      (in-range 0 50 49 50 newy newx) (assoc pos 0 (+ 100 (- 49 newy)) 1 0 2 0)
      ; nowrap
      :else (assoc pos 0 newy 1 newx))))

(defn check [in out]
  (let [result (wrap-pos-cubes grid directions in)]
    (println in "=>" result "=" out "?" (= result out))))

; Unit tests for the transitions
(check [0 55 3] [155 0 0])
(check [0 105 3] [199 5 3])
(check [10 149 0] [139 99 2])
(check [49 110 1] [60 99 2])
(check [60 99 0] [49 110 3])
(check [139 99 0] [10 149 2])
(check [149 60 1] [160 49 2])
(check [160 49 0] [149 60 3])
(check [160 0 2] [0 60 1])
(check [120 0 2] [29 50 0])
(check [100 20 3] [70 50 0])
(check [70 50 2] [100 20 1])
(check [29 50 2] [120 0 0])


(defn move-on-grid-2 [grid path directions pos]
  ;(println pos path)
  ;(print-grid grid pos)
  (cond
    (zero? (count path)) pos
    :else
    (let [move (first path)
          remaining-path (drop 1 path)
          [new-pos new-path]
          (cond
            (= move "L") [(update pos 2 #(mod (- % 1) 4)) remaining-path]
            (= move "R") [(update pos 2 #(mod (+ % 1) 4)) remaining-path]
            (= move 0) [pos remaining-path]
            :else
            (let [
                  new-pos (wrap-pos-cubes grid directions pos)
                  block (get-in grid [(first new-pos) (second new-pos)])]
              (cond
                (= block "#") [pos remaining-path]
                :else [new-pos (conj remaining-path (- move 1))])))]
      (recur grid new-path directions new-pos))))

;; Only works for the real input
(def result-2 (move-on-grid-2 grid (second transformed-input) directions [0 start 0]))
(def password-2 (+ (* 1000 (+ (first result-2) 1)) (* 4 (+ (second result-2) 1)) (nth result-2 2)))
(println "Part 2" result-2 password-2)