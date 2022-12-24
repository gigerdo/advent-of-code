(ns day24.day24
  (:require [clojure.set]
            [instaparse.core :as insta]))

;(def input (slurp "sample.txt"))
(def input (slurp "input.txt"))

(def parse
  (insta/parser
    "<S> = line+
    line = tile+ <newline>
    <newline> = #'\n'\n
    <tile> = #'[.#<>^v]'"))
(def parsed-input (parse input))
(def transformed-input
  (insta/transform
    {:line (fn [& x] (vec x))}
    parsed-input))
;(println transformed-input)

(def height (- (count transformed-input) 2))
(def width (- (count (first transformed-input)) 2))
(def blizzards
  (->>
    (map-indexed
      (fn [y line]
        (map-indexed
          (fn [x el]
            (cond
              (not (or (= el ".") (= el "#"))) [[(- y 1) (- x 1)] el]
              :else nil))
          line))
      transformed-input)
    (mapcat identity)
    (filter #(not (nil? %)))
    (set)))
;(println height width blizzards)

(defn move-blizzard [blizzard height width]
  (let [dir (second blizzard)]
    (cond
      (= dir "^") (update-in blizzard [0 0] #(mod (- % 1) height))
      (= dir ">") (update-in blizzard [0 1] #(mod (+ % 1) width))
      (= dir "v") (update-in blizzard [0 0] #(mod (+ % 1) height))
      (= dir "<") (update-in blizzard [0 1] #(mod (- % 1) width)))))
(defn move-blizzards [blizzards height width]
  (let [new-blizzards (map #(move-blizzard % height width) blizzards)]
    new-blizzards))

(def get-blizzard-at-minute
  (memoize
    (fn [minute]
      (cond
        (= minute 0) blizzards
        :else (move-blizzards (get-blizzard-at-minute (- minute 1)) height width)))))

; Pre-calculate some Blizzards
(reduce
  (fn [agg next]
    (get-blizzard-at-minute next))
  (range 300))

(defn out-of-bounds? [pos]
  (let [y (first pos)
        x (second pos)]
    (cond
      (= pos [height (- width 1)]) false
      (= pos [-1 0]) false
      (< x 0) true
      (< y 0) true
      (>= y height) true
      (>= x width) true
      :else false)))

(defn in-blizzard? [blizzard pos]
  (contains? blizzard pos))
(defn move [path]
  (let [minute (get path :minute)
        blizzard (into {} (get-blizzard-at-minute (+ minute 1)))
        pos (get path :pos)
        moves [pos (update pos 0 - 1) (update pos 0 + 1) (update pos 1 - 1) (update pos 1 + 1)]
        valid-moves (filter #(and (not (out-of-bounds? %)) (not (in-blizzard? blizzard %))) moves)
        with-metadata (map (fn [x] {:pos x :minute (+ 1 minute)}) valid-moves)]
    with-metadata))
(defn find-path [end paths]
  (let [shortest-path (reduce
                        (fn [agg next]
                          (cond
                            (< (get next :minute) (get agg :minute)) next
                            :else agg))
                        paths)
        next-steps (move shortest-path)
        new-paths (clojure.set/union (disj paths shortest-path) (set next-steps))]
    ;(println shortest-path)
    ;(println (get shortest-path :minute))
    (cond
      (= (get shortest-path :pos) end) shortest-path
      :else (recur end new-paths))))

(defn print-blizzard [blizzard height width]
  (let [blizzard-map (into {} blizzard)]
    (doseq [y (range height)]
      (doseq [x (range width)]
        (cond
          (in-blizzard? blizzard-map [y x]) (print (get blizzard-map [y x]))
          :else (print ".")))
      (println))
    (println)))

(def start [-1 0])
(def end [height (- width 1)])

(def result (find-path end (hash-set {:pos start :minute 0})))
;(def result {:pos [25 119], :minute 308})
(println "Part 1" result)

(def back-to-start (find-path start (hash-set result)))
(println "Back at start" back-to-start)

(def back-to-goal-again (find-path end (hash-set back-to-start)))
(println "Part 2" back-to-goal-again)