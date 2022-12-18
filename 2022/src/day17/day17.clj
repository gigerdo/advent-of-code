(ns day17.day17
  (:require [clojure.set]))

;(def input (slurp "sample.txt"))
(def input (slurp "input.txt"))

(def jets (clojure.string/split (clojure.string/trim input) #""))

(def blocks [[[1 1 1 1]]
             [[0 1 0]
              [1 1 1]
              [0 1 0]]
             [[1 1 1]
              [0 0 1]
              [0 0 1]]
             [[1]
              [1]
              [1]
              [1]]
             [[1 1]
              [1 1]]])

;(println blocks)
;(println jets)

(defn print-grid [grid]
  (doseq [line (reverse grid)]
    (doseq [e line]
      (cond
        (= e 0) (print ".")
        :else (print "#")))
    (println)))


(defn block-in-gridspace [pos id]
  (let [block (get blocks id)
        points (map-indexed
                 (fn [y line]
                   (map-indexed
                     (fn [x el]
                       [y x el])
                     line))
                 block)
        update-only (filter #(> (get % 2) 0) (apply concat points))
        updates-in-gridspace (map (fn [x] [(+ (first pos) (first x)) (+ (second pos) (second x))]) update-only)]
    updates-in-gridspace))

(defn collides? [pos id grid]
  (let [block (get blocks id)
        block-height (count block)
        block-width (count (first block))
        y (first pos)
        x (second pos)]
    (cond
      (< x 0) true
      (< y 0) true
      (> (+ x block-width) 7) true
      :else (let [gridspace-blocks (block-in-gridspace pos id)
                  grid-collisions (reduce
                                    (fn [agg next]
                                      (or agg (not (zero? (get-in grid next)))))
                                    false
                                    gridspace-blocks)]
              (cond
                grid-collisions true
                :else false)))))

(defn draw-block [pos id grid]
  (let [updates-in-gridspace (block-in-gridspace pos id)]
    (reduce
      (fn [agg next]
        (update-in agg next (constantly 1)))
      grid
      updates-in-gridspace)))

(defn apply-jet [pos jet]
  (cond
    (= jet ">") (update pos 1 + 1)
    (= jet "<") (update pos 1 - 1)
    :else (throw (Exception. "unknown jet"))))

(defn simulate-block [grid block-id block-pos stack-top num-blocks jets-id cycle-state]
  (let [jet (get jets jets-id)
        new-jet-id (mod (+ jets-id 1) (count jets))
        [skipped-num-blocks skipped-stack-top] (cond
                                                 (and (= new-jet-id 0) (= (first cycle-state) (get grid (- stack-top 1))))
                                                 (let [skip-blocks (- (get cycle-state 2) num-blocks)
                                                       skip-top (- stack-top (get cycle-state 1))
                                                       iterations-to-skip (quot num-blocks skip-blocks)
                                                       blocks-to-skip (* iterations-to-skip skip-blocks)
                                                       top-to-skip (* iterations-to-skip skip-top)]
                                                   (println "skipping" blocks-to-skip top-to-skip)
                                                   [(- num-blocks blocks-to-skip) top-to-skip])
                                                 :else [num-blocks 0])
        current-state (cond
                        (and (= new-jet-id 0) (zero? skipped-stack-top)) (do
                                                                           ;(if (= (first cycle-state) (get grid (- stack-top 1)))
                                                                           ;  (println "block-diff" (- num-blocks (get cycle-state 2)) "stack-diff" (- stack-top (get cycle-state 1))))
                                                                           [(get grid (- stack-top 1)) stack-top num-blocks])
                        (not (zero? skipped-stack-top)) (update cycle-state 3 (constantly skipped-stack-top))
                        :else cycle-state)
        jetted-pos (apply-jet block-pos jet)
        jetted-pos-collides (collides? jetted-pos block-id grid)
        previous-pos (cond
                       jetted-pos-collides block-pos
                       :else jetted-pos)
        next-pos (update previous-pos 0 - 1)
        next-pos-collides (collides? next-pos block-id grid)]
    ;(println block-pos jetted-pos next-pos jet)
    ;(println stack-top)
    (cond
      (zero? skipped-num-blocks) (do
                                   (+ stack-top (get cycle-state 3 0)))
      next-pos-collides (let [final-pos previous-pos
                              new-grid (draw-block final-pos block-id grid)
                              block-height (count (get blocks block-id))
                              new-stack-top (max (+ (get final-pos 0) block-height) stack-top)
                              next-block [(+ new-stack-top 3) 2]
                              next-block-id (mod (+ block-id 1) (count blocks))
                              extended-grid (cond
                                              (>= (+ new-stack-top 10) (count new-grid))
                                              (vec (concat new-grid (vec (repeat 10 (vec (repeat 7 0))))))
                                              :else new-grid)]
                          ;(print-grid (draw-block next-block next-block-id extended-grid))
                          ;(println)
                          (recur
                            extended-grid
                            next-block-id
                            next-block
                            new-stack-top
                            (- skipped-num-blocks 1)
                            new-jet-id
                            current-state))
      :else (recur grid block-id next-pos stack-top skipped-num-blocks new-jet-id current-state))))

; grid is inverted with the bottom at the top, blocks falling up
(def grid (vec (repeat 10 (vec (repeat 7 0)))))

(println "Part 1" (simulate-block grid 0 [3 2] 0 2022 0 []))

(println "Part 2" (simulate-block grid 0 [3 2] 0 1000000000000 0 []))