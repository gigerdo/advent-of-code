(ns day25.day25
  (:require [clojure.set]))

;(def input (slurp "sample.txt"))
(def input (slurp "input.txt"))

(def numbers
  (->> (clojure.string/split input #"\n")
       (map (fn [x] (clojure.string/split x #"")))
       (map
         (fn [x]
           (map
             (fn [y]
               (cond
                 (= y "2") 2
                 (= y "1") 1
                 (= y "0") 0
                 (= y "-") -1
                 (= y "=") -2))
             x)))))

(defn to-decimal [x]
  (cond
    (empty? x) 0
    :else
    (let [len (count x)
          digit (first x)
          rest (drop 1 x)
          factor (Math/pow 5 (dec len))]
      (+ (* digit factor) (to-decimal rest)))))
(defn snafu-transform [x carryover result]
  ;(println x carryover result)
  (cond
    (empty? x) result
    :else
    (let [e1 (get x (dec (count x)))
          e2 (+ carryover e1)
          r (vec (drop-last 1 x))]
      (cond
        (>= e2 3) (snafu-transform r 1 (conj result (- e2 5)))
        :else (snafu-transform r 0 (conj result e2))))))
(defn to-snafu [x idx result]
  (let [factor (long (Math/pow 5 idx))
        d (quot x factor)
        remaining (- x (* d factor))
        r (- x remaining)]
    ;(println x factor r )
    (cond
      (<= idx 0) (snafu-transform (vec (conj result r)) 0 (seq []))
      :else (to-snafu remaining (dec idx) (conj result d)))))

(def decs (map to-decimal numbers))
(def total (reduce + decs))
(def snafu (to-snafu (long total) 20 []))


(println "Part 1")
(println "Decimal" (long total))
(print "SNAFU ")
(doseq [s snafu]
  (cond
    (= -2 s) (print "=")
    (= -1 s) (print "-")
    :else (print s)))

(println)
(println "Back to decimal" (long (to-decimal snafu)))

