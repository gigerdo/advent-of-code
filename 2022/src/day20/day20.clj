(ns day20.day20
  (:require [clojure.set]))

;(def input (slurp "sample.txt"))
(def input (slurp "input.txt"))

(def numbers (->> (clojure.string/split (clojure.string/trim input) #"\n")
                  (map-indexed (fn [idx x] [(Integer/parseInt x) idx]))))

(defn swap [numbers idxa idxb]
  (let [wrapped-idxa (mod idxa (count numbers))
        wrapped-idxb (mod idxb (count numbers))
        element-a (get numbers wrapped-idxa)
        element-b (get numbers wrapped-idxb)
        temp (assoc! numbers wrapped-idxa element-b)
        final-numbers (assoc! temp wrapped-idxb element-a)]
    ;(println "swap" wrapped-idxa wrapped-idxb)
    final-numbers))

; First solution used swap, but it is too slow
(defn move-one-swap [numbers from offset]
  ;(println numbers from offset)
  (cond
    (zero? offset) numbers
    (< offset 0)
    (reduce
      (fn [agg next]
        (swap agg next (- next 1)))
      numbers
      (reverse (range (+ from offset 1) (+ from 1))))
    :else
    (reduce
      (fn [agg next]
        (swap agg next (+ next 1)))
      numbers
      (range from (+ from offset)))))

(defn find-number [numbers idx i]
  (cond
    (= idx (second (get numbers i))) [i (get numbers i)]
    :else (recur numbers idx (inc i))))

; Move element from to to
(defn move-one [numbers from to]
  (let [element (get numbers from)
        removed (vec (concat (subvec numbers 0 from)
                             (subvec numbers (+ from 1))))
        added (vec (concat
                     (subvec removed 0 to)
                     [element]
                     (subvec removed to)))]
    ;(println numbers from to element)
    added))


(defn swap-numbers [numbers idx]
  ;(println numbers)
  (cond
    (= idx (count numbers)) numbers
    :else
    (let [[real-idx number] (find-number numbers idx 0)
          offset (get number 0)
          to-idx (mod (+ real-idx offset) (- (count numbers) 1))]
      (recur (move-one numbers real-idx to-idx) (inc idx)))))
;(recur (move-one-swap numbers real-idx offset) (inc idx)))))

(def result (swap-numbers (vec numbers) 0))
;(println result)

(defn find-value [numbers val i]
  (cond
    (= val (first (get numbers i))) i
    :else (recur numbers val (inc i))))
(defn find-grove-coord [result]
  (let [idx-0 (find-value result 0 0)
        idx-a (mod (+ idx-0 1000) (count result))
        idx-b (mod (+ idx-0 2000) (count result))
        idx-c (mod (+ idx-0 3000) (count result))
        a (first (get result idx-a))
        b (first (get result idx-b))
        c (first (get result idx-c))]
    (+ a b c)))

(println "Part 1" (find-grove-coord result))
; Part 1 7228
(def decryption-key 811589153)
(def decrypted-numbers (map (fn [x] (update x 0 * decryption-key)) numbers))

(def result-2
  (reduce
    (fn [agg next]
      (println next)
      (swap-numbers agg 0))
    (vec decrypted-numbers)
    (range 10)))

(println "Part 2" (find-grove-coord result-2))
; Part 2 4526232706281