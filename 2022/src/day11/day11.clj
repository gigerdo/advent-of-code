(require 'clojure.set)

;(def input (slurp "sample.txt"))
(def input (slurp "input.txt"))

(def input
  (->> (clojure.string/split input #"\n\n")
       (map #(clojure.string/split % #"\n"))
       (map (fn [lines]
              {:monkey              (-> (get lines 0)
                                        (.charAt 7))
               :items               (as-> (get lines 1) v
                                          (clojure.string/trim v)
                                          (clojure.string/split v #": ")
                                          (drop 1 v)
                                          (first v)
                                          (clojure.string/split v #", ")
                                          (map #(Integer/parseInt %) v))
               :operation           (as-> (get lines 2) v
                                          (clojure.string/trim v)
                                          (clojure.string/split v #" ")
                                          (drop 1 v)
                                          (vec v))
               :divisible-by        (-> (get lines 3)
                                        (clojure.string/split #" ")
                                        (last)
                                        (Integer/parseInt))
               :if-true             (-> (clojure.string/split (get lines 4) #" ")
                                        (last)
                                        (Integer/parseInt))
               :if-false            (-> (clojure.string/split (get lines 5) #" ")
                                        (last)
                                        (Integer/parseInt))
               :num-inspected-items 0}))
       (vec)))

(println input)

(defn give-item-to-monkey [monkeys give-to-monkey-id item]
  ;(println "to monkey" give-to-monkey-id)
  (update-in monkeys [give-to-monkey-id :items] conj item))

(defn expand-param [p old-val]
  (cond
    (= p "old") old-val
    :else (Integer/parseInt p)))

(defn apply-operation [operation old-val]
  ;(println operation old-val)
  (let [first-param (expand-param (get operation 2) old-val)
        op (get operation 3)
        second-param (expand-param (get operation 4) old-val)]
    (cond
      (= op "+") (+ first-param second-param)
      (= op "*") (* first-param second-param)
      :else (throw (Exception. "unknown op")))))

(defn inspect-items [monkeys monkey-id]
  ;(println "monkey" monkey-id)
  (let [monkey (get monkeys monkey-id)
        items (get monkey :items)
        true-monkey (get monkey :if-true)
        false-monkey (get monkey :if-false)
        operation (get monkey :operation)
        divisible-by (get monkey :divisible-by)
        already-inspected-items (get monkey :num-inspected-items)]
    (update-in
      (reduce
        (fn [monkeys item]
          (let [new-worry-level (quot (apply-operation operation item) 3)]
            ;(println "Giving item" monkey-id item new-worry-level)
            (cond
              (= 0 (mod new-worry-level divisible-by)) (give-item-to-monkey monkeys true-monkey new-worry-level)
              :else (give-item-to-monkey monkeys false-monkey new-worry-level)))
          )
        monkeys
        items)
      [monkey-id]
      assoc :items [] :num-inspected-items (+ already-inspected-items (count items)))))

(def result
  (reduce
    (fn [round-agg round]
      ;(println "round" round)
      (reduce
        (fn [monkey-agg monkey]
          (inspect-items monkey-agg monkey))
        round-agg
        (range (count input))))
    input
    (range 20)))

(defn count-monkey-business [result]
  (->> result
       (map #(get % :num-inspected-items))
       (sort)
       (take-last 2)
       (reduce *)))

(println "Part 1" (count-monkey-business result))

(def max-worry (->> input
                    (map #(get % :divisible-by))
                    (reduce *)))
(defn reduce-worry [in]
  (mod in max-worry))

(defn inspect-items-2 [monkeys monkey-id]
  ;(println "monkey" monkey-id)
  (let [monkey (get monkeys monkey-id)
        items (get monkey :items)
        true-monkey (get monkey :if-true)
        false-monkey (get monkey :if-false)
        operation (get monkey :operation)
        divisible-by (get monkey :divisible-by)
        already-inspected-items (get monkey :num-inspected-items)]
    (update-in
      (reduce
        (fn [monkeys item]
          (let [new-worry-level (reduce-worry (apply-operation operation item))]
            ;(println "Giving item" monkey-id item new-worry-level)
            (cond
              (= 0 (mod new-worry-level divisible-by)) (give-item-to-monkey monkeys true-monkey new-worry-level)
              :else (give-item-to-monkey monkeys false-monkey new-worry-level)))
          )
        monkeys
        items)
      [monkey-id]
      assoc :items [] :num-inspected-items (+ already-inspected-items (count items)))))

(def result-2
  (reduce
    (fn [round-agg round]
      ;(println "round" round)
      (reduce
        (fn [monkey-agg monkey]
          (inspect-items-2 monkey-agg monkey))
        round-agg
        (range (count input))))
    input
    (range 10000)))


(println "Part 2" (count-monkey-business result-2))

