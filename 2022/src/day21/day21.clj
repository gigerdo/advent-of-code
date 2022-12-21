(ns day21.day21
  (:require [clojure.set]
            [instaparse.core :as insta]))

;(def input (slurp "sample.txt"))
(def input (slurp "input.txt"))

(def parse
  (insta/parser
    "<S> = (statement <newline>)+
    statement = id <': '> (number|operation)
    operation = id <' '> op <' '> id
    <id> = #'[a-z]+'
    <op> = #'[+-/*]'
    <newline> = #'\n'
    number = #'[-]?[0-9]+'"))

(def operations (parse input))

(def operation-map (into {} (map (fn [x] [(get x 1) (get x 2)]) operations)))
;(println operation-map)

(def calculate
  (memoize
    (fn [statements id]
      (let [statement (get statements id)]
        (cond
          (= (first statement) :number) (Long/parseLong (second statement))
          :else (let [operation (get statement 2)
                      a (get statement 1)
                      evaluated-a (calculate statements a)
                      b (get statement 3)
                      evaluated-b (calculate statements b)
                      result (cond
                               (= operation "=") (= evaluated-a evaluated-b)
                               (= operation "+") (+ evaluated-a evaluated-b)
                               (= operation "-") (- evaluated-a evaluated-b)
                               (= operation "*") (* evaluated-a evaluated-b)
                               (= operation "/") (quot evaluated-a evaluated-b)
                               :else (throw (Exception. "unknown operation")))]
                  ;(println a operation b " => " evaluated-a operation evaluated-b " => "  result)
                  result))))))

(println "Part 1" (calculate operation-map "root"))

(def calculate-2
  (memoize
    (fn [statements id expected-value]
      (let [statement (get statements id)]
        (cond
          (= id "humn") (cond
                          (not (nil? expected-value)) (do
                                                        ;(println "human is" expected-value)
                                                        [true expected-value])
                          :else [true -1])
          (= (first statement) :number) [false (Long/parseLong (second statement))]
          :else
          (let [operation (get statement 2)
                a (get statement 1)
                [a-has-humn evaluated-a] (calculate-2 statements a nil)
                b (get statement 3)
                [b-has-humn evaluated-b] (calculate-2 statements b nil)]
            ;(println a operation b a-has-humn b-has-humn evaluated-b evaluated-b expected-value)
            (cond
              (and (or a-has-humn b-has-humn) (= expected-value nil)) [true -1]
              a-has-humn (cond
                           (= operation "=") (calculate-2 statements a evaluated-b)
                           (= operation "+") (calculate-2 statements a (- expected-value evaluated-b))
                           (= operation "-") (calculate-2 statements a (+ expected-value evaluated-b))
                           (= operation "*") (calculate-2 statements a (quot expected-value evaluated-b))
                           (= operation "/") (calculate-2 statements a (* expected-value evaluated-b)))
              b-has-humn (cond
                           (= operation "=") (calculate-2 statements b evaluated-a)
                           (= operation "+") (calculate-2 statements b (- expected-value evaluated-a))
                           (= operation "-") (calculate-2 statements b (- evaluated-a expected-value))
                           (= operation "*") (calculate-2 statements b (quot expected-value evaluated-a))
                           (= operation "/") (calculate-2 statements b (quot evaluated-a expected-value)))
              (= operation "=") [false (= evaluated-a evaluated-b)]
              (= operation "+") [false (+ evaluated-a evaluated-b)]
              (= operation "-") [false (- evaluated-a evaluated-b)]
              (= operation "*") [false (* evaluated-a evaluated-b)]
              (= operation "/") [false (quot evaluated-a evaluated-b)]
              :else (throw (Exception. "unknown operation")))))))))
(def operations-map-2 (update-in operation-map ["root" 2] (constantly "=")))

(def result (calculate-2 operations-map-2 "root" -1))
(println "Part 2" (second result))

; Test if the calculated humn results in true using the part-1 calculation
(def operations-map-3 (update-in operations-map-2 ["humn" 1] (constantly (.toString (second result)))))
(println "test-result" (calculate operations-map-3 "root"))