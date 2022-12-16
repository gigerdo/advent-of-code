(ns day16.day16
  (:require [clojure.math.combinatorics :as combo]
            [clojure.set]
            [instaparse.core :as insta]))

;(def input (slurp "sample.txt"))
(def input (slurp "input.txt"))

(def parse
  (insta/parser
    "<S> = <'Valve '> id <' has flow rate='> number <tunnel-text> leads-to
    tunnel-text = ('; tunnels lead to valves ' | '; tunnel leads to valve ')
    <leads-to> = (id <', '>)* id
    <id> = #'[A-Z]*'
     <number> = #'[-]?[0-9]+'"))

(def tunnels (->> (clojure.string/split input #"\n")
                  (map parse)
                  (map vec)))

(def tunnel-map (->> tunnels
                     (map (fn [x] [
                                   (first x)
                                   {:rate     (Integer/parseInt (second x))
                                    :leads-to (drop 2 x)}]))
                     (into {})))

(println tunnel-map)


(defn find-shortest-paths [graph queue dist]
  (cond
    (zero? (count queue)) dist
    :else (let [node-id (reduce (fn [agg next]
                                  (cond (< (get dist next) (get dist agg)) next
                                        :else agg))
                                (first queue)
                                queue)
                node (get graph node-id)
                node-dist (get dist node-id)
                neighbors (get node :leads-to)
                [updated-dist neighbors-to-add] (reduce (fn [agg next]
                                                          (let [current-dist (get dist next 99999)
                                                                new-dist (+ 1 node-dist)]
                                                            ;(println node-id next current-dist new-dist)
                                                            (cond
                                                              (< new-dist current-dist) [(update (first agg) next (constantly new-dist)) (conj (second agg) next)]
                                                              :else agg)))
                                                        [dist []]
                                                        neighbors)
                updated-queue (clojure.set/union (disj queue node-id) (set neighbors-to-add))]
            ;(println node node-dist neighbors)
            (recur graph updated-queue updated-dist))))

(def valve-paths
  (into {} (map (fn [key]
                  (let [shortest-paths (find-shortest-paths tunnel-map (set [key]) {key 0})
                        no-flow-zero (into {}
                                           (filter
                                             (fn [x]
                                               (and (not (= (second x) 0))
                                                    (> (get-in tunnel-map [(first x) :rate]) 0)))
                                             shortest-paths))]
                    [key no-flow-zero]))
                (filter (fn [key] (or (= key "AA") (> (get-in tunnel-map [key :rate]) 0))) (keys tunnel-map)))))

(println "paths" valve-paths)

(def graph valve-paths)
(def valves tunnel-map)

(def visit-tunnel
  (memoize (fn [valve-id visited-valves remaining-minutes]
             (let [current-valve (get valves valve-id)
                   current-pressure (get current-valve :rate)
                   to-valves (get graph valve-id)
                   to-visit (apply dissoc to-valves visited-valves)
                   added-pressure (* current-pressure remaining-minutes)]
               ;(println valve-id visited-valves to-visit new-pressure total-pressure)
               (cond
                 (<= remaining-minutes 0) [0 visited-valves]
                 (empty? to-visit) [added-pressure visited-valves]
                 :else
                 (reduce
                   (fn [agg next]
                     (let [next-id (first next)
                           distance (+ (second next) 1)     ; add one minute to open valve in target
                           sub-result (visit-tunnel
                                        next-id
                                        (conj visited-valves next-id)
                                        (- remaining-minutes distance))
                           total-pressure (+ added-pressure (first sub-result))]
                       (cond
                         (> total-pressure (first agg)) [total-pressure (second sub-result)]
                         :else agg)))
                   [0 []]
                   to-visit))))))

(println "Part 1" (visit-tunnel "AA" (set ["AA"]) 30))

(def num-valves (count graph))

(def subsets (combo/subsets (keys (dissoc graph "AA"))))

;(println "Total subsets" (count subsets))

(def result-2
  (time (reduce
          (fn [agg x]
            (let [person (visit-tunnel "AA" (conj (set x) "AA") 26)
                  elephant (visit-tunnel "AA" (clojure.set/difference (set (second person)) x) 26)
                  total (+ (first person) (first elephant))
                  ]
              ;(println (max agg total) x total person elephant)
              (max agg total)))
          0
          subsets)))

(println "Part 2" result-2)