(ns day19.day19
  (:require [clojure.set]
            [instaparse.core :as insta]))

;(def input (slurp "sample.txt"))
(def input (slurp "input.txt"))

(def parse
  (insta/parser
    "<S> = (blueprint <newline>)+
    blueprint = <'Blueprint '> number <':'> ore clay obsidian geode
    ore = <' Each ore robot costs '> number <' ore.'>
    clay = <' Each clay robot costs '> number <' ore.'>
    obsidian = <' Each obsidian robot costs '> number <' ore and '> number <' clay.'>
    geode = <' Each geode robot costs '> number <' ore and '> number <' obsidian.'>
    newline = #'\n'
    <number> = #'[-]?[0-9]+'"))

(def blueprints (->> (parse input)
                     (map (fn [x]
                            [(second x)
                             (let [r (drop 2 x)]
                               (into {} (map (fn [y] [(first y) (rest y)]) r)))]))))

(println blueprints)

(defn to-integer [l]
  (map (fn [x] (Integer/parseInt x)) l))

(defn buy-robot [state type cost cost-type]
  (update-in
    (update state cost-type - cost)
    [:robots type]
    + 1))

(defn buy-obsidian [state cost-ore cost-clay]
  (update-in
    (update
      (update state :clay - cost-clay)
      :ore - cost-ore)
    [:robots :obsidian]
    + 1))

(defn buy-geode [state cost-ore cost-obsidian]
  (update-in
    (update
      (update state :obsidian - cost-obsidian)
      :ore - cost-ore)
    [:robots :geode]
    + 1))

(defn max-state [a b]
  (cond
    (> (get a :geode) (get b :geode)) a
    :else b))

(def produce
  (memoize
    (fn [costs state minute]
      ;(println minute state)
      (let [ore (get state :ore)
            clay (get state :clay)
            obsidian (get state :obsidian)
            ore-cost (get costs :ore)
            clay-cost (get costs :clay)
            obsidian-ore-cost (get costs :obsidian-ore)
            obsidian-clay-cost (get costs :obsidian-clay)
            geode-ore-cost (get costs :geode-ore)
            geode-obsidian-cost (get costs :geode-obsidian)
            clay-robots (get-in state [:robots :clay])
            obsidian-robots (get-in state [:robots :obsidian])
            new-state (assoc state
                        :ore (+ ore (get-in state [:robots :ore]))
                        :clay (+ clay clay-robots)
                        :obsidian (+ obsidian obsidian-robots)
                        :geode (+ (get state :geode) (get-in state [:robots :geode])))
            new-minute (+ minute 1)]
        (cond
          (= minute (get costs :total-minutes)) state

          (and (>= ore geode-ore-cost) (>= obsidian geode-obsidian-cost))
          (produce costs (buy-geode new-state geode-ore-cost geode-obsidian-cost) new-minute)

          (and (>= ore obsidian-ore-cost) (>= clay obsidian-clay-cost) (<= obsidian-robots geode-obsidian-cost))
          (produce costs (buy-obsidian new-state obsidian-ore-cost obsidian-clay-cost) new-minute)

          :else
          (let [buy-clay (if (and (>= ore clay-cost) (<= clay-robots obsidian-clay-cost)) (produce costs (buy-robot new-state :clay clay-cost :ore) new-minute) state)
                buy-ore (if (>= ore ore-cost) (produce costs (buy-robot new-state :ore ore-cost :ore) new-minute) state)
                dont-buy (produce costs new-state new-minute)]
            (max-state dont-buy (max-state buy-clay buy-ore))))))))

(defn calculate-result [input minutes]
  (map
    (fn [blueprint]
      (let [costs (second blueprint)
            ore-cost (first (to-integer (get costs :ore)))
            clay-cost (first (to-integer (get costs :clay)))
            obsidian-cost (to-integer (get costs :obsidian))
            obsidian-ore-cost (first obsidian-cost)
            obsidian-clay-cost (second obsidian-cost)
            geode-cost (to-integer (get costs :geode))
            geode-ore-cost (first geode-cost)
            geode-obsidian-cost (second geode-cost)
            costs {:ore            ore-cost
                   :clay           clay-cost
                   :obsidian-ore   obsidian-ore-cost
                   :obsidian-clay  obsidian-clay-cost
                   :geode-ore      geode-ore-cost
                   :geode-obsidian geode-obsidian-cost
                   :total-minutes  minutes}
            state {:ore      0
                   :clay     0
                   :obsidian 0
                   :geode    0
                   :robots   {:ore      1
                              :clay     0
                              :obsidian 0
                              :geode    0}}
            result (produce costs state 0)]
        (println costs result)
        result))
    input))

(def part1-result (map-indexed (fn [idx result] (* (+ 1 idx) (get result :geode))) (calculate-result blueprints 24)))
(def quality-level (reduce + part1-result))
(println "Part 1" quality-level)
; Part 1 1150

(def part2-result (map #(get % :geode) (calculate-result (take 3 blueprints) 32)))
(println "Part 2" (reduce * 1 part2-result))
; Part 2 37367