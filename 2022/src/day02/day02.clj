;(def input (slurp "sample.txt"))
(def input (slurp "input.txt"))

(def rock (int 1))
(def paper (int 2))
(def scissors (int 3))

(def plays
  (map
    #(clojure.string/split % #" ")
    (clojure.string/split input #"\n")))

(defn map-opponent [opponent-coded]
  (cond
    (= opponent-coded "A") rock
    (= opponent-coded "B") paper
    (= opponent-coded "C") scissors))

(defn map-you [youCoded]
  (cond
    (= youCoded "X") rock
    (= youCoded "Y") paper
    (= youCoded "Z") scissors))

(defn calculate-score [opponent you]
  (cond
    (and (= opponent rock) (= you scissors)) (+ 0 you)
    (and (= you rock) (= opponent scissors)) (+ 6 you)
    (= opponent you) (+ 3 you)
    (> opponent you) (+ 0 you)
    (< opponent you) (+ 6 you)))

(def score-per-play
  (map
    #(calculate-score
       (map-opponent (first %))
       (map-you (second %)))
    plays))

(def total-score (reduce + score-per-play))

(println "Part 1" total-score)

(def lose "X")
(def draw "Y")
(def win "Z")

(defn calculate-score-2 [opponent expected-outcome]
  (cond
    (= expected-outcome lose) (+ 0 (+ (mod (- opponent 2) 3) 1))
    (= expected-outcome draw) (+ 3 opponent)
    (= expected-outcome win) (+ 6 (+ (mod opponent 3) 1))))

(def score-per-play-2
  (map #(calculate-score-2
          (map-opponent (first %))
          (second %))
       plays))

(println "Part 2" (reduce + score-per-play-2))