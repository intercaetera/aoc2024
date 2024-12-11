(ns aoc.day11
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(def example-input "125 17")

(defn parse [input]
  (->> (str/split input #"\s+")
       (map parse-long)))

(defn condition-zero [stone] (zero? stone))
(defn result-zero [_stone] (list 1))

(defn condition-even [stone]
    (even? (count (str stone))))
(defn result-even [stone]
  (map (comp parse-long str/join) (partition-all (/ (count (str stone)) 2) (str stone))))

(defn result-other [stone]
  (list (* stone 2024)))

(defn check-stone [stone]
  (cond
    (condition-zero stone) (result-zero stone)
    (condition-even stone) (result-even stone)
    :else (result-other stone)))

(defn blink [stones] (mapcat check-stone stones))

(defn solve [input]
  (count (nth (iterate blink (parse input)) 25)))

(solve example-input) ; 55312
(solve (utils/read-input 11)) ; 239714
