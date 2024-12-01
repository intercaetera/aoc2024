(ns aoc.day01
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(def example-input
"3   4
4   3
2   5
1   3
3   9
3   3")

(defn parse [input]
  (let [lines (str/split-lines input)
        pairs (map #(-> %
                        (str/split #"\s+")
                        (->> (map parse-long)))
                   lines)]
    (apply map list pairs)))

(defn solve [input]
  (let [[first-col second-col] (parse input)
        sorted-pairs (map list (sort first-col) (sort second-col))
        sum-of-pairs (map #(abs(apply - %)) sorted-pairs)]
    (apply + sum-of-pairs)))

(defn solve2 [input]
  (let [[first-col second-col] (parse input)
        second-freqs (frequencies second-col)]
    (reduce (fn [acc val]
              (+ acc 
                 (* val (second-freqs val 0))))
            0
            first-col)))

(solve example-input)
(solve (utils/read-input 1)) ; 2375403

(solve2 example-input)
(solve2 (utils/read-input 1)) ; 23082277
