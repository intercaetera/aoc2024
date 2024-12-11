(ns aoc.day11
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(def example-input "125 17")

(defn parse [input]
  (->> (str/split input #"\s+")
       (map parse-long)))

(defn len [num] (count (str num)))

(defn split-in-half [stone]
  (map (comp parse-long str/join) (partition-all (/ (count (str stone)) 2) (str stone))))

(defn safe-add [val count] (if (nil? val) count (+ val count)))

(defn check-stone [stones [stone count]]
  (cond
    (zero? stone) (update stones 1 safe-add count)
    (even? (len stone)) (let [[a b] (split-in-half stone)]
                          (-> stones
                              (update a safe-add count)
                              (update b safe-add count)))
    :else (update stones (* 2024 stone) safe-add count)))

(defn blink [stones] (reduce check-stone {} stones))

(defn solve-n [n input]
  (let [starting-stones (parse input)
        starting-map (reduce (fn [acc stone] (assoc acc stone 1)) {} starting-stones)]
    (apply + (vals (nth (iterate blink starting-map) n)))))

(def solve (partial solve-n 25))
(def solve2 (partial solve-n 75))

(solve example-input) ; 55312
(solve (utils/read-input 11)) ; 239714
(solve2 (utils/read-input 11)) ; 284973560658514
