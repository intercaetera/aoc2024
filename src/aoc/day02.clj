(ns aoc.day02
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(def example-input
"7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(defn parse [input]
  (->> input
       str/split-lines
       (map #(->> (str/split % #"\s+")
                  (map parse-long)))))

(defn asc-or-desc? [row]
  (or (= row (sort row))
      (= row (sort #(compare %2 %1)))))

(defn valid-neighbors? [row]
  (every? (fn [[a b]]
            (let [diff (abs (- a b))]
              (and (>= diff 1) (<= diff 3))))
          (partition 2 1 row)))

(defn valid-row? [row]
  (and (asc-or-desc? row) (valid-neighbors? row)))

(defn solve [input]
  (->> input
      (parse)
      (filter valid-row?)
      (count)))

(defn valid-row-with-dampening? [row]
  (let [subrows (for [i (range (count row))]
                  (concat (take i row)
                          (drop (inc i) row)))]
    (some valid-row? (cons row subrows))))

(defn solve2 [input]
  (->> input
       (parse)
       (filter valid-row-with-dampening?)
       (count)))

(solve example-input)
(solve (utils/read-input 2))

(solve2 example-input)
(solve2 (utils/read-input 2))
