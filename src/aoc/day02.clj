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
                  (apply list)
                  (map parse-long)))))

(defn asc-or-desc? [row]
  (let [asc (sort row)
        desc (sort #(compare %2 %1) row)]
    (or (= row asc) (= row desc))))

(defn valid-neighbors? [row]
  (every? (fn [[a b]]
            (let [diff (abs (- a b))]
              (and (>= diff 1) (<= diff 3))))
          (partition 2 1 row)))

(defn solve [input]
  (->> input
      (parse)
      (filter asc-or-desc?)
      (filter valid-neighbors?)
      (count)))

(solve example-input)
(solve (utils/read-input 2))
