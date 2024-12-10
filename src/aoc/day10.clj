(ns aoc.day10
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(def trivial-input
"0123
1234
8765
9876")

(def example-input
"89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")

(def neighbor-vectors [[0 -1] [1 0] [0 1] [-1 0]])

(defn parse [input]
  (->> input
      (str/split-lines)
      (mapv #(mapv (comp parse-long str) %))))

(defn get-neighbors [grid [x y]]
  (->> neighbor-vectors 
       (mapv (fn [[dx dy]]
               {:value (get-in grid [(+ y dy) (+ x dx)])
                :loc [(+ x dx) (+ y dy)]}))
       (filter #(= (:value %) (inc (get-in grid [y x]))))))

(defn get-trailheads [grid]
  (for [y (range (count grid))
        x (range (count (first grid)))
        :let [value (get-in grid [y x])]
        :when (zero? value)]
    [x y]))

(defn dfs
  ([grid pos] (dfs grid pos #{} #{}))
  ([grid pos visited peaks]
   (let [height (get-in grid (reverse pos))]
     (cond
       (contains? visited pos) peaks
       (= height 9) (conj peaks pos)
       :else (reduce
               (fn [peaks neighbor]
                 (dfs grid (:loc neighbor) (conj visited pos) peaks))
               peaks
               (get-neighbors grid pos))))))

(defn solve [input]
  (let [grid (parse input)
        trailheads (get-trailheads grid)]
    (count (mapcat (partial dfs grid) trailheads))))

(solve example-input) ; 36
(solve (utils/read-input 10)) ; 582
