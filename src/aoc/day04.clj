(ns aoc.day04
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(def example-input
"MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(def directions [[-1 -1] [0 -1] [1 -1]
                 [-1  0]        [1  0]
                 [-1  1] [0  1] [1  1]])

(defn parse [input]
  (let [lines (str/split-lines input)
        height (count lines)
        width (count (first lines))]
    (into {} (for [y (range height)
                  x (range width)
                  :let [char (get-in lines [y x])]]
               [[x y] char]))))

(defn get-letter [grid searched-letter]
  (filter (fn
            [[[x y] letter]]
            (= letter searched-letter))
          grid))

(defn check-direction [grid [x y] [dx dy]]
  (let [mas '(\M \A \S)
        lookup (map-indexed (fn [idx _letter]
                              (get grid [(+ x (* (inc idx) dx)) (+ y (* (inc idx) dy))]))
                            mas)]
    (= lookup mas)))

(defn count-directions [grid [x y]]
  (count (filter (fn [direction]
                   (check-direction grid [x y] direction))
                 directions)))

(defn solve [input]
  (let [grid (parse input)
        xs (get-letter grid \X)]
    (apply + (map (fn [[[x y] _]]
           (count-directions grid [x y]))
         xs))))

(defn check-diagonal-mas [grid [x y]]
  (let [nw (get grid [(- x 1) (- y 1)])
        se (get grid [(+ x 1) (+ y 1)])
        ne (get grid [(+ x 1) (- y 1)])
        sw (get grid [(- x 1) (+ y 1)])

        nw-se-match (or (and (= nw \M) (= se \S)) (and (= nw \S) (= se \M)))
        ne-sw-match (or (and (= ne \M) (= sw \S)) (and (= ne \S) (= sw \M)))]
    (and nw-se-match ne-sw-match)))

(defn solve2 [input]
  (let [grid (parse input)
        as (get-letter grid \A)]
    (count (filter (fn [[[x y] _]]
           (check-diagonal-mas grid [x y]))
         as))))

(solve example-input)
(solve (utils/read-input 4))

(solve2 example-input)
(solve2 (utils/read-input 4))
