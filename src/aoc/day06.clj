(ns aoc.day06
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(def north [0 -1])
(def east [1 0])
(def south [0 1])
(def west [-1 0])

(def example-input
"....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(defn parse [input]
  (let [lines (str/split-lines input)
        height (count lines)
        width (count (first lines))
        obstacles (set (for [y (range height)
                        x (range width)
                        :let [char (get-in lines [y x])]
                        :when (= char \#)]
                    [x y]))
        start (first (for [y (range height)
                           x (range width)
                           :let [char (get-in lines [y x])]
                           :when (= char \^)]
                       [x y]))]
    {:height height
     :width width
     :obstacles obstacles
     :start start}))

(defn turn-clockwise [direction]
  (cond
    (= direction north) east
    (= direction east) south
    (= direction south) west
    (= direction west) north))

(defn out-of-bounds? [[x y] width height]
  (or (< x 0)
      (< y 0)
      (>= x width)
      (>= y height)))

(defn hits-obstacle? [position obstacles]
  (contains? obstacles position))

(defn infinite-loop? [position direction visited]
  (contains? visited [position direction]))

(defn step [position direction state]
  (cond 
    (infinite-loop? position direction (:visited state))
    :loop

    (out-of-bounds? position (:width state) (:height state))
    (set (map first (:visited state)))

    :else (let [[x y] position
          [dx dy] direction
          next-position [(+ x dx) (+ y dy)]]
      (if (hits-obstacle? next-position (:obstacles state))
        (recur position (turn-clockwise direction) state)
        (recur next-position direction 
               (update state :visited conj [position direction]))))))

(defn solve [input]
  (let [{:keys [height width obstacles start]} (parse input)
        state {:visited #{} :width width :height height :obstacles obstacles}]
    (count (step start north state))))

(defn solve2 [input]
  (let [{:keys [height width obstacles start]} (parse input)
        state {:visited #{} :width width :height height :obstacles obstacles}
        visited (step start north state)
        results (pmap #(step start north (update state :obstacles conj %))
                     visited)]
    (count (filter #(= :loop %) results))))

(solve example-input)
(solve (utils/read-input 6)) ; 4789

(solve2 example-input)
(solve2 (utils/read-input 6)) ; 1304
