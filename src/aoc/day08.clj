(ns aoc.day08
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(def example-input 
"............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")

(defn mmapv [f coll]
  (into (sorted-map) (mapv (fn [[k v]] {k, (f v)}) coll)))

(defn remove-key [[char xs]]
  {char (mapv #(apply vector (rest %)) xs)})

(defn group-by-and-pop [fn coll]
  (->> (group-by fn coll)
       (mapv remove-key)
       (into (sorted-map))))

(defn parse [input]
  (let [lines (str/split-lines input)
        height (count lines)
        width (count (first lines))
        all-antennas (for [y (range height)
                       x (range width)
                       :let [char (get-in lines [y x])]
                       :when (not= char \.)]
                   [char x y])
        antennas (group-by-and-pop first all-antennas)]
    {:height height
     :width width
     :antennas antennas}))

(defn get-pairs [coords]
  (for [a coords
        b coords
        :when (not= a b)]
    (vector a b)))

(defn get-vector-between-points [[x1 y1] [x2 y2]]
  [(- x2 x1) (- y2 y1)])

(defn get-antinodes-for-pair [[a b]]
  (let [vect (get-vector-between-points a b)
        ap1 (mapv + b vect)
        ap2 (mapv - a vect)]
    [ap1 ap2]))

(defn get-antinodes [coords]
  (let [pairs (get-pairs coords)
        antinodes (mapcat get-antinodes-for-pair pairs)]
    antinodes))

(defn get-in-bounds-predicate [width height]
  (fn [[x y]] (and
                (>= x 0)
                (>= y 0)
                (< x width)
                (< y height))))

(defn solve [input]
  (let [{:keys [height width antennas]} (parse input)
        antinodes-for-frequency (mmapv get-antinodes antennas)
        all-antinodes (set (apply concat (vals antinodes-for-frequency)))
        antinodes-in-bounds (filter (get-in-bounds-predicate width height) all-antinodes)]
    (count antinodes-in-bounds)))

(defn get-resonant-antinodes-for-pair [width height]
  (fn [[a b]]
    (let [vect (get-vector-between-points a b)
          get-next-point (fn [point] (mapv + point vect))
          get-prev-point (fn [point] (mapv - point vect))
          in-bounds? (get-in-bounds-predicate width height)
          next-points (take-while in-bounds? (iterate get-next-point a))
          prev-points (take-while in-bounds? (iterate get-prev-point b))]
      (distinct (concat next-points prev-points)))))

(defn get-resonant-antinodes [width height]
  (fn [coords]
    (let [pairs (get-pairs coords)
          antinodes (mapcat (get-resonant-antinodes-for-pair width height) pairs)]
      antinodes)))

(defn solve2 [input]
  (let [{:keys [height width antennas]} (parse input)
        antinodes-for-frequency (mmapv (get-resonant-antinodes width height) antennas)
        all-antinodes (set (apply concat (vals antinodes-for-frequency)))
        antinodes-in-bounds (filter (get-in-bounds-predicate width height) all-antinodes)]
    (count antinodes-in-bounds)))

(solve example-input)
(solve (utils/read-input 8)) ; 357

(solve2 example-input)
(solve2 (utils/read-input 8)) ; 1266
