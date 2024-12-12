(ns aoc.day12
  (:require [aoc.utils :as utils]
            [clojure.string :as str]
            [clojure.set :as set]))

(def example-input
"RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE")

(defn parse [input]
  (let [lines (str/split-lines input)
        height (count lines)
        width (count (first lines))]
    (->> (for [y (range height)
          x (range width)
          :let [element (get-in lines [y x])]]
      [element [x y]])
         (reduce (fn [acc [element coords]]
                   (update acc element (fnil conj #{}) coords))
                 {}))))

(defn neighbors [[x y]]
  #{[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]})

(defn valid-neighbors [region coords]
  #(and (contains? coords %)
        (not (contains? region %))))

(defn find-region [coords start]
  (loop [region #{start}
         to-visit (into #{} (filter (valid-neighbors region coords) (neighbors start)))]
    (if (empty? to-visit) region
      (let [new-region (set/union region to-visit)
            new-to-visit (reduce
                           (fn [acc coord]
                             (into acc (filter (valid-neighbors new-region coords) (neighbors coord))))
                           #{}
                           to-visit)]
        (recur new-region new-to-visit)))))

(defn partition-regions [all-coords]
  (loop [coords all-coords
         start (first all-coords)
         regions []]
    (if (zero? (count coords)) regions
      (let [found-region (find-region coords start)
            remaining-coords (set/difference coords found-region)
            new-start (first remaining-coords)
            new-regions (conj regions found-region)]
        (recur remaining-coords new-start new-regions)))))

(defn perimeter [region]
  (apply + (map (fn [field]
          (- 4 (count (filter some? (map #(get region %) (neighbors field))))))
        region)))

(defn area [region] (count region))

(defn solve [input]
  (let [all-coords (parse input)
        regions (mapcat (fn [[_key coords]] (partition-regions coords)) all-coords)
        prices (map #(* (perimeter %) (area %)) regions)]
    (apply + prices)))

(defn corners [[x y]]
  [[[(dec x) y] [(dec x) (dec y)] [x (dec y)]]   ; top left
   [[x (dec y)] [(inc x) (dec y)] [(inc x) y]]   ; top right
   [[(inc x) y] [(inc x) (inc y)] [x (inc y)]]   ; bottom right
   [[x (inc y)] [(dec x) (inc y)] [(dec x) y]]]) ; bottom left

(defn corner-values [region corner] (mapv #(get region %) corner))

(defn check-corner [[left diag top]] ; on the example of top left, but works for all
  (cond
    (and (nil? left) (nil? top)) 1
    (and (some? left) (nil? diag) (some? top)) 1
    :else 0))

(defn sides-for-field [region] 
  (fn [field]
    (let [values (mapv (partial corner-values region) (corners field))]
      (apply + (map check-corner values)))))

(defn sides [region]
  (apply + (map (sides-for-field region) region)))

(defn solve2 [input]
  (let [all-coords (parse input)
        regions (mapcat (fn [[_key coords]] (partition-regions coords)) all-coords)
        prices (map #(* (sides %) (area %)) regions)]
    (apply + prices)))

(solve example-input) ; 1930
(solve (utils/read-input 12)) ; 1446042

(solve2 example-input) ; 1206
(solve2 (utils/read-input 12)) ; 902742
