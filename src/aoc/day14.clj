(ns aoc.day14
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(def example-input 
["p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3" 7 11])

(def real-input [(utils/read-input 14) 103 101])

(defn parse [[input height width]]
  (let [lines (str/split-lines input)
        pattern #"-?\d+"
        parse-line (fn [line] (mapv parse-long (re-seq pattern line)))
        matches->robot (fn [[px py vx vy]] {:position [px py] :velocity [vx vy]})]
    {:robots (map (comp matches->robot parse-line) lines)
     :height height
     :width width}))

(defn wrap [len c]
  (cond
    (< c 0) (+ c len)
    (>= c len) (- c len)
    :else c))

(defn move-once-fn [width height [vx vy]]
  (fn [[px py]]
    [(wrap width (+ px vx))
     (wrap height (+ py vy))]))

(defn move-n-times [n move coords]
  (nth (iterate move coords) n))

(defn quadrant [half-x half-y [x y]]
  (cond
    (and (< half-x x) (< half-y y)) 1
    (and (> half-x x) (< half-y y)) 2
    (and (< half-x x) (> half-y y)) 3
    (and (> half-x x) (> half-y y)) 4
    :else nil
  ))

(defn get-final-positions [moves robots width height]
  (map (fn [{:keys [position velocity]}]
         (move-n-times moves (move-once-fn width height velocity) position))
       robots))

(defn solve [input]
  (let [{:keys [robots width height]} (parse input)
        final-positions (get-final-positions 100 robots width height)
        half-x (quot width 2)
        half-y (quot height 2)
        quadrants-or-nil (map #(quadrant half-x half-y %) final-positions)
        quadrants (filter some? quadrants-or-nil)
        freqs (frequencies quadrants)]
    (apply * (vals freqs))))

(defn render [width height positions]
  (let [board (vec (repeat height (vec (repeat width "."))))
        mark-position (fn [board [x y]]
                       (assoc-in board [y x] "#"))]
    (->> positions
         (reduce mark-position board)
         (map #(str/join "" %)))))

(defn next-robots-fn [width height]
  (fn [robots] (map (fn [robot]
                      (update robot :position (move-once-fn width height (:velocity robot))))
                    robots)))

(defn solve2 [input]
  (let [{:keys [robots width height]} (parse input)
        next-robots (next-robots-fn width height)]
    (loop [current-robots robots
           idx 0]
      (let [positions (map #(:position %) current-robots)
            output (render width height positions)
            potentially-valid (some #(str/includes? % "##################") output)]
        (if (or potentially-valid (= idx 10000))
          [idx output]
          (recur (next-robots current-robots) (inc idx)))))))

(solve example-input) ; 12
(solve real-input) ; 232589280

(solve2 real-input) ; 7569
