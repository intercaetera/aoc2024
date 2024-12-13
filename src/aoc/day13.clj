(ns aoc.day13
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(def example-input
"Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279")

; 94a + 22b = 8400
; 34a + 67b = 5400

; ax*a + bx*b = px
; ay*a + by*b = px

(def pattern
  #"Button A: X\+(\d+), Y\+(\d+)\nButton B: X\+(\d+), Y\+(\d+)\nPrize: X=(\d+), Y=(\d+)")

(defn parse [input]
  (let [games (str/split (str/trim input) #"\s\s+")]
    (map (fn [game]
           (let [[_ & matches] (re-matches pattern game)
                 [ax ay bx by px py] (mapv parse-long matches)]
             {:ax ax :ay ay
              :bx bx :by by
              :px px :py py}))
         games)))

; Cramer's rule:
; https://www.matemaks.pl/metoda-wyznacznikow.html
; W = (det ax bx ay by)
; Wa = (det px bx py by)
; Wb = (det ax px ay py)

; if W = 0 => no solutions, else:
; a = (Wa / W), y = (Wb / W)

(defn det [a b c d] (- (* a d) (* b c)))

(defn solve-game [{:keys [ax ay bx by px py]}]
  (let [w (det ax bx ay by)]
    (if (zero? w) 0
      (let [wa (det px bx py by)
            wb (det ax px ay py)
            a (/ wa w)
            b (/ wb w)
            valid (and (integer? a) (pos? a) (integer? b) (pos? b))]
        (if valid (+ (* 3 a) b) 0)))))

(defn solve [input]
  (apply + (map solve-game (parse input))))

(def error 10000000000000)
(defn update-game [game]
  (-> game
      (update :px + error)
      (update :py + error)))

(defn solve2 [input]
  (let [games (parse input)
        _ (print games)
        updated-games (map update-game games)]
    (apply + (map solve-game updated-games))))

(solve example-input)
(solve (utils/read-input 13))

(solve2 example-input) ; 875318608908
(solve2 (utils/read-input 13)) ; 108713182988244
