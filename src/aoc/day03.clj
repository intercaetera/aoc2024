(ns aoc.day03
  (:require [aoc.utils :as utils]))

(def example-input "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
(def example-input2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defn solve [input]
  (->> (re-seq #"mul\((\d+),(\d+)\)" input)
       (map (fn [[_ x y]]
              (* (parse-long x) (parse-long y))))
       (apply +)))

(defn tokenize [[_full mul x y do dont]]
  (cond
    mul {:type :mul :x (parse-long x) :y (parse-long y)}
    do {:type :do}
    dont {:type :dont}))

(defn process [tokens]
  (:sum (reduce
          (fn [{:keys [enabled sum]} token]
            (case (:type token)
              :do {:enabled true :sum sum}
              :dont {:enabled false :sum sum}
              :mul (if enabled
                     {:enabled enabled :sum (+ sum (* (:x token) (:y token)))}
                     {:enabled enabled :sum sum})))
          {:enabled true :sum 0}
          tokens)))

(defn solve2 [input]
  (->> (re-seq #"(mul\((\d+),(\d+)\))|(do\(\))|(don't\(\))" input)
       (map tokenize)
       (process)))

(solve example-input) ; 161
(solve (utils/read-input 3)) ; 175700056

(solve2 example-input2) ; 48
(solve2 (utils/read-input 3)) ; 71668682
