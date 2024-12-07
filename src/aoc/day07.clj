(ns aoc.day07
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(def example-input
"190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(defn parse [input]
  (let [lines (str/split-lines input)]
    (map (fn [line]
           (let [[test rest] (str/split line #": ")
                 numbers (map parse-long (str/split rest #"\s+"))]
             [(parse-long test) numbers]))
         lines)))

(defn generate-possible-symbols
  ([length] (generate-possible-symbols length [[]]))
  ([length all-symbols] (if (= (count (first all-symbols)) length)
                      all-symbols
                      (recur
                        length
                        (mapcat (fn [symbols] [(conj symbols +) (conj symbols *)]) all-symbols)))))

(defn apply-symbols [numbers symbols]
  (if (empty? symbols) (first numbers)
    (let [[a b & rest-numbers] numbers
          [symbol & rest-symbols] symbols]
      (recur (conj rest-numbers (symbol a b)) rest-symbols))))

(defn valid-result [test numbers]
  (let [all-symbols (generate-possible-symbols (dec (count numbers)))
        all-results (map #(apply-symbols numbers %) all-symbols)]
    (if (some #(= test %) all-results) test 0)))

(defn solve [input]
  (let [lines (parse input)
        results (map #(apply valid-result %) lines)]
    (apply + results)))

(solve example-input)
(solve (utils/read-input 7)) ; 1399219271639