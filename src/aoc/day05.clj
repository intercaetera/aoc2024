(ns aoc.day05
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(def example-input
"47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(defn parse [input]
  (let [[raw-rules raw-pages] (str/split input #"\n\n")
        all-rules (->> raw-rules
                       (str/split-lines)
                       (map #(str/split % #"\|"))
                       (map #(map parse-long %)))
        all-pages (->> raw-pages
                       (str/split-lines)
                       (map #(str/split % #","))
                       (map #(map parse-long %)))]
    [all-rules all-pages]))

(defn find-rules-for-pages [all-rules pages]
  (filter (fn [[x y]]
            (and (some #{x} pages)
                 (some #{y} pages)))
          all-rules))

(defn satisfies-rule? [[x y] pages]
  (< (.indexOf pages x) (.indexOf pages y)))

(defn middle-element [coll]
  (nth coll (quot (count coll) 2)))

(defn solve [input]
  (let [[all-rules all-pages] (parse input)
        valid-pages (filter (fn [pages]
                              (let [rules-for-pages (find-rules-for-pages all-rules pages)]
                                (every? #(satisfies-rule? % pages) rules-for-pages)))
                            all-pages)
        middle-elements (map middle-element valid-pages)]
    (apply + middle-elements)))

(solve example-input)
(solve (utils/read-input 5))
