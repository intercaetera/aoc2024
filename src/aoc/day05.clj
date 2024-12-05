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

(defn valid-pages? [pages all-rules]
  (let [rules-for-pages (find-rules-for-pages all-rules pages)]
    (every? #(satisfies-rule? % pages) rules-for-pages))
  )

(defn solve [input]
  (let [[all-rules all-pages] (parse input)
        valid-pages (filter (fn [pages]
                              (valid-pages? pages all-rules))
                            all-pages)
        middle-elements (map middle-element valid-pages)]
    (apply + middle-elements)))

(defn swap [v i j]
  (assoc v j (v i) i (v j)))

(defn find-first-invalid-rule [pages rules]
  (first (remove #(satisfies-rule? % pages) rules)))

(defn reorder-pages [pages rules]
  (if (valid-pages? pages rules)
    pages
    (let [[x y] (find-first-invalid-rule pages rules)
          x-idx (.indexOf pages x)
          y-idx (.indexOf pages y)
          swapped-pages (swap (vec pages) x-idx y-idx)]
      (recur swapped-pages rules))))

(defn solve2 [input]
  (let [[all-rules all-pages] (parse input)
        invalid-pages (remove (fn [pages]
                                (valid-pages? pages all-rules))
                              all-pages)
        reordered-pages (map
                          #(reorder-pages %
                             (find-rules-for-pages
                               all-rules %))
                          invalid-pages)
        middle-elements (map middle-element reordered-pages)]
    (apply + middle-elements)))

(solve example-input)
(solve (utils/read-input 5))

(solve2 example-input)
(solve2 (utils/read-input 5))
