(ns aoc.day09
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(def trivial-input "12345")
(def example-input "2333133121414131402")

(defn parse [input]
  (let [xs (map (comp parse-long str) (str (str/trim input) "0"))
        pairs (partition 2 xs)
        indexed-pairs (map-indexed #(apply vector %1 %2) pairs)]
    indexed-pairs))

(defn triple->layout [[id occupied free]]
  (apply vector (concat (repeat occupied id) (repeat free nil))))

(defn generate-layout [triples]
  (apply vector (mapcat triple->layout triples)))

(defn print-layout [layout]
  (str/join (map #(if (nil? %) "." (str %)) layout)))

(defn find-first-index [coll predicate]
  (first (keep-indexed
           (fn [idx value]
             (when (predicate value)
               idx))
           coll)))

(defn find-first-space [layout]
  (find-first-index layout #(nil? %)))

(defn find-last-index [coll predicate]
  (first (keep-indexed
           (fn [idx value]
             (when (predicate value)
               (- (count coll) idx 1)))
           (reverse coll))))

(defn find-last-file [layout]
  (find-last-index layout #(not (nil? %))))

(defn move-file [layout from to]
  (let [file-id (nth layout from)]
    (-> layout
        (assoc from nil)
        (assoc to file-id))))

(defn compact [layout]
  (let [space-idx (find-first-space layout)
        file-idx (find-last-file layout)]
    (if (> space-idx file-idx)
      (filter identity layout)
      (recur (move-file layout file-idx space-idx)))))

(defn calculate-checksum [layout]
  (apply + (map-indexed (fn [idx value] (* idx value)) layout)))

(defn solve [input]
  (-> input
      (parse)
      (generate-layout)
      (compact)
      (calculate-checksum)))

(defn get-layout-runs [layout]
  (partition-by identity layout))

(defn find-last-new-file-block [layout-runs skipped]
  (find-last-index
    layout-runs
    (fn [block]
      (and (not (nil? (first block)))
           (not (contains? skipped (first block)))))))

(defn find-first-space-block-of-size [layout-runs size]
  (find-first-index
    layout-runs
    (fn [block]
      (and (nil? (first block))
           (>= (count block) size)))))

(defn overlay [v1 v2]
  (vec (concat v1 (drop (count v1) v2))))

(defn move-block [runs from to]
  (let [file-block (nth runs from)
        space-block (nth runs to)
        file-length (count file-block)
        space-length (count space-block)
        remaining-space (- space-length file-length)]
    (concat
      (take to runs)
      (list file-block)
      (when (pos? remaining-space)
        (list (repeat remaining-space nil)))
      (drop (inc to) (take from runs))
      (list (repeat file-length nil))
      (drop (inc from) runs))))

(defn compact-blocks 
  ([layout-runs] (compact-blocks layout-runs #{}))
  ([layout-runs processed-ids]
   (print (count processed-ids) "\n")
   (let [file-idx (find-last-new-file-block layout-runs processed-ids)
         file-block (nth layout-runs file-idx)
         file-id (first file-block)
         space-idx (find-first-space-block-of-size layout-runs (count file-block))]
     (if (or (nil? file-idx) (= file-id 0))
       (map #(if (nil? %) 0 %) (apply concat layout-runs))
       (recur 
         (if-not (or (nil? space-idx) (> space-idx file-idx)) 
           (move-block layout-runs file-idx space-idx)
           layout-runs)
         (conj processed-ids file-id))))))

(defn solve2 [input]
  (-> input
      (parse)
      (generate-layout)
      (get-layout-runs)
      (compact-blocks)
      (calculate-checksum)))

(solve example-input)
(solve (utils/read-input 9)) ; 6346871685398

(solve2 example-input)
(solve2 (utils/read-input 9)) ; 6373055193464
