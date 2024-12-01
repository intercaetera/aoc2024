(ns aoc.utils
  (:require [clojure.string :as str]))

(defn read-input [day]
  (slurp (format "resources/day%02d.txt" day)))

(defn read-lines [day]
  (-> (read-input day)
      str/split-lines))
