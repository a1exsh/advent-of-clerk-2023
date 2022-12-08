;; # 🎄 Advent of Clerk: Day 8: Treetop Tree House
(ns advent-of-clerk.day-08
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as string]))

;; ## Parsing the input
(def example
  (string/trim "
30373
25512
65332
33549
35390"))

(defn parse [s]
  (->> s
       string/split-lines
       (mapv (fn [line]
               (mapv #(-> % str parse-long)
                     line)))))

(def input (slurp "input/2022/08"))

(def puzzle (parse input #_ example))

;; ## Part I
;;
;; We will have to scan the map four times (from each direction) and then take
;; a union of all trees marked as visible, so that we count all of them, but
;; only _once_.
;;
(defn scan-row
  "Scan a row of trees and return the visible ones as 1, and obscured — as 0."
  [ts]
  (->> ts
       (reductions max -1)
       butlast
       (map (fn [l r]
              (if (> l r) 1 0))
            ts)))

(def from-left 
  (->> puzzle
       (map scan-row)))

(def from-right
  (->> puzzle
       (map (comp reverse scan-row reverse))))

;; To look from the top we will have to transpose the matrix:
(defn transpose [vs]
  (let [h (count vs)
        w (count (first vs))]
    (for [i (range w)]
      (for [j (range h)]
        (nth (nth vs j) i)))))

(transpose puzzle)

(def from-top
  (->> puzzle
       transpose
       (map scan-row)
       transpose))

(def from-bottom
  (->> puzzle
       transpose
       (map (comp reverse scan-row reverse))
       transpose))

;; Finally, we can take the union of the four views:
(def from-any
  (map (fn [ls rs ts bs]
         (map (fn [l r t b]
                (max l r t b))
              ls rs ts bs))
       from-left
       from-right
       from-top
       from-bottom))

(->> from-any
     (map #(reduce + %))
     (reduce +))
