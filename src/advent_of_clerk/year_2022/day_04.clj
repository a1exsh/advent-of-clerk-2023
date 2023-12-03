;; # 🎄 Advent of Clerk: 2022: Day 4: Camp Cleanup
(ns advent-of-clerk.year-2022.day-04
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as string]))

;; ## Parsing the input
;; Example:
#_(def input "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(def input (slurp "input/2022/04"))

(def lines (string/split-lines input))

(def pairs (map (fn [s]
                  (map (fn [p]
                         (map parse-long (string/split p #"-")))
                       (string/split s #",")))
                lines))

;; ## Part I
;; Count the pairs where one is fully contained in the other.
(defn contained?
  "Checks if l1-r1 is fully contained by l2-r2."
  [[l1 r1] [l2 r2]]
  (and (>= l1 l2)
       (<= r1 r2)))

(->> pairs
     (filter (fn [[a b]]
               (or (contained? a b)
                   (contained? b a))))
     count)

;; ## Part II
;; Count the pairs that overlap at all.
(defn overlap? [[l1 r1] [l2 r2]]
  (or (<= l2 l1 r2)
      (<= l2 r1 r2)
      (<= l1 l2 r1)
      (<= l1 r2 r1)))

(->> pairs
     (filter #(apply overlap? %))
     count)
