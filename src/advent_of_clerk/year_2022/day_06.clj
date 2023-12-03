;; # 🎄 Advent of Clerk: 2022: Day 6: Tuning Trouble
(ns advent-of-clerk.year-2022.day-06
  (:require [nextjournal.clerk :as clerk]))

;; ## The input
(def example "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

(def input (slurp "input/2022/06"))

;; ## Part I
;; Life is easy when you know exactly the right function ;)
(partition 4 1 example)

(defn find-signal [s n]
  (->> s
       (partition n 1)
       (take-while #(->> % distinct count (not= n)))
       count
       (+ n)))

(find-signal input 4)

;; ## Part II
(find-signal input 14)
