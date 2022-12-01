;; # 🎄 Advent of Clerk: Day 1
(ns advent-of-clerk.day-01
  (:require [nextjournal.clerk :as clerk]))

;; Input

(def input (slurp "input/2022/01"))

(def lines (clojure.string/split input #"\n"))

(def blocks (partition-by clojure.string/blank? lines))
(count blocks)

(def str-elves (remove #(= % [""]) blocks))
(count str-elves)

(defn parse-int [s]
  (Integer/valueOf s))

(def elves (map #(map parse-int %) str-elves))

(def calories (map #(reduce + %) elves))

(def fattest-elf (apply max calories))

(def sum-three-top (->> calories sort reverse (take 3) (reduce +)))
