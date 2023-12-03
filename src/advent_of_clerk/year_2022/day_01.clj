;; # 🎄 Advent of Clerk: 2022: Day 1: Calorie Counting
(ns advent-of-clerk.year-2022.day-01
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as string]))

;; ## Parsing the input
(def input (slurp "input/2022/01"))

(def lines (string/split-lines input))

(def blocks (partition-by string/blank? lines))
(count blocks)

(def str-elves (remove #(= % [""]) blocks))
(count str-elves)

(def elves (map #(map parse-long %) str-elves))

;; ## Part I
(def calories (map #(reduce + %) elves))

(def fattest-elf (apply max calories))

;; ## Part II
(def three-fattest-elves (->> calories sort reverse (take 3)))

(reduce + three-fattest-elves)
