;; # 🎄 Advent of Clerk: Day 2
(ns advent-of-clerk.day-02
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as string]))

;; ## Parsing the input
(def input (slurp "input/2022/02"))

(def lines (string/split-lines input))

(def rounds (map #(string/split % #" ") lines))

;; # Part I
(clerk/table [["A" "X" "Rock"     1]
              ["B" "Y" "Paper"    2]
              ["C" "Z" "Scissors" 3]])
;; Lost: 0
;; Draw: 3
;; Won: 6
(def score
  {["A" "X"] 4
   ["A" "Y"] 8
   ["A" "Z"] 3
   ["B" "X"] 1
   ["B" "Y"] 5
   ["B" "Z"] 9
   ["C" "X"] 7
   ["C" "Y"] 2
   ["C" "Z"] 6})

(->> rounds #_[["A" "Y"] ["B" "X"] ["C" "Z"]]
     (map score)
     (reduce +))

;; # Part II
;; X=Lose
;; Y=Draw
;; Z=Win
(def score2
  {["A" "X"] 3
   ["A" "Y"] 4
   ["A" "Z"] 8
   ["B" "X"] 1
   ["B" "Y"] 5
   ["B" "Z"] 9
   ["C" "X"] 2
   ["C" "Y"] 6
   ["C" "Z"] 7})

(->> rounds #_[["A" "Y"] ["B" "X"] ["C" "Z"]]
     (map score2)
     (reduce +))
