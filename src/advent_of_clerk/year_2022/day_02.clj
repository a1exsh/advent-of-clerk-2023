;; # 🎄 Advent of Clerk: 2022: Day 2: Rock Paper Scissors
(ns advent-of-clerk.year-2022.day-02
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
;; Won:  6
(def score
  {["A" "X"] (+ 1 3)
   ["A" "Y"] (+ 2 6)
   ["A" "Z"] (+ 3 0)
   ["B" "X"] (+ 1 0)
   ["B" "Y"] (+ 2 3)
   ["B" "Z"] (+ 3 6)
   ["C" "X"] (+ 1 6)
   ["C" "Y"] (+ 2 0)
   ["C" "Z"] (+ 3 3)})

(->> rounds #_[["A" "Y"] ["B" "X"] ["C" "Z"]]
     (map score)
     (reduce +))

;; # Part II
;; X=Lose
;; Y=Draw
;; Z=Win
(def score2
  {["A" "X"] (+ 0 3)
   ["A" "Y"] (+ 3 1)
   ["A" "Z"] (+ 6 2)
   ["B" "X"] (+ 0 1)
   ["B" "Y"] (+ 3 2)
   ["B" "Z"] (+ 6 3)
   ["C" "X"] (+ 0 2)
   ["C" "Y"] (+ 3 3)
   ["C" "Z"] (+ 6 1)})

(->> rounds #_[["A" "Y"] ["B" "X"] ["C" "Z"]]
     (map score2)
     (reduce +))
