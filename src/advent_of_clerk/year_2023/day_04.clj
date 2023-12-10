;; # 🎄 Advent of Clerk: 2023: Day 4: Scratchcards
(ns advent-of-clerk.year-2023.day-04
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as string]))

(def example "
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

;; ## Parsing the input
(def input (slurp "input/2023/04"))
;;(def input (string/trim example))

(def lines (string/split-lines input))

(defn parse-numbers [nums-str]
  (->> (-> nums-str
           string/trim
           (string/split #"\s+"))
       (map parse-long)
       (into [])))

(parse-numbers " 1  2 12 ")

(defn parse-card [line]
  (let [[_ cn-str wins-str draws-str]
        (re-find #"Card\s+(\d+): (.*) \| (.*)" line)]
    {:card-no (parse-long cn-str)
     :wins  (parse-numbers wins-str)
     :draws (parse-numbers draws-str)}))

(parse-card (first lines))

(def cards
  (->> lines
       (map parse-card)
       (into [])))

;; ## Part I
(defn card-points [{:keys [wins draws]}]
  (let [winning (clojure.set/intersection (set wins) (set draws))
        cnt (count winning)]
    (if (= 0 cnt)
      0
      (->> cnt dec (bit-shift-left 1)))))

(card-points (first cards))

(->> cards
     (map card-points)
     (reduce +))
