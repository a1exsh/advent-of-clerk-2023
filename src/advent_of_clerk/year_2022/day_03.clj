;; # 🎄 Advent of Clerk: 2022: Day 3: Rucksack Reorganization
(ns advent-of-clerk.year-2022.day-03
  (:require [nextjournal.clerk :as clerk]
            [clojure.set :as set]
            [clojure.string :as string]))

;; ## Parsing the input
(def input (slurp "input/2022/03"))

#_(def input "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(def lines (string/split-lines input))

;; ## Part I
(defn prio [c]
  (let [i (int c)]
    (if (<= (int \a) i (int \z))
      (inc  (- i (int \a)))
      (+ 27 (- i (int \A))))))

(->> [\a \z \A \Z]
     (map (fn [c] {:char c :int (int c) :priority (prio c)}))
     clerk/table)

(->> lines
     (map (fn [s]
            (let [len   (count s)
                  left  (subs s 0 (/ len 2))
                  right (subs s (/ len 2))]
              (-> (set/intersection (set left) (set right))
                  first
                  prio))))
     (reduce +))

;; ## Part II
(->> lines
     (partition 3)
     (map #(->> %
                (map set)
                (apply set/intersection)
                first
                prio))
     (reduce +))
