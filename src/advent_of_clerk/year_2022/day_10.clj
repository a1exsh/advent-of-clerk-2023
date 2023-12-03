;; # 🎄 Advent of Clerk: 2022: Day 10: Cathode-Ray Tube
(ns advent-of-clerk.year-2022.day-10
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as string]))

;; ## Parsing the input
(def example
  (string/trim "
addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop"))

(def input (slurp "input/2022/10"))

(defn parse [s]
  (->> s
       string/split-lines
       (map (fn [l]
              (if (= "noop" l)
                :noop
                (let [[_ v] (re-matches #"^addx (.*)$" l)]
                  (parse-long v)))))))

(def program (parse input #_ example))

;; ## Part I
(def cycles
  (->> program
       (mapcat (fn [p]
                 (if (= :noop p)
                   [0]
                   [0 p])))
       (reductions + 1)
       (into [])))

(->> (range 20 221 40)
     (map #(* % (nth cycles (dec %))))
     (reduce +))

;; ## Part II
(->> cycles
     (partition 40)
     (map #(->> %
                (map-indexed (fn [i x]
                               (if (<= (dec x) i (inc x))
                                 "#"
                                 ".")))
                string/join)))
