;; # 🎄 Advent of Clerk: 2023: Day 2: Cube Conundrum
(ns advent-of-clerk.day-02
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as string]))

(def example "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

;; ## Parsing the input
(def input (slurp "input/02"))
;;(def input example)

(def lines (string/split-lines input))

(defn parse-single-color [n+color]
  (let [[_ n color] (re-find #"(\d+) (\w+)" n+color)]
    [(parse-long n) (keyword color)]))

(defn parse-handful [cubes]
  (->> cubes
       (map parse-single-color)
       (mapcat (fn [[n c]] [c n]))
       (apply hash-map)))

(defn parse-draws [draws]
  (->> (string/split draws #"; ")
       (map #(string/split % #", "))
       (map parse-handful)
       (into [])))

(defn parse-game [line]
  (let [[_ gid draws] (re-find #"Game (\d+): (.*)" line)]
    {:id    (parse-long gid)
     :draws (parse-draws draws)}))

(def games (->> lines (map parse-game) (into [])))

(def total-cubes
  {:red   12
   :green 13
   :blue  14})

(def colors (keys total-cubes))

(defn legal-draw? [total draw]
  (every? (fn [[color n]]
            (<= n (total color)))
          draw))

(legal-draw? total-cubes {:red 12 :green 13})
(legal-draw? total-cubes {:red 12 :green 14})

(->> games
     (filter (fn [g]
               (->> g :draws (every? #(legal-draw? total-cubes %)))))
     (map :id)
     (reduce +))

;; Part II
(defn cubes-power [cubes]
  (->> cubes
       vals
       (reduce *)))

(cubes-power total-cubes)

(defn minimal-set [colors draws]
  (->> colors
       (map (fn [c]
              (->> draws
                   (keep c)
                   (apply max))))))

(minimal-set colors [{:blue 3 :red 4} {:blue 6 :green 2 :red 1} {:green 2}])

(->> games
     (map :draws)
     (map #(minimal-set colors %))
     (map #(reduce * %))
     (reduce +))
