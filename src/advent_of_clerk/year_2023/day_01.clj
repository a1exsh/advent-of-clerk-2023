;; # 🎄 Advent of Clerk: 2023: Day 1: Trebuchet?!
(ns advent-of-clerk.year-2023.day-01
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as string]))

;; (def example
;;   "1abc2
;; pqr3stu8vwx
;; a1b2c3d4e5f
;; treb7uchet")

;; ## Parsing the input
(def input (slurp "input/2023/01"))

(def lines (string/split-lines input))

(defn calibration-value [line]
  (let [digits (re-seq #"[0-9]" line)]
    (parse-long (str (first digits)
                     (last digits)))))

(->> lines
     (map calibration-value)
     (reduce +))

;; Part II

(def example2
  "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(def spelled-digits
  ["one" "two" "three" "four" "five" "six" "seven" "eight" "nine"])

(def true-digits
  (concat (map str (range 1 10))
          spelled-digits))

(defn first-true-digit [line]
  (->> true-digits
       (map (fn [digit]
              {:index (string/index-of line digit)
               :digit digit}))
       (filter (fn [{:keys [index digit]}]
                 (some? index)))
       (sort-by :index)
       first
       :digit))

(def example-line2 "7pqrstsixteen")
(first-true-digit example-line2)

(defn last-true-digit [line]
  (->> true-digits
       (map (fn [digit]
              {:index (string/last-index-of line digit)
               :digit digit}))
       (filter (fn [{:keys [index digit]}]
                 (some? index)))
       (sort-by :index)
       last
       :digit))

(last-true-digit example-line2)

(def true-digit-value
  {"1" 1
   "2" 2
   "3" 3
   "4" 4
   "5" 5
   "6" 6
   "7" 7
   "8" 8
   "9" 9
   "one" 1
   "two" 2
   "three" 3
   "four" 4
   "five" 5
   "six" 6
   "seven" 7
   "eight" 8
   "nine" 9})

(defn true-calibration-value [line]
  (parse-long (str (true-digit-value (first-true-digit line))
                   (true-digit-value (last-true-digit line)))))

(true-calibration-value example-line2)

(->> lines ;; (->> example2 string/split-lines)
     (map true-calibration-value)
     (reduce +))
