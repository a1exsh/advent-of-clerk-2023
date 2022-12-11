;; # 🎄 Advent of Clerk: Day 11: Monkey in the Middle
(ns advent-of-clerk.day-11
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as string])
  (:import [clojure.lang PersistentQueue]))

;; ## Parsing the input
(def example
  (string/trim "
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1"))

(def input (slurp "input/2022/11"))

(def Q PersistentQueue/EMPTY)

(defn parse-monkey [s]
  (let [[_ items op test tru fal] (string/split-lines s)]

    {:items  (->> items
                  (re-seq #"\d+")
                  (map parse-long)
                  (into Q))

     :inspected 0                       ; accumulate inspected items count

     :op     (let [[_ o v] (re-find #"new = old (.) (.+)" op)]
               [o v])                   ; NB: `v` may be a number or "old"

     :div-by (let [[_ d] (re-find #"divisible by (\d+)" test)]
               (parse-long d))

     :t      (let [[_ t] (re-find #"throw to monkey (\d+)" tru)]
               (parse-long t))

     :f      (let [[_ f] (re-find #"throw to monkey (\d+)" fal)]
               (parse-long f))}))

(defn parse [s]
  (->> (string/split s #"\n\n")
       (mapv parse-monkey)))

(def puzzle (parse input #_ example))

;; ## Part I
(defn monkey-inspects-item [monkeys {:keys [op div-by f t] :as m} item]
  (let [[o v] op
        value (if (= "old" v)
                item
                (parse-long v))
        worry (case o
                "+" (+ item value)
                "*" (* item value))
        wor3  (quot worry 3)
        test  (= 0 (rem wor3 div-by))
        ->to  (if test t f)]
    (update-in monkeys [->to :items] conj wor3)))

(defn play-turn [monkeys n]
  (let [m     (nth monkeys n)
        items (:items m)]
    (reduce #(monkey-inspects-item %1 m %2)
            (update monkeys n assoc :items Q :inspected (count items))
            items)))

(defn transpose [matrix]
  (partition (count matrix)
             (apply interleave matrix)))

(def first-20-turns
  (->> (range 20)
       (reductions (fn [monkeys _]
                     (reduce play-turn
                             monkeys
                             (range (count monkeys))))
                   puzzle)))

(def inspected-count-per-turn-per-monkey
  (map #(map :inspected %) first-20-turns))

(def inspected-count-per-monkey-per-turn
  (transpose inspected-count-per-turn-per-monkey))

(def inspected-count-per-monkey
  (map #(reduce + %) inspected-count-per-monkey-per-turn))

(def top-two-busiest-monkeys
  (->> inspected-count-per-monkey
       (sort >)
       (take 2)))

(def monkey-business-level
  (apply * top-two-busiest-monkeys))

;; ## Part II
