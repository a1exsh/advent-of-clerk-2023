;; # 🎄 Advent of Clerk: 2022: Day 11: Monkey in the Middle
(ns advent-of-clerk.year-2022.day-11
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

;; We need it again here:
(defn transpose [matrix]
  (partition (count matrix)
             (apply interleave matrix)))

;; ## Part I
(defn monkey-inspects-item
  [worry-fn monkeys {:keys [op div-by f t] :as m} item]
  (let [[o v] op
        value (if (= "old" v)
                item
                (parse-long v))
        worry (case o
                "+" (+ item value)
                "*" (* item value))
        worr' (worry-fn worry)
        test  (= 0 (rem worr' div-by))
        ->to  (if test t f)]
    (update-in monkeys [->to :items] conj worr')))

(def monkey-inspects-item-1 (partial monkey-inspects-item #(quot % 3)))

(defn play-turn [inspect-fn monkeys n]
  (let [m     (nth monkeys n)
        items (:items m)]
    (reduce #(inspect-fn %1 m %2)
            (update monkeys n assoc :items Q :inspected (count items))
            items)))

(def play-turn-1 (partial play-turn monkey-inspects-item-1))

(def first-20-turns-1
  (->> (range 20)
       (reductions (fn [monkeys _]
                     (reduce play-turn-1
                             monkeys
                             (range (count monkeys))))
                   puzzle)))

(def inspected-count-per-turn-per-monkey-1
  (map #(map :inspected %) first-20-turns-1))

(def inspected-count-per-monkey-per-turn-1
  (transpose inspected-count-per-turn-per-monkey-1))

(def inspected-count-per-monkey-1
  (map #(reduce + %) inspected-count-per-monkey-per-turn-1))

(def top-two-busiest-monkeys-1
  (->> inspected-count-per-monkey-1
       (sort >)
       (take 2)))

(def monkey-business-level-1
  (apply * top-two-busiest-monkeys-1))

;; ## Part II
;;
;; This is one of those rare times when the math learned in university can be
;; applied directly ;)
;;
(def ring (->> puzzle (map :div-by) (apply *)))

(def monkey-inspects-item-2 (partial monkey-inspects-item #(mod % ring)))

(def play-turn-2 (partial play-turn monkey-inspects-item-2))

(def first-10000-turns-2
  (->> (range 10000)
       (reductions (fn [monkeys _]
                     (reduce play-turn-2
                             monkeys
                             (range (count monkeys))))
                   puzzle)))

(def inspected-count-per-turn-per-monkey-2
  (map #(map :inspected %) first-10000-turns-2))

(def inspected-count-per-monkey-per-turn-2
  (transpose inspected-count-per-turn-per-monkey-2))

(def inspected-count-per-monkey-2
  (map #(reduce + %) inspected-count-per-monkey-per-turn-2))

(def top-two-busiest-monkeys-2
  (->> inspected-count-per-monkey-2
       (sort >)
       (take 2)))

(def monkey-business-level-2
  (apply * top-two-busiest-monkeys-2))
