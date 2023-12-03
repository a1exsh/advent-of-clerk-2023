;; # 🎄 Advent of Clerk: 2022: Day 13: Distress Signal
(ns advent-of-clerk.year-2022.day-13
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as string]
            [clojure.edn :as edn]))

;; ## Parsing the input
(def example
  (string/trim "
[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]"))

(def input (slurp "input/2022/13"))

(defn parse [s]
  (->> (string/split s #"\n\n")
       (map #(->> % string/split-lines (map edn/read-string)))))

(def packet-pairs (parse input #_ example))

;; ## Part I
(defn ensure-vector [x]
  (if (vector? x)
    x
    (vector x)))

(defn cmp [x y]
  (cond
    ;; we know that at least one of [x, y] is not nil
    (nil? x) -1
    (nil? y)  1
    (and (integer? x) (integer? y)) (compare x y)
    (and (seqable? x) (seqable? y)) (let [x1 (first x)
                                          y1 (first y)]
                                      (if (and (nil? x1)
                                               (nil? y1))
                                        0 ;; bail early if both are depleted
                                        (let [c (cmp x1 y1)]
                                          (if (not= 0 c)
                                            c
                                            (recur (rest x)
                                                   (rest y))))))

    :else (cmp (ensure-vector x)
               (ensure-vector y))))

(->> packet-pairs
     (map #(apply cmp %))
     (map-indexed (fn [i c]
                    (if (neg? c)
                      (inc i)
                      0)))
     (reduce + 0))

;; ## Part II
(def divider1 [[2]])
(def divider2 [[6]])

(def packets
  (concat [divider1 divider2]
          (mapcat identity packet-pairs)))

(def ordered-packets
  (->> packets
       (sort cmp)
       (into [])))

(defn packet-index [xs x]
  (->> xs
       (take-while #(not= x %))
       count
       inc))

(def decoder-key
  (* (packet-index ordered-packets divider1)
     (packet-index ordered-packets divider2)))
