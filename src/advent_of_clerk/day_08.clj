;; # 🎄 Advent of Clerk: Day 8: Treetop Tree House
(ns advent-of-clerk.day-08
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as string])
  (:import [java.awt Color]
           [java.awt.image BufferedImage]))

;; ## Parsing the input
(def example
  (string/trim "
30373
25512
65332
33549
35390"))

(defn parse [s]
  (->> s
       string/split-lines
       (mapv (fn [line]
               (mapv #(-> % str parse-long)
                     line)))))

(def input (slurp "input/2022/08"))

(def puzzle (parse input #_ example))

;; ## Part I
;;
;; We will have to scan the map four times (from each direction) and then take
;; a union of all trees marked as visible, so that we count all of them, but
;; only _once_.
;;
(defn scan-row
  "Scan a row of trees and return the visible ones as 1, and obscured — as 0."
  [ts]
  (->> ts
       (reductions max -1)
       butlast
       (map (fn [l r]
              (if (> l r) 1 0))
            ts)))

(def from-left 
  (->> puzzle
       (map scan-row)))

(def from-right
  (->> puzzle
       (map (comp reverse scan-row reverse))))

;; To look from the top we will have to transpose the matrix:
(defn transpose [matrix]
  (vec (map vec
            (partition (count matrix)
                       (apply interleave matrix)))))

(transpose puzzle)

(def from-top
  (->> puzzle
       transpose
       (map scan-row)
       transpose))

(def from-bottom
  (->> puzzle
       transpose
       (map (comp reverse scan-row reverse))
       transpose))

;; Finally, we can take the union of the four views:
(def from-many
  (mapv (fn [& scans]
          (apply mapv (fn [& s]
                        (apply max s))
                 scans))
        from-left
        from-right
        from-top
        from-bottom))

(def height (count puzzle))
(def width  (count (first puzzle)))

(def white (Color. 255 255 255))
(def black (Color.   0   0   0))

(let [scale 16
      img   (BufferedImage. (* scale width)
                            (* scale height)
                            BufferedImage/TYPE_3BYTE_BGR)]
  (doseq [j (range height)
          i (range width)
          :let [rgb (.getRGB (if (= 1 (nth (nth from-many j) i))
                               black
                               white))]
          y (range (* scale j) (* scale (inc j)))
          x (range (* scale i) (* scale (inc i)))]
    (.setRGB img x y rgb))

  img)

(->> from-many
     (map #(reduce + %))
     (reduce +))

;; ## Part II
(defn look-east [mat row col]
  (let [ts  (nth mat row)
        len (count ts)
        t   (nth ts col)]
    #_(println {:len len :t t})
    (loop [d 0
           i (inc col)]
      #_(println {:d d :i i})
      (if (>= i len)
        d
        (let [ti (nth ts i)]
          (if (< ti t)
            (recur (inc d) (inc i))
            (inc d)))))))

(defn look-west [mat row col]
  (let [ts (nth mat row)
        t  (nth ts col)]
    #_(println {:t t})
    (loop [d 0
           i (dec col)]
      #_(println {:d d :i i})
      (if (< i 0)
        d
        (let [ti (nth ts i)]
          (if (< ti t)
            (recur (inc d) (dec i))
            (inc d)))))))

(defn look-south [mat row col]
  (let [len (count mat)
        t   (nth (nth mat row) col)]
    #_(println {:t t})
    (loop [d 0
           j (inc row)]
      #_(println {:d d :i i})
      (if (>= j len)
        d
        (let [tj (nth (nth mat j) col)]
          (if (< tj t)
            (recur (inc d) (inc j))
            (inc d)))))))

(defn look-north [mat row col]
  (let [t (nth (nth mat row) col)]
    #_(println {:t t})
    (loop [d 0
           j (dec row)]
      #_(println {:d d :i i})
      (if (< j 0)
        d
        (let [tj (nth (nth mat j) col)]
          (if (< tj t)
            (recur (inc d) (dec j))
            (inc d)))))))

(reduce max
        (for [row (range height)
              col (range width)]
          (apply * (map #(% puzzle row col)
                        [look-east look-south look-west look-north]))))
