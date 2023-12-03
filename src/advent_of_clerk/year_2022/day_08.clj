;; # 🎄 Advent of Clerk: 2022: Day 8: Treetop Tree House
(ns advent-of-clerk.year-2022.day-08
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

;; ## Visualization
(defn render [trees scale color-fn]
  (binding [*warn-on-reflection* true]
    (let [height (count trees)
          width  (count (first trees))
          img (BufferedImage. (* scale width)
                              (* scale height)
                              BufferedImage/TYPE_3BYTE_BGR)
          gfx (.createGraphics img)]
      (doseq [j (range height)
              i (range width)]
        (.setColor gfx (color-fn (nth (nth trees j) i)))
        (.fillRect gfx (* scale i) (* scale j) scale scale))
      img)))

(def scale 8)
(render puzzle scale #(Color. 0 (int (+ 30 (* 25 %))) 0))

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
(defn transposev [matrix]
  (vec (map vec
            (partition (count matrix)
                       (apply interleave matrix)))))

(transposev puzzle)

(def from-top
  (->> puzzle
       transposev
       (map scan-row)
       transposev))

(def from-bottom
  (->> puzzle
       transposev
       (map (comp reverse scan-row reverse))
       transposev))

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

(render from-many scale #(if (= 1 %) Color/BLACK Color/WHITE))

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

(def scenic-score
  (vec (for [row (range (count puzzle))]
         (vec (for [col (range (count (first puzzle)))]
                (apply * (map #(% puzzle row col)
                              [look-east look-south look-west look-north])))))))

(def max-scenic-score
  (reduce max (mapcat identity scenic-score)))

;;
;; Let's render the scenic score per tree.  We will need a HSL to RGB
;; conversion to make it nicer:
;;
;; ```js
;; // input: h as an angle in [0,360] and s,l in [0,1] - output: r,g,b in [0,1]
;; function hsl2rgb(h,s,l) 
;; {
;;    let a=s*Math.min(l,1-l);
;;    let f= (n,k=(n+h/30)%12) => l - a*Math.max(Math.min(k-3,9-k,1),-1);
;;    return [f(0),f(8),f(4)];
;; }
;; ```
;;
;; https://stackoverflow.com/a/64090995
;;
(defn hsl->color [^long h ^double s ^double l]
  (let [a (* s (Math/min l (- 1.0 l)))
        f (fn [n]
            (let [k (int (mod (+ n (/ h 30)) 12))]
              (* 255 (- l (* a (Math/max (Math/min (- k 3)
                                                   (Math/min (- 9 k) 1))
                                         -1))))))]
    (Color. (int (f 0)) (int (f 8)) (int (f 4)))))

(render scenic-score
        scale
        #(hsl->color 120 1.0 (/ (Math/log (inc %))
                                (Math/log (inc max-scenic-score)))))
