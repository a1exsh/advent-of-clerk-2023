;; # 🎄 Advent of Clerk: 2023: Day 3: Gear Ratios
(ns advent-of-clerk.year-2023.day-03
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as string]))

(def example "
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

;; ## Parsing the input
(def input (slurp "input/2023/03"))
;;(def input (string/trim example))

(def lines (string/split-lines input))

(def engine
  {:lines lines
   :nrows (count lines)
   :ncols (count (first lines))})

(defn append-dig [num x dig]
  (-> num
      (update :val #(+ (* 10 %) dig))
      (assoc :right x)))

(append-dig {:val 4 :left 0 :right 0} 1 6)

(defn scan-xyc [acc [x y c]]
  (if (Character/isDigit c)
    (let [dig (- (int c) (int \0))
          nums (:nums acc)
          nlst (dec (count nums))]
      (if (or (> 0 nlst)
              (> (dec x) (:right (get nums nlst))))
        (update acc :nums conj {:val dig :nline y :left x :right x})
        (update-in acc [:nums nlst] append-dig x dig)))
    (if (= \* c)
      (update acc :gears conj x)        ; for part II
      acc)))

(scan-xyc {:nums []} [0 0 \4])
(scan-xyc {:nums [{:val 4, :left 0, :right 0}]} [1 0 \6])
(scan-xyc {:nums [{:val 46, :left 0, :right 1}]} [2 0 \7])
(scan-xyc {:nums [{:val 46, :left 0, :right 1}]} [3 0 \7])

(def scan-lines
  (->> engine
       :lines
       (map (fn [y row]
              (->> row
                   (map (fn [x c]
                          [x y c])
                        (range))
                   (reduce scan-xyc {:nums [] :gears []})))
            (range))))

(defn engine-symbol? [c]
  (not (or (= \. c)
           (Character/isDigit c))))

(engine-symbol? \.)
(engine-symbol? \1)
(engine-symbol? \*)

(defn char-at-xy [{:keys [lines nrows ncols]} x y]
  (if (and (<= 0 y (dec nrows))
           (<= 0 x (dec ncols)))
    (nth (nth lines y) x)
    \.))

(char-at-xy engine -1 -1)
(char-at-xy engine 0 0)
(char-at-xy engine 10 10)

(defn part-number? [engine nline {:keys [left right]}]
  (let [xys (for [y (range (- nline 1) (+ nline 2))
                  x (range (- left  1) (+ right 2))]
              [x y])]
    (->> xys
         (map (fn [[x y]]
                (char-at-xy engine x y)))
         (some engine-symbol?))))

(part-number? engine 0 {:left 0 :right 2 :val 467})
(part-number? engine 0 {:left 5 :right 7 :val 144})

(->> scan-lines
     (mapcat (fn [y line]
               (->> line
                    :nums
                    (map (fn [num]
                           (if (part-number? engine y num)
                             (:val num)
                             0)))))
             (range))
     (reduce +))

;; Part II

(defn num-at-xy [scans x y]
  (->> (nth scans y)
       :nums
       (filter (fn [{:keys [left right]}]
                 (<= left x right)))
       first))

(num-at-xy scan-lines 0 0)
(num-at-xy scan-lines 3 0)

(defn adjacent-nums [scans nline x]
  (let [xys (for [y (range (- nline 1) (+ nline 2))
                  x (range (- x     1) (+ x     2))]
              [x y])]
    (->> xys
         (map (fn [[x y]]
                (num-at-xy scans x y)))
         (keep identity)
         distinct)))

(adjacent-nums scan-lines 1 3)

(defn gear-ratio [scans nline x]
  (let [nums (adjacent-nums scans nline x)]
    (if (-> nums count (= 2))
      (->> nums
           (map :val)
           (reduce *))
      0)))

(gear-ratio scan-lines 1 3)

(->> scan-lines
     (mapcat (fn [y line]
               (->> line
                    :gears
                    (map (fn [x]
                           (gear-ratio scan-lines y x)))))
             (range))
     (reduce +))
