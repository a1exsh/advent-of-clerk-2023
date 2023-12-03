;; # 🎄 Advent of Clerk: 2022: Day 17: Pyroclastic Flow
(ns advent-of-clerk.year-2022.day-17
  (:require [nextjournal.clerk :as clerk])
  (:import [java.awt Color]
           [java.awt.image BufferedImage]))

;; ## Loading the input

(def example ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(def input (slurp "input/2022/17"))

(def puzzle example #_ input)

(def total-rocks 5 #_ 2022)

(def scale 16)

;; ## Part I
(def shapes
  [["####"]
   
   [".#."
    "###"
    ".#."]

   ["..#"
    "..#"
    "###"]

   ["#"
    "#"
    "#"
    "#"]

   ["##"
    "##"]])

;; It seems that we will have to just literally play this Tetris game,
;; tracking each step in the memory of our program.
;;
;; Since we expect to track a lot of mutations, let's (carefully!) use some
;; char arrays.
;;
;; Let's start by calculating the maximum chamber height for the puzzle input
;; that we might need:
;;
(def chamber-height
  (->> shapes
       repeat
       (mapcat identity)
       (map count)
       (take total-rocks)
       (reduce + 7)))                   ; add 7 rows for a buffer at the top

(def chamber-width 7)

(defrecord Chamber [width height arr top-y])

(defn index-of [chamber x y]
  (+ x (* y (:width chamber))))

(defn value-at [chamber [x y]]
  (aget (:arr chamber) (index-of chamber x y)))

(defn set-value-at! [chamber [x y] v]
  (aset (:arr chamber) (index-of chamber x y) v))

;; ### Visualization
(defn render [{:keys [width height] :as chamber} scale]
  (binding [*warn-on-reflection* true]
    (let [img (BufferedImage. (* scale width)
                              (* scale height)
                              BufferedImage/TYPE_3BYTE_BGR)
          gfx (.createGraphics img)]
      (doseq [j (range height)
              i (range width)
              :let [color (case (value-at chamber [i j])
                            \. Color/BLACK
                            \# Color/GRAY
                            \@ Color/WHITE)]]
        (.setColor gfx color)
        (.fillRect gfx (* scale i) (* scale j) scale scale))
      img)))

;; Back to the actual puzzle...

(def step-0
  {:moves puzzle
   :shapes (cycle shapes)
   :chamber (map->Chamber {:width chamber-width
                           :height chamber-height
                           :arr (char-array (* chamber-width chamber-height)
                                            \.)
                           :top-y (dec chamber-height)} ; it's actually bottom
                          )})

;;
;; Each rock appears so that its left edge is two units away from the left
;; wall and its bottom edge is three units above the highest rock in the room
;; (or the floor, if there isn't one).
;;
(defn appear-rock! [chamber rock-shape]
  (let [top (- (:top-y chamber) 3 (dec (count rock-shape)))
        lft 2]
    (doseq [y (range (count rock-shape))
            x (range (count (first rock-shape)))]
      (set-value-at! chamber
                     [(+ lft x) (+ top y)]
                     (case (get-in rock-shape [y x])
                       \. \.
                       \# \@)))))

(appear-rock! (:chamber step-0) (first shapes))

^::clerk/no-cache
(render (:chamber step-0) scale)

(def chamber-viewer
  { ;; :transform-fn (comp (clerk/update-val symbol)
   ;;                     clerk/mark-presented)
   :render-fn
   `(fn [x]
      [:canvas
       {:id "chamber"
        :width 100
        :height 100
        :on-click #(let [canvas (.-target %)
                         ctx (.getContext canvas "2d")]
                     (.fillRect ctx 0 0 100 100))}])})

^{::clerk/viewer chamber-viewer}
(:chamber step-0)


(defn make-move [{:keys [moves shapes chamber]}]
  (let [move (first moves)]
    ))
