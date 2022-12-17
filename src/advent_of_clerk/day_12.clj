;; # 🎄 Advent of Clerk: Day 12: Hill Climbing Algorithm
(ns advent-of-clerk.day-12
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as string])
  (:import [clojure.lang PersistentQueue]
           [java.awt Color]
           [java.awt.image BufferedImage]))

;; ## Parsing the input
(def example
  (string/trim "
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"))

(def input (slurp "input/2022/12"))

(defn parse [s]
  (->> s
       string/split-lines
       (mapv vec)))

(def puzzle (parse input #_ example))

;; ## Visualization
(defn render [mat scale color-fn]
  (binding [*warn-on-reflection* true]
    (let [height (count mat)
          width  (count (first mat))
          img (BufferedImage. (* scale width)
                              (* scale height)
                              BufferedImage/TYPE_3BYTE_BGR)]
      (doseq [j (range height)
              i (range width)
              :let [ji    [j i]
                    v     (get-in mat ji)
                    color (color-fn v ji)
                    rgb   (.getRGB color)]
              y (range (* scale j) (* scale (inc j)))
              x (range (* scale i) (* scale (inc i)))]
        (.setRGB ^BufferedImage img (int x) (int y) (int rgb)))

      img)))

(def scale 6 #_ 64)

;; a..z: 5..255
(render puzzle scale (fn [h _]
                       (case h
                         \S Color/WHITE
                         \E Color/BLUE
                         (let [x (int (+ 5 (* 10 (- (int h) (int \a)))))]
                           (Color. 0 x 0)))))

;; ## Part I
(def height (count puzzle))
(def width  (count (first puzzle)))

(defn find-yx-of [mat v]
  (->> (for [y (range height)
             x (range width)]
         [y x])
       (filter #(->> % (get-in mat) (= v)))
       first))

(def start-yx
  (find-yx-of puzzle \S))

(def end-yx
  (find-yx-of puzzle \E))

(defn neigh-yxs [height-map [y x]]
  (let [height (count height-map)
        width  (count (first height-map))]
    (keep identity [(when (> x 0) [y (dec x)])
                    (when (> y 0) [(dec y) x])
                    (when (< x (dec width))  [y (inc x)])
                    (when (< y (dec height)) [(inc y) x])])))

(defn height-at [height-map yx]
  (let [hyx (get-in height-map yx)]
    (case hyx
      \S \a
      \E \z
      hyx)))

(defn next-step-yxs [height-map path-map yx]
  (let [len  (->> yx (get-in path-map) count)
        maxh (->> yx (height-at height-map) int inc)]
    (->> yx
         (neigh-yxs height-map)
         (filter #(->> % (height-at height-map) int (>= maxh)))
         (filter #(let [lyx (->> % (get-in path-map) count)]
                    (or (= 0 lyx)
                        (< len lyx)))))))

(defn make-step [height-map {:keys [next-yxs path-map] :as step}]
  (reduce (fn [s yx]
            (let [cand-yxs (next-step-yxs height-map path-map yx)
                  pyx      (get-in path-map yx)]
              (-> s
                  (update :next-yxs #(apply conj % cand-yxs))
                  (update :path-map (fn [pm]
                                      (reduce #(assoc-in %1 %2 (conj pyx %2))
                                              pm
                                              cand-yxs))))))
          (assoc step :next-yxs #{})
          next-yxs))

(def empty-path-map
  (vec (for [_ (range height)]
         (vec (for [_ (range width)]
                #{})))))

(def step-0 {:next-yxs #{start-yx}
             :path-map (assoc-in empty-path-map start-yx #{start-yx})})

(neigh-yxs puzzle start-yx)
(next-step-yxs puzzle (:path-map step-0) start-yx)

(def step-fn (partial make-step puzzle))

(step-fn (step-fn (step-fn step-0)))

(def steps (iterate step-fn step-0))

(defn render-step [{path-map :path-map}]
  (let [longest-path (->> path-map
                          (mapcat identity)
                          (sort-by #(if (contains? % end-yx)
                                      Integer/MAX_VALUE
                                      (count %)))
                          last)
        max-path-len (count longest-path)]
    (render path-map
            scale
            (fn [p yx]
              (Color.
               ;; RED
               (if (contains? longest-path yx) 128 0)
               ;; GREEN
               (let [h (height-at puzzle yx)]
                 (int (+ 5 (* 10 (- (int h) (int \a))))))
               ;; BLUE
               (let [x (count p)]
                 (if (= 0 x)
                   0
                   (-> x (* 191.0) (/ max-path-len) (+ 64) int))))))))

(def slider-viewer
  {:transform-fn (comp (clerk/update-val symbol)
                       clerk/mark-presented)
   :render-fn '(fn [x]
                 [:input {:type :range
                          :value (:counter @@(resolve x))
                          :min 0
                          :max 520
                          :on-change #(swap! @(resolve x) assoc :counter (int (.. % -target -value)))}])})

^::clerk/sync
(defonce step* (atom {:counter 0}))

@step*

^{::clerk/viewer slider-viewer}
`step*

(def step-n (:counter @step*))
(def step (nth steps step-n))

(render-step step)

(def last-step
  (->> steps
       (filter #(-> % :path-map (get-in end-yx) count (> 0)))
       first))

(def path-to-end
  (-> last-step
      :path-map
      (get-in end-yx)))

(def total-steps
  (dec (count path-to-end)))

(comment
  ^{:nextjournal.clerk/visibility {:code :hide}}
  (->> steps
       (take total-steps)
       (take-last 5)
       (map render-step)))

;; ## Part II
(defn next-step2-yxs [height-map path-map yx]
  (let [len  (->> yx (get-in path-map) count)
        minh (->> yx (height-at height-map) int dec)]
    (->> yx
         (neigh-yxs height-map)
         (filter #(->> % (height-at height-map) int (<= minh)))
         (filter #(let [lyx (->> % (get-in path-map) count)]
                    (or (= 0 lyx)
                        (< len lyx)))))))

(defn make-step2 [height-map {:keys [next-yxs path-map] :as step}]
  (reduce (fn [s yx]
            (let [cand-yxs (next-step2-yxs height-map path-map yx)
                  pyx      (get-in path-map yx)]
              (-> s
                  (update :next-yxs #(apply conj % cand-yxs))
                  (update :path-map (fn [pm]
                                      (reduce #(assoc-in %1 %2 (conj pyx %2))
                                              pm
                                              cand-yxs))))))
          (assoc step :next-yxs #{})
          next-yxs))

(def step2-0
  {:next-yxs #{end-yx}
   :path-map (assoc-in empty-path-map end-yx #{end-yx})})

(def step2-fn (partial make-step2 puzzle))

(def steps2 (iterate step2-fn step2-0))

(defn path-reaching-bottom? [height-map path]
  (->> path
       (map #(->> % (height-at height-map)))
       (some #{\a})
       boolean))

(defn render-step2 [{path-map :path-map}]
  (let [longest-path (->> path-map
                          (mapcat identity)
                          (sort-by #(if (path-reaching-bottom? puzzle %)
                                      Integer/MAX_VALUE
                                      (count %)))
                          last)
        max-path-len (count longest-path)]
    (render path-map
            scale
            (fn [p yx]
              (Color.
               ;; RED
               (if (contains? longest-path yx) 128 0)
               ;; GREEN
               (let [h (height-at puzzle yx)]
                 (int (+ 5 (* 10 (- (int h) (int \a))))))
               ;; BLUE
               (let [x (count p)]
                 (if (= 0 x)
                   0
                   (-> x (* 191.0) (/ max-path-len) (+ 64) int))))))))

^::clerk/sync
(defonce step2* (atom {:counter 0}))

@step2*

^{::clerk/viewer slider-viewer}
`step2*

(def step2-n (:counter @step2*))
(def step2 (nth steps2 step2-n))

(render-step2 step2)

(def last-step2
  (->> steps2
       (remove (fn [s]
                 (->> s
                      :path-map
                      (mapcat (fn [row]
                                (map #(path-reaching-bottom? puzzle %) row)))
                      (every? false?))))
       first))

(def path-to-bottom
  (->> last-step2
       :path-map
       (mapcat identity)
       (filter #(path-reaching-bottom? puzzle %))
       first))

(def total-steps2
  (dec (count path-to-bottom)))
