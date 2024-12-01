;; # 🎄 Advent of Clerk: 2022: Day 12: Hill Climbing Algorithm
(ns advent-of-clerk.year-2022.day-12
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as string])
  (:import [java.awt Color]
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

(defrecord Field [width height arr])

(defn parse [s]
  (let [lines  (string/split-lines s)
        height (count lines)
        width  (count (first lines))]
    (->Field width height (->> lines string/join vec))))

(defn index-of [field x y]
  (+ (* (:width field) y)
     x))

(defn value-at [field [x y]]
  (get (:arr field) (index-of field x y)))

(defn set-value-at [field [x y] v]
  (let [elem-index (index-of field x y)]
    (assoc-in field [:arr elem-index] v)))

#_(defn print-field [{:keys [width height arr]}]
    (doseq [line (partition width arr)]
      (println line)))

(def puzzle (parse input #_ example))

;; ## Visualization
(def scale 5 #_ 64)

(defn render [{:keys [width height] :as field} scale color-fn]
  (binding [*warn-on-reflection* true]
    (let [img (BufferedImage. (* scale width)
                              (* scale height)
                              BufferedImage/TYPE_3BYTE_BGR)
          gfx (.createGraphics img)]
      (doseq [j (range height)
              i (range width)
              :let [ij    [i j]
                    color (color-fn (value-at field ij)
                                    ij)]]
        (.setColor gfx color)
        (.fillRect gfx (* scale i) (* scale j) scale scale))
      img)))

(defn green-shade [h]
  (int (+ 5
          (* 10
             (- (int h)
                (int \a))))))

(render puzzle scale (fn [h _]
                       (case h
                         \S Color/RED
                         \E Color/WHITE
                         (Color. 0 (green-shade h) 0))))

;; ## Part I
(defn find-xy-of [{:keys [width height] :as field} v]
  (->> (for [y (range height)
             x (range width)]
         [x y])
       (filter #(->> % (value-at field) (= v)))
       first))

(def start-xy
  (find-xy-of puzzle \S))

(def end-xy
  (find-xy-of puzzle \E))

(defn neigh-xys [{:keys [width height] :as field} [x y]]
  (keep identity
        [(when (> x 0) [(dec x) y])
         (when (> y 0) [x (dec y)])
         (when (< x (dec width))  [(inc x) y])
         (when (< y (dec height)) [x (inc y)])]))

(defn height-at [height-map xy]
  (let [hxy (value-at height-map xy)]
    (case hxy
      \S \a
      \E \z
      hxy)))

(height-at puzzle start-xy)
(height-at puzzle   end-xy)

(defn next-step-xys [height-map path-map xy]
  (let [maxh (->> xy (height-at height-map) int inc)]
    (->> xy
         (neigh-xys height-map)
         (filter #(->> % (height-at height-map) int (>= maxh)))
         (filter #(->> % (value-at path-map) nil?)))))

(defn empty-path-map [{:keys [width height] :as field}]
  (let [n-elems (* width height)]
    (->Field width height (vec (repeat n-elems nil)))))

(defn path-map-copy [{:keys [width height arr]}]
  (->Field width height arr))

(defn make-step [height-map {:keys [next-xys path-map] :as step}]
  (reduce (fn [{pm :path-map :as s} xy]
            (let [cand-xys (next-step-xys height-map pm xy)
                  path-xy  (value-at pm xy)
                  stepped-pm (reduce #(set-value-at %1 %2 (cons xy path-xy))
                                     pm
                                     cand-xys)]
              (-> s
                  (assoc  :path-map stepped-pm)
                  (update :next-xys #(apply conj % cand-xys)))))
          (assoc step
                 :next-xys #{}
                 :path-map (path-map-copy path-map))
          next-xys))

(def step-0
  (let [path-map (empty-path-map puzzle)]
    {:next-xys #{start-xy}
     :path-map (set-value-at path-map start-xy '())}))

;; (neigh-xys puzzle start-xy)
;; (next-step-xys puzzle (:path-map step-0) start-xy)

(def step-fn (partial make-step puzzle))

;;^{::clerk/visibility {:result :hide}}
(time
 (def steps
   (->> step-0
        (iterate step-fn)
        (take-while #(not (empty? (:next-xys %))))
        (into []))))

(def total-steps (count steps))

(def first-step-reaching-end
  (->> steps
       (remove #(-> % :path-map (value-at end-xy) nil?))
       first))

(def shortest-path-to-end
  (-> first-step-reaching-end
      :path-map
      (value-at end-xy)))

(def shortest-path-len
  (count shortest-path-to-end))

(defn render-step [{:keys [next-xys path-map]}]
  (let [paths (->> next-xys (mapcat #(->> % (value-at path-map))) (into #{}))]
    (render path-map
            scale
            (fn [p xy]
              (Color.
               ;; RED
               (if (contains? next-xys xy)
                 255
                 (if (contains? paths xy)
                   128
                   0))
               ;; GREEN
               (green-shade (height-at puzzle xy))
               ;; BLUE
               (if (nil? p)
                 0
                 (-> p count (* 191.0) (/ (count paths)) (+ 64) int)))))))

(defn slider-viewer [max-value]
  {:transform-fn (comp (clerk/update-val symbol)
                       clerk/mark-presented)
   :render-fn `(fn [x]
                 [:input {:type :range
                          :value (:counter @@(resolve x))
                          :min 0
                          :max ~max-value
                          :on-change #(swap! @(resolve x)
                                             assoc
                                             :counter
                                             (int (.. % -target -value)))}])})

^::clerk/sync
(defonce step* (atom {:counter shortest-path-len}))
#_(reset! step* {:counter shortest-path-len})

^{::clerk/viewer (slider-viewer (dec total-steps))}
`step*

(def step-number (:counter @step*))
(def step-to-render (steps step-number))

(render-step step-to-render)

;; ## Part II
(defn next-step2-xys [height-map path-map xy]
  (let [minh (->> xy (height-at height-map) int dec)]
    (->> xy
         (neigh-xys height-map)
         (filter #(->> % (height-at height-map) int (<= minh)))
         (filter #(->> % (value-at path-map) nil?)))))

(defn make-step2 [height-map {:keys [next-xys path-map] :as step}]
  (reduce (fn [{pm :path-map :as s} xy]
            (let [cand-xys (next-step2-xys height-map pm xy)
                  path-xy  (value-at pm xy)
                  stepped-pm (reduce #(set-value-at %1 %2 (cons xy path-xy))
                                     pm
                                     cand-xys)]
              (-> s
                  (assoc  :path-map stepped-pm)
                  (update :next-xys #(apply conj % cand-xys)))))
          (assoc step
                 :next-xys #{}
                 :path-map (path-map-copy path-map))
          next-xys))

(def step2-0
  (let [path-map (empty-path-map puzzle)]
    {:next-xys #{end-xy}
     :path-map (set-value-at path-map end-xy '())}))

(def step2-fn (partial make-step2 puzzle))

;;^{::clerk/visibility {:result :hide}}
(time
  (def steps2
    (->> step2-0
         (iterate step2-fn)
         (take-while #(not (empty? (:next-xys %))))
         (into []))))

(def total-steps2 (count steps2))

total-steps2

(defn path-reaching-bottom? [height-map path]
  (->> path
       (map #(->> % (height-at height-map)))
       (some #{\a})
       boolean))

(def first-step-reaching-bottom
  (->> steps2
       (map (fn [{:keys [next-xys path-map] :as s}]
              (assoc s
                     :path-to-bottom
                     (->> next-xys
                          (map #(value-at path-map %))
                          (filter #(path-reaching-bottom? puzzle %))
                          (first)))))
       (filter :path-to-bottom)
       first))

(def shortest-path-to-bottom (:path-to-bottom first-step-reaching-bottom))

(def shortest-path-len2
  (dec (count shortest-path-to-bottom)))

(defn render-step2 [{:keys [next-xys path-map]}]
  (let [paths (->> next-xys (mapcat #(->> % (value-at path-map))) (into #{}))]
    (render path-map
            scale
            (fn [p xy]
              (let [h (height-at puzzle xy)]
                (if (and (= \a h)
                         (contains? next-xys xy))
                  Color/WHITE
                  (Color.
                   ;; RED
                   (if (contains? next-xys xy)
                     255
                     (if (contains? paths xy)
                       128
                       0))
                   ;; GREEN
                   (green-shade h)
                   ;; BLUE
                   (if (nil? p)
                     0
                     (-> p count (* 191.0) (/ (count paths)) (+ 64) int)))))))))

^::clerk/sync
(defonce step2* (atom {:counter shortest-path-len2}))
#_(reset! step2* {:counter shortest-path-len2})

^{::clerk/viewer (slider-viewer (dec (count steps2)))}
`step2*

(def step2-number (:counter @step2*))
(def step2-to-render (steps2 step2-number))

(render-step2 step2-to-render)
