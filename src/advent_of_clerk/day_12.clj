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

(defrecord Field [width height arr])

(defn parse [s]
  (let [lines  (string/split-lines s)
        height (count lines)
        width  (count (first lines))]
    (->Field width height (->> lines string/join char-array))))

(defn index-of [field x y]
  (+ (* (:width field) y)
     x))

(defn value-at [field [x y]]
  (aget (:arr field) (index-of field x y)))

(def ^:dynamic *mutable-arrays* false)

(defn set-value-at! [field [x y] v]
  (assert *mutable-arrays*)
  (aset (:arr field) (index-of field x y) v))

(defn print-field [{:keys [width height arr]}]
  (doseq [line (partition width arr)]
    (println line)))

(def puzzle (parse input #_ example))

;; ## Visualization
(def scale 16 #_ 64)

(defn render [{:keys [width height] :as field} scale color-fn]
  (binding [*warn-on-reflection* true]
    (let [img (BufferedImage. (* scale width)
                              (* scale height)
                              BufferedImage/TYPE_3BYTE_BGR)]
      (doseq [j (range height)
              i (range width)
              :let [ij    [i j]
                    v     (value-at field ij)
                    color (color-fn v ij)
                    rgb   (.getRGB color)]
              y (range (* scale j) (* scale (inc j)))
              x (range (* scale i) (* scale (inc i)))]
        (.setRGB ^BufferedImage img (int x) (int y) (int rgb)))
      img)))

(render puzzle scale (fn [h _]
                       (case h
                         \S Color/WHITE
                         \E Color/BLUE
                         (let [x (int (+ 5 (* 10 (- (int h) (int \a)))))]
                           (Color. 0 x 0)))))

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

(defn next-step-xys [height-map path-len-map xy]
  (let [len  (->> xy (value-at path-len-map))
        maxh (->> xy (height-at height-map) int inc)]
    (->> xy
         (neigh-xys height-map)
         (filter #(->> % (height-at height-map) int (>= maxh)))
         (filter #(let [lxy (value-at path-len-map %)]
                    (or (= 0 lxy)
                        (< len lxy)))))))

(defn field-copy [{:keys [width height arr]}]
  (->Field width height (into-array arr)))

(defn make-step [height-map {:keys [next-xys path-len-map] :as step}]
  #_(println "make-step" next-xys)
  #_(print-field path-len-map)
  (reduce (fn [{plm :path-len-map :as s} xy]
            (let [cand-xys (next-step-xys height-map plm xy)
                  lxy      (value-at plm xy)]
              #_(println "len:" lxy)
              (doseq [cxy cand-xys]
                #_(println cxy)
                (set-value-at! plm cxy (inc lxy)))
              (update s :next-xys #(apply conj % cand-xys))))
          (assoc step
                 :next-xys #{}
                 :path-len-map (field-copy path-len-map))
          next-xys))

(defn empty-path-len-map [{:keys [width height] :as field}]
  (->Field width height (long-array (* width height) 0)))

(def step-0
  (binding [*mutable-arrays* true]
    (let [path-len-map (empty-path-len-map puzzle)]
      (set-value-at! path-len-map start-xy 1)
      {:next-xys     #{start-xy}
       :path-len-map path-len-map})))

;; (neigh-xys puzzle start-xy)
;; (next-step-xys puzzle (:path-len-map step-0) start-xy)

(defn step-fn [height-map]
  (fn [step]
    (let [next-step (make-step height-map step)]
      #_(println "next-step")
      #_(print-field (:path-len-map next-step))
      next-step)))

#_(step-fn (step-fn step-0))

;;^{::clerk/visibility {:result :hide}}
;;^::clerk/no-cache

(def steps
  (binding [*mutable-arrays* true]
    (->> step-0
         (iterate (step-fn puzzle))
         #_(take 520)
         (take-while #(not (empty? (:next-xys %))))
         doall)))

(def first-step-reaching-end
  (->> steps
       (filter #(-> % :path-len-map (value-at end-xy) (> 0)))
       first))

;; (def path-to-end
;;   (-> last-step
;;       :path-map
;;       (get-in end-xy)))

(def shortest-path-len
  (-> first-step-reaching-end
      :path-len-map
      (value-at end-xy)
      dec))

(defn render-step [{path-len-map :path-len-map}]
  #_(println path-len-map)
  #_(print-field path-len-map)
  (let [longest-path (->> path-len-map :arr (reduce max 0))]
    #_(println "longest-path" longest-path)
    (render path-len-map
            scale
            (fn [l xy]
              #_(println l xy)
              (Color.
               ;; RED
               0 #_(if (contains? longest-path xy) 128 0)
               ;; GREEN
               (let [h (height-at puzzle xy)]
                 (int (+ 5 (* 10 (- (int h) (int \a))))))
               ;; BLUE
               (if (= 0 l)
                 0
                 (-> l (* 191.0) (/ longest-path) (+ 64) int)))))))

(defn slider-viewer [max-n]
  {:transform-fn (comp (clerk/update-val symbol)
                       clerk/mark-presented)
   :render-fn `(fn [x]
                 [:input {:type :range
                          :value (:counter @@(resolve x))
                          :min 0
                          :max ~max-n
                          :on-change #(swap! @(resolve x) assoc :counter (int (.. % -target -value)))}])})

^::clerk/sync
(defonce step* (atom {:counter shortest-path-len}))

@step*

^{::clerk/viewer (slider-viewer (dec (count steps)))}
`step*

(def step-n (:counter @step*))
(def step (nth steps step-n))

(render-step step)

;; ## Part II
;; (defn next-step2-xys [height-map path-map xy]
;;   (let [len  (->> xy (get-in path-map) count)
;;         minh (->> xy (height-at height-map) int dec)]
;;     (->> xy
;;          (neigh-xys height-map)
;;          (filter #(->> % (height-at height-map) int (<= minh)))
;;          (filter #(let [lxy (->> % (get-in path-map) count)]
;;                     (or (= 0 lxy)
;;                         (< len lxy)))))))

;; (defn make-step2 [height-map {:keys [next-xys path-map] :as step}]
;;   (reduce (fn [s xy]
;;             (let [cand-xys (next-step2-xys height-map path-map xy)
;;                   pxy      (get-in path-map xy)]
;;               (-> s
;;                   (update :next-xys #(apply conj % cand-xys))
;;                   (update :path-map (fn [pm]
;;                                       (reduce #(assoc-in %1 %2 (conj pxy %2))
;;                                               pm
;;                                               cand-xys))))))
;;           (assoc step :next-xys #{})
;;           next-xys))

;; (def step2-0
;;   {:next-xys #{end-xy}
;;    :path-map (assoc-in (empty-path-map puzzle)
;;                        end-xy
;;                        #{end-xy})})

;; (def step2-fn (partial make-step2 puzzle))

;; (def steps2
;;   (->> step2-0
;;        (iterate step2-fn)
;;        (take 5 #_20)
;;        #_(take-while #(not (empty? (:next-xys %))))))

;; (defn path-reaching-bottom? [height-map path]
;;   (->> path
;;        (map #(->> % (height-at height-map)))
;;        (some #{\a})
;;        boolean))

;; (def last-step2
;;   (->> steps2
;;        (remove (fn [s]
;;                  (->> s
;;                       :path-map
;;                       (mapcat (fn [row]
;;                                 (map #(path-reaching-bottom? puzzle %) row)))
;;                       (every? false?))))
;;        first))

;; (def path-to-bottom
;;   (->> last-step2
;;        :path-map
;;        (mapcat identity)
;;        (filter #(path-reaching-bottom? puzzle %))
;;        first))

;; (def total-steps2
;;   (dec (count path-to-bottom)))

;; (defn render-step2 [{path-map :path-map}]
;;   (let [longest-path (->> path-map
;;                           (mapcat identity)
;;                           (sort-by count)
;;                           last)
;;         max-path-len (count longest-path)]
;;     (render path-map
;;             scale
;;             (fn [p xy]
;;               (Color.
;;                ;; RED
;;                (if (contains? longest-path xy) 128 0)
;;                ;; GREEN
;;                (let [h (height-at puzzle xy)]
;;                  (int (+ 5 (* 10 (- (int h) (int \a))))))
;;                ;; BLUE
;;                (let [x (count p)]
;;                  (if (= 0 x)
;;                    0
;;                    (-> x (* 191.0) (/ max-path-len) (+ 64) int))))))))

;; ^::clerk/sync
;; (defonce step2* (atom {:counter 0}))

;; @step2*

;; ^{::clerk/viewer (slider-viewer (dec (count steps2)))}
;; `step2*

;; (def step2-n (:counter @step2*))
;; (def step2 (nth steps2 step2-n))

;; (render-step2 step2)
