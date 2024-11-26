;; # 🎄 Advent of Clerk

;; [Advent of Code](https://adventofcode.com) with
;; [Clerk](https://clerk.vision).
(ns advent-of-clerk.index
  {:nextjournal.clerk/visibility {:code :hide :result :hide}}
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [nextjournal.clerk :as clerk]))

(def years (range 2022 2024))

(defn- build-year-index-path
  [year]
  (str (fs/file "src"
                "advent_of_clerk"
                (format "year_%d" year)
                "index.clj")))

(defn build-paths-for-year
  "Computes the paths to build by looking for files in
  `src/advent_of_clerk/year_YYYY/day_DD.clj` and filtering out unmodified
  templates (files with less than four lines).

  NB: it is also used in in the per-year index.clj files!"
  [year]
  (->> (range 25)
       (keep (fn [day]
               (let [f (fs/file "src"
                                "advent_of_clerk"
                                (format "year_%d" year)
                                (format "day_%02d.clj" day))]
                 (when (and (.exists f)
                            (< 3 (count (str/split-lines (slurp f)))))
                   (str f)))))))

(defn build-index-paths
  []
  (map build-year-index-path years))

(defn build-paths
  "Returns paths to all files to build: this is referred to from deps.edn as :paths-fn."
  []
  (mapcat (fn [year]
            (cons (build-year-index-path year)
                  (build-paths-for-year year)))
          years))

#_(build-paths)

(defn render-toc
  [paths nsplit]
  (clerk/html (->> paths
                   (mapv (fn [path]
                           [:li [:a {:href (clerk/doc-url path)}
                                 (-> path
                                     slurp
                                     str/split-lines
                                     first
                                     (str/split #": " nsplit)
                                     last)]]))
                   (into [:ul]))))

^{::clerk/no-cache true
  ::clerk/visibility {:result :show}}
(render-toc (build-index-paths) 2)
