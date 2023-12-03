;; # 🎄 Advent of Clerk

;; [Advent of Code](https://adventofcode.com) with
;; [Clerk](https://clerk.vision).
(ns advent-of-clerk.index
  {:nextjournal.clerk/visibility {:code :hide :result :hide}}
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [nextjournal.clerk :as clerk]))

(defn build-index-paths
  "Computes the paths to build by looking for files in
  `src/advent_of_clerk/year_*/index.clj`."
  []
  (->> (range 2022 2024)
       (map (fn [year]
              (str (fs/file "src"
                            "advent_of_clerk"
                            (format "year_%02d" year)
                            "index.clj"))))))

#_(build-index-paths)

(defn build-paths-for-year
  "Computes the paths to build by looking for files in
  `src/advent_of_clerk/year_YYYY/day_DD.clj` and filtering out unmodified
  templates (files with less than four lines)."
  [year]
  (->> (range 25)
       (keep (fn [day]
               (let [f (fs/file "src" "advent_of_clerk" year (format "day_%02d.clj" day))]
                 (when (and (.exists f)
                            (< 3 (count (str/split-lines (slurp f)))))
                   (str f)))))))

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
