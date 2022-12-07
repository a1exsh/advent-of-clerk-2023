;; # 🎄 Advent of Clerk: Day 7: No Space Left On Device
(ns advent-of-clerk.day-07
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as string]
            [clojure.walk :refer [postwalk]]))

;; ## Parsing the input
(def example "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(def input (slurp "input/2022/07"))

;; We will represent the input during parsing as a map of `:dir` — the path to
;; current directory as a vector of directory names; and `:fs` — the nested
;; map of files and directories, e.g:
{:dir ["a" "e"]
 :fs {"a" {"e" {"i" 584}
           "f" 29116
           "g" 2557
           "h.lst" 62596}
      "b.txt" 14848514}}

(defn parse-line [sh s]
  #_(println s "\t\t" sh)
  (case s
    "$ cd /"  (assoc  sh :dir [])
    "$ cd .." (update sh :dir pop)
    "$ ls"    sh ;; no update, just skip
    ;; else
    (if-let [[_ name] (re-find #"^\$ cd (.*)$" s)]
      (update sh :dir conj name)

      (if-let [[_ data name] (re-matches #"^([^ ]+) (.*)$" s)]
        (update sh :fs
                assoc-in (conj (:dir sh) name)
                (if (= "dir" data)
                  {}
                  (parse-long data)))

        (throw (ex-info (str "Didn't understand: " s)
                        {:sh sh}))))))

(update {} :fs assoc-in (conj [] :a) {})

(defn parse [s]
  (->> s
       string/split-lines
       (reduce parse-line {})))

(def shell (parse input #_example))

;; ## Part I
;;
;; Walk the filesystem tree depth-first, calculating the size of directories
;; bottom-down, then walk the tree again, producing the sequence of nodes,
;; that we then filter by size:
;;
(def filesystem
  (->> shell
       :fs
       (postwalk (fn [x]
                   (println x)
                   (if (map? x)
                     (assoc x :size (->> x
                                         vals
                                         (map #(if (map? %)
                                                 (:size %)
                                                 %))
                                         (reduce +)))
                     x)))))

(->> filesystem
     (tree-seq map? vals)
     (filter map?)
     (map :size)
     (filter #(<= % 100000))
     (reduce +))

;; ## Part II

(def total-space    70000000)
(def min-free-space 30000000)

(def used-space (filesystem :size))
(def free-space (- total-space used-space))
(def needed-space (- min-free-space free-space))

(->> filesystem
     (tree-seq map? vals)
     (filter map?)
     (map :size)
     (filter #(>= % needed-space))
     (apply min))
