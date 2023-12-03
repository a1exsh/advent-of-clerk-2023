;; # 🎄 Advent of Clerk: 2022: Day 7: No Space Left On Device
(ns advent-of-clerk.year-2022.day-07
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
;; current directory as a vector of directory names; and `"/"` — the root of
;; the filesystem:
{:dir ["/""a" "e"]
 "/" {"a" {"e" {"i" 584}
           "f" 29116
           "g" 2557
           "h.lst" 62596}
      "b.txt" 14848514}}

(defn parse-line [sh s]
  #_(println s "\t\t" sh)
  (case s
    "$ cd /"  (assoc  sh :dir ["/"])
    "$ cd .." (update sh :dir pop)
    "$ ls"    sh ;; no update, just skip
    ;; else
    (if-let [[_ name] (re-matches #"\$ cd (.*)" s)]
      (update sh :dir conj name)

      (if-let [[_ data name] (re-matches #"([^ ]+) (.*)" s)]
        (assoc-in sh (conj (:dir sh) name)
                  (if (= "dir" data)
                    {}
                    (parse-long data)))

        (throw (ex-info (str "Didn't understand: " s)
                        {:sh sh}))))))

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
  (postwalk (fn [x]
              #_(println x)
              (if (map? x)
                (assoc x :size (->> x
                                    vals
                                    (map #(if (map? %)
                                            (:size %)
                                            %))
                                    (reduce +)))
                x))
            (shell "/")))

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
