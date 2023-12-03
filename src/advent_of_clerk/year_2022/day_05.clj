;; # 🎄 Advent of Clerk: 2022: Day 5: Supply Stacks
(ns advent-of-clerk.year-2022.day-05
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as string]))

;; ## Parsing the input
#_(def input "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(def input (slurp "input/2022/05"))

;;
;; The stacks description is a number of lines, each of the same length $l$.
;; The number of stacks $n$ can be found like this:
;; $$
;; l = 3 \times n + (n-1) = 4 \times n - 1;
;; n = \frac{l+1}{4}
;; $$.
;;
;; Parse the stacks as $1 \times d_i$ vectors — left to right, bottom to top,
;; where $d_i$ is the depth of the $i$-th vector.
;;
;; Finally, we need the result to be a vector of vectors: for the fast lookup
;; by the stack index, and for efficient pop/push on the stacks.
;;
(defn parse-stacks [s]
  (let [lines (string/split-lines s)
        depth (dec (count lines))
        l     (count (first lines))
        n     (/ (+ l 1) 4)]
    (into []
          (for [i (range n)
                :let [col (+ 1 (* 4 i))]]
            (into []
                  (for [j (range depth 0 -1)
                        :let [line (nth lines (dec j))
                              c    (nth line col)]
                        :when (not= c \ )]
                    c))))))

;;
;; Print the stacks in the same format as we got them on input.  We don't need
;; it to solve the puzzle, but it's helpful for debugging:
;;
(defn print-stacks [sts]
  (let [n     (count sts)
        depth (->> sts (map count) (apply max))]
    (doseq [j (range depth)]
      (println (string/join " "
                            (for [st sts
                                  :let [len (count st)
                                        k   (- depth 1 j)]]
                              (if (< k len)
                                (format "[%c]" (nth st k))
                                "   ")))))
    ;; add stack numbers at the bottom:
    (println (string/join " " (map #(format " %d " (inc %))
                                   (range n))))))

(defn parse-moves [s]
  (let [lines (string/split-lines s)]
    (for [l lines
          :let [[_ n f t] (re-matches #"move ([0-9]+) from ([0-9]+) to ([0-9]+)" l)]]
      {:crates (parse-long n)
       :from  (parse-long f)
       :to    (parse-long t)})))

(defn parse [s]
  (let [[stack-lines move-lines] (string/split s #"\n\n")
        stacks (parse-stacks stack-lines)]
    ;; safety net for the parse/print round-trip:
    (assert (= (str stack-lines "\n")
               (with-out-str (print-stacks stacks))))
    {:stacks stacks
     :moves  (parse-moves move-lines)}))

(def puzzle (parse input))

(with-out-str
  (print-stacks (:stacks puzzle)))

;; ## Part I
;; Now, finally we can start solving the puzzle...

(defn move-crate [stacks {:keys [from to]}]
  ;;
  ;; Printing the progress on the console is nice, but it's EXTREMELY slow —
  ;; about 30 seconds penalty!
  ;;
  #_(print-stacks stacks)
  #_(println)
  (let [from  (dec from)
        to    (dec to)
        crate (peek (nth stacks from))]
    (-> stacks
        (update from pop)
        (update to   conj crate))))

#_(move-crate (puzzle :stacks) (-> puzzle :moves first))

(defn make-move [stacks {:keys [crates from to]}]
  (reduce move-crate stacks (repeat crates {:from from :to to})))

#_(with-out-str
  (print-stacks (make-move (puzzle :stacks) (-> puzzle :moves first))))

(def part1-final-state
  (reduce make-move (puzzle :stacks) (puzzle :moves)))

(with-out-str
  (print-stacks part1-final-state))

(->> part1-final-state
     (map peek)
     string/join)

;; ## Part II

(defn move-crates-9001 [stacks {:keys [crates from to]}]
  #_(print-stacks stacks)
  #_(println)
  (let [from  (dec from)
        to    (dec to)
        donor (nth stacks from)
        n     (- (count donor) crates)
        block (subvec donor n)]
    (-> stacks
        (update from subvec 0 n)
        (update to   #(vec (concat % block))))))

(move-crates-9001 (puzzle :stacks) (-> puzzle :moves first))

(def part2-final-state
  (reduce move-crates-9001 (puzzle :stacks) (puzzle :moves)))

(with-out-str
  (print-stacks part2-final-state))

(->> part2-final-state
     (map peek)
     string/join)
