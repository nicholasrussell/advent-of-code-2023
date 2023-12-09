(ns main
  (:require [file-util]
            [clojure.string :as string]
            [clojure.math.numeric-tower :as math]))

(defn parse-input
  [lines]
  (->> lines
       (map (partial re-seq #"\-?\d+"))
       (mapv (fn [line] (mapv #(Integer/parseInt %) line)))))

(defn apply-diffs
  [sequence]
  (if (<= (count sequence) 1)
    [0]
    (loop [rem-seq (rest (rest sequence))
           last (second sequence)
           result [(- (second sequence) (first sequence))]]
      (if (empty? rem-seq)
        result
        (recur (rest rem-seq) (first rem-seq) (conj result (- (first rem-seq) last)))))))

(defn expand-seq
  [sequence]
  (loop [sequences [sequence]
         current-seq sequence]
    (if (every? #(= % 0) current-seq)
      sequences
      (let [new-seq (apply-diffs current-seq)]
        (recur (concat [new-seq] sequences) new-seq)))))

(defn solve
  [seqs]
  (->> seqs
       (map expand-seq)
       (map (fn [seq] (reduce + 0 (map last seq))))
       (reduce + 0)))

(defn solve2
  [seqs]
  (solve (map reverse seqs)))

;; Part 1:
;;  114
;;  1731106378
;; Part 2:
;;  [2] = 5
;;  1087
(defn -main
  [& args]
  (let [input (file-util/lazy-load-resource "input.txt")
        parsed (parse-input input)]
    (println "Part 1:")
    (println (solve parsed))
    (println "Part 2:")
    (println (solve2 parsed))))

