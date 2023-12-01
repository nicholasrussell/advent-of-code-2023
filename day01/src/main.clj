(ns main
  (:require [file-util]
            [clojure.string :as string]))

(def num-str {"one" "1" "two" "2" "three" "3" "four" "4" "five" "5" "six" "6" "seven" "7" "eight" "8" "nine" "9"})

(defn parse-nums-1
  [line]
  (re-seq #"\d" line))

(defn parse-nums-2
  [line]
  (let [matchers (concat (keys num-str) (vals num-str))]
    (loop [sub line
           result []]
      (if (string/blank? sub)
        result
        (let [match (some #(when (string/starts-with? sub %) %) matchers)]
          (recur
           (subs sub 1)
           (if-not match
             result
             (conj result match))))))))

(defn parse-line
  [part line]
  (let [nums (mapv
              #(or (get num-str %) %)
              (if (= part 2)
                (parse-nums-2 line)
                (parse-nums-1 line)))
        first (first nums)
        last (last nums)]
    (Integer/parseInt (str first last))))

(defn solve
  [part lines]
  (reduce + 0 (map (partial parse-line part) lines)))

;; Part 1:
;;  142
;;  54605
;; Part 2:
;;  281
;;  55429
(defn -main
  [& args]
  (let [part-1-input (file-util/lazy-load-resource "input.txt")]
    (println "Part 1:")
    (println (solve 1 part-1-input)))
  (let [part-2-input (file-util/lazy-load-resource "input.txt")]
    (println "Part 2:")
    (println (solve 2 part-2-input))))
 
