(ns main
  (:require [file-util]
            [clojure.string :as string]
            [clojure.set :as sets]
            [clojure.math.numeric-tower :as math]))

(defn parse-scratcher
  [line]
  (let [card-end-index (string/index-of line ":")
        winners-end-index (string/index-of line "|")
        card-part (subs line 0 card-end-index)
        winners-part (subs line (+ card-end-index 2) (- winners-end-index 1))
        players-part (subs line (+ winners-end-index 2))]
    {:card-number (Integer/parseInt (second (re-matches #"Card +(\d+)" card-part)))
     :winning-numbers (set (re-seq #"\d+" winners-part))
     :players-numbers (set (re-seq #"\d+" players-part))}))

(defn parse-pile
  [input]
  (mapv parse-scratcher input))

(defn player-winning-numbers
  [card]
  (sets/intersection (:winning-numbers card) (:players-numbers card)))

(defn determine-player-winning-numbers
  [cards]
  (reduce
   (fn [acc cur] (conj acc (assoc cur :player-winning-numbers (player-winning-numbers cur))))
   []
   cards))

(defn card-points
  [card]
  (let [num (count (:player-winning-numbers card))]
    (if (> num 0)
      (math/expt 2 (- num 1))
      0)))

(defn card-copies
  [pile]
  (->> (loop [pile (reduce (fn [acc cur] (assoc acc (:card-number cur) cur)) {} pile)
              copies (reduce (fn [acc cur] (assoc acc (:card-number cur) 1)) {} (vals pile))
              cur-card-number 1]
         (if (> cur-card-number (count copies))
           copies
           (recur pile
                  (let [cur-copies (get copies cur-card-number 0)
                        num-winners (count (:player-winning-numbers (get pile cur-card-number {:player-winning-numbers #{}})))
                        copy-card-nums (range (+ cur-card-number 1) (+ cur-card-number num-winners 1))]
                    (reduce
                     (fn [acc cur] (assoc acc cur (+ (get copies cur 0) (* cur-copies 1))))
                     copies
                     copy-card-nums))
                  (+ cur-card-number 1))))
       vals
       (reduce + 0)))

;; Part 1:
;;  13
;;  32609
;; Part 2:
;;  30
;;  14624680
(defn -main
  [& args]
  (let [input (file-util/lazy-load-resource "input.txt")
        pile (->> input
                  parse-pile
                  determine-player-winning-numbers)]
    (println "Part 1:")
    (println (reduce + 0 (map card-points pile)))
    (println "Part 2:")
    (println (card-copies pile))))
   
