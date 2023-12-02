(ns main
  (:require [file-util]
            [clojure.string :as string]))

(defn parse-pull
  [pull]
  (let [parse-color (fn [color pull]
                      (Integer/parseInt
                       (or
                        (second
                         (re-find (re-pattern (str "(\\d+) " color)) pull))
                        "0")))]
    {:red (parse-color "red" pull)
     :green (parse-color "green" pull)
     :blue (parse-color "blue" pull)}))

(defn parse-game
  [line]
  (let [game (let [match (re-find #"Game (\d+):" line)]
               (Integer/parseInt (second match)))
        raw-pulls (let [rest-line (subs line (+ 7 (count (str game))))]
                    (map string/trim (string/split rest-line #";")))]
    {:game game
     :pulls (mapv parse-pull raw-pulls)}))

(defn meets-constraint
  [constraint pull k]
  (<= (k pull) (k constraint)))

(defn possible-game
  [constraint pull]
  (every? true? (map (partial meets-constraint constraint pull) [:red :green :blue])))

(defn solve
  [lines constraint]
  (->> lines
       (map parse-game)
       (filter (fn [game] (every? true? (map (partial possible-game constraint) (:pulls game)))))
       (map :game)
       (reduce + 0)))

;; Part 1:
;;  8
;;  2679
;; Part 2:
;;  
;;  
(defn -main
  [& args]
  (let [part-1-input (file-util/lazy-load-resource "input.txt")]
    (println "Part 1:")
    (println (solve part-1-input {:red 12 :green 13 :blue 14}))))
