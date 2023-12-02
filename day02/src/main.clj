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

(defn solve-1
  [games constraint]
  (->> games
       (filter (fn [game] (every? true? (map (partial possible-game constraint) (:pulls game)))))
       (map :game)
       (reduce + 0)))

(defn fmap
  [f m]
  (into (empty m) (for [[k v] m] [k (f v)])))

(defn max-cubes
  [game]
  (->> game
       :pulls
       (into [] cat)
       (group-by key)
       (fmap (fn [row]
               (->> row
                    (map second)
                    (apply max))))))

(defn power-cubes
  [cube-set]
  (apply * (map #(% cube-set) [:red :green :blue])))

(defn solve-2
  [games]
  (->> games
       (map max-cubes)
       (map power-cubes)
       (reduce + 0)))

;; Part 1:
;;  8
;;  2679
;; Part 2:
;;  2286
;;  77607
(defn -main
  [& args]
  (let [input (file-util/lazy-load-resource "input.txt")
        games (map parse-game input)]
    (println "Part 1:")
    (println (solve-1 games {:red 12 :green 13 :blue 14}))
    (println "Part 2:")
    (println (solve-2 games))))
