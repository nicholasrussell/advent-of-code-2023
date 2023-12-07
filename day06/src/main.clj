(ns main
  (:require [file-util]
            [clojure.string :as string]
            [clojure.math.numeric-tower :as math]))

(defn parse-data
  [lines]
  (->> lines
       (map (partial re-seq #"\d+"))
       (map (fn [row] (map bigint row)))
       (apply map vector)
       (mapv (fn [race] {:time (first race) :record-distance (second race)}))))

(defn quad-roots
  [a b c]
  (let [discriminant (- (math/expt b 2) (* 4 a c))]
    (if (or (= a 0) (< discriminant 0))
      nil
      (let [sqrt-discr (math/sqrt discriminant)
            inv-2a (/ 1 (* 2 a))
            t0 (* (- (- b) sqrt-discr) inv-2a)
            t1 (* (+ (- b) sqrt-discr) inv-2a)]
        [t0 t1]))))

(defn solve
  [races]
  (->> races
       (map
        (fn [race]
          (let [roots (quad-roots -1 (:time race) (- 0 (:record-distance race) 0.00001))]
            (int (- (math/floor (first roots)) (math/floor (second roots)))))))
       (reduce * 1)))

;; Part 1:
;;  288
;;  771628
;; Part 2:
;;  71503
;;  27363861
(defn -main
  [& args]
  (let [input (file-util/lazy-load-resource "input.txt")]
    (println "Part 1:")
    (println (solve (parse-data input)))
    (println "Part 2:")
    (println (solve (->> input (map (fn [line] (string/replace line #"\s" ""))) parse-data)))))
