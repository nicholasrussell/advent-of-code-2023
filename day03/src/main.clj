(ns main
  (:require [file-util]
            [clojure.string :as string]))

(defn possible-part-numbers
  [y row]
  (let [matcher (re-matcher #"\d+" row)]
    (into
     []
     ((fn step []
        (when (. matcher (find))
          (cons
           {:x (. matcher start) ; inclusive
            :x-end (- (. matcher end) 1) ; inclusive
            :y y
            :number (Integer/parseInt (re-groups matcher))}
           (lazy-seq (step)))))))))

(defn gears
  [y row]
  (let [matcher (re-matcher #"\*" row)]
    (into
     []
     ((fn step []
        (when (. matcher (find))
          (cons
           {:x (. matcher start) ; inclusive
            :y y}
           (lazy-seq (step)))))))))

(defn is-digit?
  [c]
  (<= (int \0) (int c) (int \9)))

(defn is-part-symbol?
  [c]
  (and (not= c \.) (not (is-digit? c))))

(defn make-coord
  [x y]
  {:x x :y y})

(defn adjacent-coords
  [max-x max-y init-x-start init-x-end y]
  (let [x-start (if (> init-x-start 0) (- init-x-start 1) init-x-start)
        x-end (if (< init-x-end max-x) (+ init-x-end 1) init-x-end)
        x-len (- (+ x-end 1) x-start)
        y-start (if (> y 0) (- y 1) y)
        y-end (if (< y max-y) (+ y 1) y)
        top (when (>= y-start 0) (map make-coord (range x-start (+ x-end 1)) (repeat x-len y-start)))
        middle (filter some? [(when (> x-start 0) (make-coord x-start (+ y-start 1))) (when (< x-end max-x) (make-coord x-end (+ y-start 1)))])
        bottom (when (<= y-end max-y) (map make-coord (range x-start (+ x-end 1)) (repeat x-len y-end)))]
    (into [] (concat top middle bottom))))

(defn valid-part-number?
  [max-x max-y board part]
  (let [{part-x :x
         part-x-end :x-end
         part-y :y} part]
    (->> (adjacent-coords max-x max-y part-x part-x-end part-y)
         (map (fn [coord] (nth (nth board (:y coord)) (:x coord))))
         (some is-part-symbol?))))

(defn part-numbers
  [board possible-part-numbers]
  (let [max-x (- (count (first board)) 1) ;; assume all rows same length
        max-y (- (count board) 1)]
    (filter (partial valid-part-number? max-x max-y board) possible-part-numbers)))

(defn find-valid-gear-parts
  [board parts max-x max-y gear]
  (let [{gear-x :x gear-y :y} gear
        adjacent-coords (adjacent-coords max-x max-y gear-x gear-x gear-y)
        adjacent-parts (->> parts
                            (filter (fn [part]
                                      (some (fn [coord]
                                              (and (<= (:x part) (:x coord) (:x-end part))
                                                   (= (:y part) (:y coord))))
                                            adjacent-coords))))]
    (when (= (count adjacent-parts) 2)
      {:gear gear
       :parts adjacent-parts})))

(defn gear-ratios
  [board parts gears]
  (let [max-x (- (count (first board)) 1) ;; assume all rows same length
        max-y (- (count board) 1)]
    (->> gears
         (map (partial find-valid-gear-parts board parts max-x max-y))
         (filter some?))))

;; Part 1:
;;  4361
;;  540131
;; Part 2:
;;  467835
;;  86879020
(defn -main
  [& args]
  (let [input (file-util/lazy-load-resource "input.txt")
        board (->> input
                   (map sequence)
                   (mapv (partial into [])))
        possible-part-numbers (->> input
                                   (map-indexed possible-part-numbers)
                                   flatten
                                   (into []))
        part-numbers (part-numbers board possible-part-numbers)
        sum (reduce + 0 (map :number part-numbers))
        gears (->> input
                   (map-indexed gears)
                   flatten
                   (into []))
        gear-ratios (gear-ratios board part-numbers gears)
        sum-gear-ratios (->> gear-ratios
                             (map :parts)
                             (map (partial map :number))
                             (map (partial reduce * 1))
                             (reduce + 0))]
    (println "Part 1:")
    (println sum)
    (println "Part 2:")
    (println sum-gear-ratios)))
