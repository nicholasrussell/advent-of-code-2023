(ns main
  (:require [file-util]
            [clojure.string :as string]
            [clojure.math.numeric-tower :as math]))

(defn parse-input
  [input]
  (let [instruction (first input)
        nodes (reduce
               (fn [acc cur]
                 (let [matches (re-matches #"(.{3}) = \((.{3}),\s+(.{3})\)" cur)]
                   (conj
                    acc
                    {:node-id (second matches)
                     :children (into [] (rest (rest matches)))})))
               []
               (rest (rest input)))]
    {:instruction (cycle instruction)
     :start-node "AAA"
     :nodes (reduce
             (fn [acc cur]
               (assoc acc (:node-id cur) cur))
             {}
             nodes)}))

(defn lr
  [instr]
  (if (= \L instr) first second))

(defn solve
  [input target-node-id]
  (let [nodes (:nodes input)]
    (loop [instr (:instruction input)
           current-node-id (:start-node input)
           current-node (get nodes (:start-node input))
           steps 0N]
      (if (= current-node-id target-node-id)
        steps
        (let [next-node ((lr (first (take 1 instr))) (:children current-node))]
          (recur (drop 1 instr) next-node (get nodes next-node) (inc steps)))))))

;; Part 1:
;;  2
;;  6
;;  14681
;; Part 2:
;;  
;;  
(defn -main
  [& args]
  (let [input (file-util/lazy-load-resource "input.txt")
        parsed (parse-input input)]
    (println "Part 1:")
    (println (solve parsed "ZZZ"))))

