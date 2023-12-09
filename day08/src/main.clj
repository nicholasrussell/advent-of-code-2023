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

;; Part 2
;; I ended up not needing the first half of floyd's since it turns out the cycles start and end with the a and znodes
(defn floyd-helper
  [nodes instr start-node-id start-node]
  (loop [instr instr
         current-node-id start-node-id
         current-node start-node
         mu 0N]
    (if (string/ends-with? current-node-id "Z")
      mu
      (let [lr (lr (first (take 1 instr)))
            next-node-id (lr (:children current-node))]
        (recur (drop 1 instr) next-node-id (get nodes next-node-id) (inc mu))))))

(defn solve2
  [input]
  (let [nodes (:nodes input)
        instr (:instruction input)
        start-node-ids (filter #(string/ends-with? % "A") (keys nodes))
        cycles (pmap
                (fn [start-node-id]
                  {:start-node-id start-node-id
                   :cycle-length (floyd-helper nodes instr start-node-id (get nodes start-node-id))})
                start-node-ids)
        cycle-lengths (map :cycle-length cycles)
        min-cycle (apply min cycle-lengths)
        divisor (reduce
                 (fn [acc cur]
                   (if (every? #(= (mod % cur) 0) cycle-lengths)
                     (reduced cur)
                     acc))
                 1
                 (reverse (range 1 (inc min-cycle))))]
    (reduce
     (fn [acc cur] (/ (* acc cur) divisor))
     (/ (* (first cycle-lengths) (second cycle-lengths)) divisor)
     (rest (rest cycle-lengths)))))                  

#_(defn solve2
    [input]
    (let [nodes (:nodes input)]
      (loop [instr (:instruction input)
             current-node-ids (filter #(string/ends-with? % "A") (keys nodes))
             current-nodes (mapv (partial get nodes) current-node-ids)
             steps 0N]
        (if (every? #(string/ends-with? % "Z") current-node-ids)
          steps
          (let [lr (lr (first (take 1 instr)))
                next-node-ids (map #(lr (:children %)) current-nodes)]
            (recur (drop 1 instr) next-node-ids (mapv (partial get nodes) next-node-ids) (inc steps)))))))

;; Part 1:
;;  2
;;  6
;;  14681
;; Part 2:
;;  6
;;  < 84314908087948949100671
(defn -main
  [& args]
  (let [input (file-util/lazy-load-resource "input.txt")
        parsed (parse-input input)]
    (println "Part 1:")
    (println (solve parsed "ZZZ"))
    (println "Part 2:")
    (println (solve2 parsed))))

