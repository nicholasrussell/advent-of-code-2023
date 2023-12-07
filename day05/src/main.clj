(ns main
  (:require [file-util]
            [clojure.string :as string]))
;; Parsing
(defn parse-map-ranges
  [line]
  (let [matches (map #(bigint %) (re-seq #"\d+" line))
        range (nth matches 2)]
    {:src-start (nth matches 1)
     :src-end (- (+ (nth matches 1) range) 1)
     :dest-start (nth matches 0)
     :dest-end (- (+ (nth matches 0) range) 1)
     :diff (- (nth matches 0) (nth matches 1))
     :range (nth matches 2)}))

(defn parse-map-title
  [line]
  (let [matches (re-matches #"([a-z]+)\-to\-([a-z]+) map:" line)]
    {:map-src (nth matches 1)
     :map-dest (nth matches 2)}))

(defn parse-seeds
  [line]
  (mapv #(bigint %) (re-seq #"\d+" line)))

(defn parse-maps
  [lines]
  (let [parsed (reduce
                (fn [acc cur]
                  (cond
                    (re-find #"^\d" cur)
                    (update-in acc [:cur :ranges] conj (parse-map-ranges cur))

                    (re-find #"^[a-z]" cur)
                    (update-in acc [:cur] merge (parse-map-title cur))
       
                    :else
                    (assoc
                     (update-in acc [:result] conj (:cur acc))
                     :cur {:ranges []})))
                {:result [] :cur {:ranges []}}
                lines)]
    (map
     (fn [m] (assoc m :ranges (sort-by :src-start (:ranges m))))
     (if (seq (:cur parsed))
         (conj (:result parsed) (:cur parsed))
         (:result parsed)))))

(defn parse-input
  [lines]
  (let [seeds (parse-seeds (first lines))
        maps (parse-maps (rest (rest lines)))]
    {:seeds seeds
     :maps maps}))

;; Part 1
(defn determine-map-output
  [ranges input]
  (reduce
   (fn [acc cur]
     (if (<= (:src-start cur) input (:src-end cur))
       (reduced (+ input (:diff cur)))
       acc))
   input
   ranges))

(def determine-map-output-mem (memoize determine-map-output))

(defn make-path-fn
  [map-entry]
  {:src (:map-src map-entry)
   :f (memoize
       (fn [input]
         {:dest (:map-dest map-entry)
          :output (determine-map-output-mem (:ranges map-entry) input)}))})

(defn make-path-fns
  [maps]
  (assoc
   (reduce
    (fn [acc cur]
      (let [path-fn (make-path-fn cur)]
        (assoc acc (:src path-fn) (:f path-fn))))
    {}
    maps)
   "location"
   (fn [input] {:dest "location" :output input})))

(defn exec-path
  [input]
  (let [path-fn (get (:paths input) (:src input))
        result (path-fn (:num input))]
      (assoc input :src (:dest result) :num (:output result))))

(defn solve
  [seeds paths]
  (let [results (doall
                   (pmap
                    (fn [seed]
                      (first
                       (take
                        1
                        (drop-while
                         (fn [it] (not= (:src it) "location"))
                         (iterate exec-path {:paths paths :src "seed" :num seed})))))
                    seeds))]
    (apply min (pmap :num results))))

;; Part 2
(defn apply-ranges
  [input-ranges map-ranges]
  (loop [input-ranges input-ranges
         map-ranges map-ranges
         result []]
    (cond
      (or (empty? input-ranges) (empty? map-ranges))
      (filter (fn [m] (> (:start m) 0)) (sort-by :start (concat result input-ranges)))

      ; above range already, continue
      (> (:start (first input-ranges)) (:end (first map-ranges)))
      (recur input-ranges (rest map-ranges) result)

      ; below range, add as a possible min
      (< (:end (first input-ranges)) (:start (first map-ranges)))
      (recur (rest input-ranges) map-ranges (conj result (first input-ranges)))

      ; cross lower boundary
      (<= (:start (first input-ranges)) (:start (first map-ranges)) (:end (first input-ranges)) (:end (first map-ranges)))
      (recur (rest input-ranges)
             map-ranges
             (conj
              (conj result {:start (:start (first input-ranges))
                            :end (- (:start (first map-ranges)) 1)})
              {:start (+ (:start (first map-ranges)) (:diff (first map-ranges)))
               :end (+ (:end (first input-ranges) (:diff (first map-ranges))))}))

      ; cross upper boundary
      (<= (:start (first map-ranges)) (:start (first input-ranges)) (:end (first map-ranges)) (:end (first input-ranges)))
      (recur (conj (rest input-ranges) {:start (+ (:end (first map-ranges))) :end (:end (first input-ranges))})
             (rest map-ranges)
             (conj result {:start (+ (:start (first input-ranges)) (:diff (first map-ranges)))
                           :end (+ (:end (first input-ranges)) (:diff (first map-ranges)))}))

      ; in range, apply diff to xform range
      :else
      (recur (rest input-ranges)
             map-ranges
             (conj result {:start (+ (:start (first input-ranges)) (:diff (first map-ranges)))
                           :end (+ (:end (first input-ranges)) (:diff (first map-ranges)))})))))

(defn solve2
  [seeds maps]
  ; treat seeds like another map range
  (->> (reduce apply-ranges seeds maps)
       first
       :start))

;; Part 1:
;;  35
;;  535088217
;; Part 2:
;;  46
;;  51399228
(defn -main
  [& args]
  (let [input (file-util/lazy-load-resource "input.txt")
        data (parse-input input)
        paths (make-path-fns (:maps data))]
    (println "Part 1:")
    (println (solve (:seeds data) paths))
    (println "Part 2:")
    (solve2
     (->> (:seeds data)
          (partition 2)
          (mapv (fn [p] {:start (first p) :end (- (+ (first p) (second p)) 1)}))
          (sort-by :start))
     (->> (:maps data)
          (map :ranges)
          (map (fn [m]
                 (map
                  (fn [r] {:start (:src-start r)
                           :end (:src-end r)
                           :diff (:diff r)})
                  m)))))))
