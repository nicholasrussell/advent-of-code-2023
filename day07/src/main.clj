(ns main
  (:require [file-util]
            [clojure.string :as string]
            [clojure.math.numeric-tower :as math]))

(def card-values
  (->> (range 2 10)
       (reverse)
       (map str)
       (map keyword)
       (concat [:A :K :Q :J :T])
       (reverse)
       (map-indexed (fn [idx k] [k (+ idx 1)]))
       (into {})))

(def card-values-2
  (->> (range 2 10)
       (reverse)
       (map str)
       (map keyword)
       (concat [:A :K :Q :T])
       (reverse)
       (concat [:J])
       (map-indexed (fn [idx k] [k (+ idx 1)]))
       (into {})))

(def hand-values
  {:five-of-a-kind 6
   :four-of-a-kind 5
   :full-house 4
   :three-of-a-kind 3
   :two-pair 2
   :pair 1
   :high-card 0})

(defn parse-hands
  [lines]
  (->> lines
       (map
        (fn [line]
          (let [matches (re-matches #"(.{5})\s+(\d+)" line)
                hand (second matches)
                bid (Integer/parseInt (nth matches 2))]
            {:hand hand
             :bid bid})))))

(defn count-cards
  [js-wild? hand]
  (let [raw (reduce (fn [acc cur] (assoc acc (keyword (str cur)) (count (filter #(= % cur) hand)))) {} hand)
        wilds (if js-wild? (or (second (first (filter (fn [[k v]] (= k :J)) raw))) 0) 0)
        sans-wilds (if js-wild? (into {} (filter (fn [[k v]] (not= k :J)) raw)) raw)
        max-count (apply max (or (vals sans-wilds) [0]))
        max-count-high-card (let [cards (map first (filter (fn [[k v]] (= max-count v)) sans-wilds))]
                              (reduce
                               (fn [acc cur]
                                 (if (> (get card-values cur) (get card-values acc))
                                   cur
                                   acc))
                               (first cards)
                               (rest cards)))
        sans-wilds (if max-count-high-card
                     (update-in sans-wilds [max-count-high-card] + wilds)
                     sans-wilds)]
    {:wilds wilds
     :cards (if (= wilds 5)
              {5 [:J]}
              (reduce-kv
               (fn [m k v]
                 (if-let [existing (get m v)]
                   (assoc m v (conj existing k))
                   (assoc m v [k])))
               {}
               sans-wilds))
     :raw raw}))

(defn determine-hand
  [card-values js-wild? hand]
  (let [{counts :cards wilds :wilds raw-counts :raw} (count-cards js-wild? hand)]
    (cond
      (get counts 5)
      {:type :five-of-a-kind}
             
      (get counts 4)
      {:type :four-of-a-kind} 
             
      (and (get counts 3) (get counts 2))
      {:type :full-house}

      (get counts 3)
      {:type :three-of-a-kind}

      (= (count (get counts 2)) 2)
      {:type :two-pair}

      (get counts 2)
      {:type :pair}
       
      :else
      {:type :high-card})))
   
(defn determine-card-value
  [card-values hand]
  (->> hand
       (reverse)
       (map-indexed #(.toString (biginteger (* (math/expt 16 %1) (get card-values (keyword (str %2))))) 16))
       (reverse)
       (reduce (fn [acc cur] (+ acc (BigInteger. cur 16))) 0)))

(defn assign-values
  [card-values js-wild? item]
  (let [hand-value (determine-hand card-values js-wild? (:hand item))
        card-value (determine-card-value card-values (:hand item))]
    (assoc item
           :hand-value (get hand-values (:type hand-value))
           :card-value card-value)))

(defn solve
  [input card-values js-wild?]
  (let [parsed (parse-hands input)
        hands (map (partial assign-values card-values js-wild?) parsed)
        ranked (sort-by (juxt :hand-value :tie-breaker :card-value) hands)]
    (->> ranked
         (map-indexed #(* (inc %1) (:bid %2)))
         (reduce + 0))))

;; Part 1:
;;  6440
;;  246424613
;; Part 2:
;;  5905
;;  248256639
(defn -main
  [& args]
  (let [input (file-util/lazy-load-resource "input.txt")]
    (println "Part 1:")
    (println (solve input card-values false))
    (println "Part 2:")
    (println (solve input card-values-2 true))))

