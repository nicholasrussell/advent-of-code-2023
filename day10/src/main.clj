(ns main
  (:require [file-util]
            [clojure.string :as string]
            [clojure.math.numeric-tower :as math]))

(defn neighbors-to-check
  [pipe max-x max-y]
  (let [coords (:coords pipe)]
    (filterv
     identity
     (map
       (fn [opening]
         (cond
           (and (= opening :n) (> (:y coords) 0)) {:opening :n :x (:x coords) :y (dec (:y coords))}
           (and (= opening :e) (< (:x coords) (dec max-x))) {:opening :e :x (inc (:x coords)) :y (:y coords)}
           (and (= opening :s) (< (:y coords) (dec max-y))) {:opening :s :x (:x coords) :y (inc (:y coords))}
           (and (= opening :w) (> (:x coords) 0)) {:opening :w :x (dec (:x coords)) :y (:y coords)}
           :else nil))
       (:openings pipe)))))

(defn parse-pipe
  [char x y max-x max-y]
  (let [openings (condp = char
                   \| {:openings [:n :s]}
                   \- {:openings [:e :w]}
                   \L {:openings [:n :e]}
                   \J {:openings [:n :w]}
                   \7 {:openings [:s :w]}
                   \F {:openings [:s :e]}
                   \S {:start true :openings [:n :e :s :w]}
                   \. {:openings nil})
        base-pipe (assoc openings :id (+ x (* max-x y)) :coords {:x x :y y})]
    (assoc base-pipe :neighbors-to-check (neighbors-to-check base-pipe max-x max-y))))

(defn parse-input
  [lines]
  (let [max-y (count lines)
        max-x (count (first lines))]
    (->> lines
         (map-indexed
          (fn [y row]
            (map-indexed
             (fn [x pipe]
               (parse-pipe pipe x y max-x max-y))
             row)))
         (mapv (partial into [])))))

(defn get-pipe-by-coord
  [pipes coord]
  (nth (nth pipes (:y coord)) (:x coord)))

(defn matching-opening
  [opening]
  (condp = opening
    :n :s
    :e :w
    :s :n
    :w :e))

(defn pipe-connections
  [pipe1-opening pipe2]
  (let [pipe2-openings (:openings pipe2)]
    (->> pipe2-openings
         (reduce
          (fn [conns pipe2-opening]
            (conj conns
                  (if (= pipe1-opening (matching-opening pipe2-opening))
                    {:opening pipe1-opening :connection (:id pipe2)}
                    nil)))
          [])
         (filterv some?))))

(defn map-connections
  [pipes]
  (reduce
   (fn [acc row]
     (conj acc
           (reduce
            (fn [row-acc pipe]
              (let [neighbors (:neighbors-to-check pipe)]
                (conj row-acc
                      (assoc (dissoc pipe :neighbors-to-check)
                             :connections (into [] (flatten (filterv seq (map #(pipe-connections (:opening %) (get-pipe-by-coord pipes %)) neighbors))))))))
            []
            row)))
   []
   pipes))

(defn build-map
  [pipes]
  (into
   (sorted-map)
   (reduce
    (fn [map row]
      (reduce
       (fn [map pipe]
         (assoc map (:id pipe) pipe))
       map
       row))
    {}
    pipes)))

(defn loop-dist
  [loop-start pipes]
  (defn loop-dist-helper
    [queue dist-map]
    (if (empty? queue)
      dist-map
      (let [head (first queue)
            pipe-id (:id head)
            q-dist (:dist head)]
        (if-let [dist (get dist-map pipe-id)]
          (recur (rest queue) (assoc dist-map pipe-id (min q-dist dist)))
          (let [pipe (get pipes pipe-id)]
            (recur (concat (rest queue)
                           (->> (:connections pipe)
                                (mapv :connection)
                                (reduce (fn [acc cur] (conj acc {:id cur :dist (inc q-dist)})) [])))
                   (assoc dist-map pipe-id q-dist)))))))
  (loop-dist-helper
   (->> (get pipes loop-start)
        :connections
        (mapv :connection)
        (reduce (fn [acc cur] (conj acc {:id cur :dist 1})) []))
   {loop-start 0}))

;; Part 1:
;;  8
;;  6800
;; Part 2:
;;  
;;  
(defn -main
  [& args]
  (let [input (file-util/lazy-load-resource "input.txt")
        parsed (parse-input input)
        pipe-map (build-map (map-connections parsed))
        start-pipe (reduce-kv (fn [m k v] (when (:start v) (reduced k))) nil pipe-map)]
    (println "Part 1:")
    (let [dist-map (loop-dist start-pipe pipe-map)
          max-dist (apply max (vals dist-map))]
      (println max-dist))))
