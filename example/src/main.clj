(ns main
  (:require [java-time.api :as t]
            [file-util]))

(defn- time-str
  [instant]
  (t/format
   (t/with-zone (t/formatter "yyyy-MM-dd HH:mm:ss") (t/zone-id))
   instant))

(defn- get-message
  []
  (first (file-util/lazy-load-resource "message.txt")))

(defn -main
  [& args]
  (println (str (time-str (t/instant)) ": " (get-message))))

