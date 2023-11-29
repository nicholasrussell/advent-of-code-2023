(ns file-util
  (:require [clojure.java.io :as io]))

(defn- lazy-load-resource-seq
  [reader]
  (lazy-seq
   (if-let [line (.readLine reader)]
     (cons line (lazy-load-resource-seq reader))
     (do
       (.close reader)
       nil))))

(defn lazy-load-resource
  ([resource-path]
   (lazy-load-resource resource-path lazy-load-resource-seq))
  ([resource-path reader-fn]
   (->> resource-path
        io/resource
        io/file
        io/reader
        reader-fn)))

