(ns clavichord.method
  (:refer-clojure :exclude [name]))

(defn name
  [method]
  (.getName method))
