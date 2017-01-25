(ns user)

(require '[claro.data :as data]
         '[claro.projection :as projection]
         '[claro.engine :as engine])

(defrecord P []
  data/Resolvable
  (resolve! [_ _]
    {:y 2}))

(defn go
  []
  (engine/run!!
    (projection/apply
      (->P)
      {(projection/alias :a :x) projection/leaf})))
