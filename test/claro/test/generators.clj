(ns claro.test.generators
  (:require [clojure.test.check.generators :as gen]
            [claro.data :as data]
            [claro.engine :as engine]))

;; ## Resolvables

(defrecord Person [id]
  data/Resolvable
  (resolve! [_ _]
    {:id         id
     :name       (str "Person #" id)
     :friend-ids (range id (+ id 10) 3)})
  data/Transform
  (transform [_ {:keys [friend-ids] :as person}]
    (assoc person :friends (map #(Person. %) friend-ids))))

(defrecord Env [k]
  data/Resolvable
  (resolve! [_ env]
    (get env k)))
