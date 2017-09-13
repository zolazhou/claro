(ns claro.projection.maybe
  (:require [claro.projection.protocols :as pr]
            [claro.data.ops.then :refer [then]]))

;; ## Maybe

(defrecord MaybeProjection [template]
  pr/Projection
  (project [_ value]
    (then value
          #(some->> % (pr/project template)))))

(defn maybe
  "Apply projection template if the value is not `nil`, otherwise just keep the
   `nil`.

   ```clojure
   (projection/maybe {:name projection/leaf})
   ```
   "
  [template]
  (->MaybeProjection template))

(defn ^:no-doc maybe?
  [v]
  (instance? MaybeProjection v))

(defmethod print-method MaybeProjection
  [^MaybeProjection value ^java.io.Writer w]
  (.write w "#<claro/maybe ")
  (print-method (.-template value) w)
  (.write w ">"))

;; ## Default

(defrecord DefaultProjection [template default-value]
  pr/Projection
  (project [_ value]
    (then value
          #(->> (if (nil? %)
                  default-value
                  %)
                (pr/project template)))))

(defn default
  "Apply the given projection to any non-nil value or the given default.

   ```clojure
   (projection/default {:name projection/leaf} unknown-person)
   ```
   "
  [template default-value]
  (->DefaultProjection template default-value))

(defmethod print-method DefaultProjection
  [^DefaultProjection value ^java.io.Writer w]
  (.write w "#<claro/default ")
  (print-method (.-template value) w)
  (.write w " | ")
  (print-method (.-default-value value) w)
  (.write w ">"))

(defn ^:no-doc default?
  [v]
  (instance? DefaultProjection v))
