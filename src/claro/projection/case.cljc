(ns claro.projection.case
  (:refer-clojure :exclude [case])
  (:require [claro.projection.protocols :as pr]
            [claro.data.protocols :as p]))

;; Helpers

(defn- throw-case-mismatch!
  [value class->template]
  (throw
    (IllegalArgumentException.
      (format
        (str "no match in 'case'/'case-resolvable' projection.%n"
             "value: %s%n"
             "cases: %s")
        (pr-str value)
        (pr-str (vec (keys class->template)))))))

(defn- assert-resolvable!
  [value class->template]
  (when-not (p/resolvable? value)
    (throw
      (IllegalArgumentException.
        (format
          (str
            "'case-resolvable' projection can only be applied to resolvable.%n"
            "value: %s%n"
            "cases: %s")
          (pr-str value)
          (pr-str (vec (keys class->template))))))))

;; ## Record

(defrecord CaseResolvableProjection [class->template]
  pr/Projection
  (project [_ value]
    (assert-resolvable! value class->template)
    (let [template (get class->template
                        (class value)
                        (:else class->template ::none))]
      (if (not= template ::none)
        (pr/project template value)
        (throw-case-mismatch! value class->template)))))

;; ## Constructor

(defn- collect-cases
  [pairs]
  (->> (for [[classes template] pairs
             class (if (sequential? classes)
                     classes
                     [classes])]
         [class template])
       (reduce
         (fn [result [^Class class template]]
           (when (contains? result class)
             (throw
               (IllegalArgumentException.
                 (str "duplicate in 'case' projection: " (.getName class)))))
           (assoc result class template)) {})))

(defn ^{:added "0.2.1"} case-resolvable
  "Dispatch on the class of a `Resolvable`, applying the corresponding template.

   ```clojure
   (-> (->Animals)
       (projection/apply
         [(projection/case-resolvable
            Dolphin {:name projection/leaf, :intelligence projection/leaf}
            Zebra   {:name projection/leaf, :number-of-stripes projection/leaf}
            :else   {:name projection/leaf})])
        (engine/run!!))
   ;; => [{:name \"Tiger\"}
   ;;     {:name \"Dolphin\", :intelligence 80}
   ;;     {:name \"Zebra\", :number-of-stripes 20}]
   ```

   By specifiying a vector of classes, e.g. `[Tiger Zebra]` you can apply the
   same projection to multiple kinds of resolvables."
  [class template & more]
  {:pre [(even? (count more))]}
  (->> (partition 2 more)
       (cons [class template])
       (collect-cases)
       (->CaseResolvableProjection)))
