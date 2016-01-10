(ns claro.runtime
  (:require [claro.runtime
             [application :refer [apply-resolved-batches]]
             [impl :as impl]
             [inspection :refer [inspect-resolvables]]
             [selection :refer [select-resolvable-batches]]
             [resolution :refer [resolve-batches!]]])
  (:refer-clojure :exclude [run!]))

;; ## Depth Protection

(defn- assert-batch-count!
  [{:keys [max-batches] :or {max-batches 256}} batch-count]
  (when (some-> max-batches (< batch-count))
    (throw
      (IllegalStateException.
        (format "resolution has exceeded maximum batch count/depth: %d/%d"
                batch-count
                max-batches)))))

;; ## Runtime Logic

(defn- run-step!
  [{:keys [impl] :as opts} {:keys [value batch-count]}]
  (let [resolvables (inspect-resolvables opts value)]
    (if (empty? resolvables)
      value
      (let [batches (select-resolvable-batches opts resolvables)
            new-batch-count (+ batch-count (count batches))]
        (assert-batch-count! opts new-batch-count)
        (if (seq batches)
          (impl/chain
            impl
            batches
            #(resolve-batches! opts %)
            #(apply-resolved-batches opts value %)
            #(impl/recur impl {:value %, :batch-count new-batch-count}))
          value)))))

(defn run!
  "Run the resolution engine on the given value. `opts` is a map of:

   - `:inspect-fn`: a function that, given a value, returns a seq of all
      available resolvables within that value,
   - `:selector`: a function that, given a seq of resolvable classes returns
     those to resolve during the next step,
   - `:resolve-fn`: a function that given a seq of resolvables of the same class
     returns a manifold deferred with resolved values in-order,
   - `:deferred-fn`: a function called on each manifold deferred encapsuling a
     single resolution step (parameters are batch-to-be-resolved, as well as
     the deferred value),
   - `:apply-fn`: a function that takes the original value, as well as a map
     of resolvable -> resolved value pairs, and returns a map of `:value` and
     `:resolvables`, where `:value` is the now-more-resolved value for the next
     iteration and `:resolvables` the new resolvables within,
   - `:max-batches`: an integer describing the maximum number of batches to
     resolve before throwing an `IllegalStateException`.

   Returns a manifold deferred with the resolved result."
  [{:keys [impl] :as opts} value]
  {:pre [(every? fn? (map opts [:inspect-fn :resolve-fn :apply-fn]))]}
  (impl/loop
    impl
    #(run-step! opts %)
    {:value value, :batch-count 0}))