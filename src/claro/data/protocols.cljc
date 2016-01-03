(ns claro.data.protocols
  (:require [potemkin :refer [defprotocol+ definterface+]]))

;; ## Resolvables

(defprotocol+ Resolvable
  "Protocol for resolvable values."
  (resolve! [resolvable env]
    "Resolve the given value within the given environment, returning a manifold
     deferrable representing the resolution result. This might yield a structure
     containing more `Resolvable`s."))

(defprotocol+ BatchedResolvable
  "Protocol for resolution of multiple values at once."
  (resolve-batch! [resolvable env all-resolvables]
    "Resolve `all-resolvables` (which is a seq including `resolvable`), returning
     a manifold deferrable containing a seq with resolved values in-order."))

(defn resolvable?
  "Check whether the given value implements the `Resolvable` protocol."
  [value]
  (satisfies? Resolvable value))

;; ## Trees

(defprotocol+ ResolvableTree
  (unwrap-tree1 [tree]
    "Unwrap one level of the (potentially not fully-resolved) tree value.")
  (resolved? [tree]
    "Is the tree completely resolved?")
  (resolvables* [tree]
    "Return a seq of all Resolvables within the given tree.")
  (apply-resolved-values [tree resolvable->resolved]
    "Replace the `Resolvables` with the given resolved values, returning a
     potentially fully resolved `ResolvableTree`."))

(definterface+ WrappedTree)

(defn wrapped?
  [tree]
  (instance? WrappedTree tree))

(defn resolvables
  [tree]
  (into #{} (resolvables* tree)))
