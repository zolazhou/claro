nil (do (set! *warn-on-reflection* nil) (clojure.core/require [(quote perforate.core) :as (quote perf)]) (do (set! clojure.core/*warn-on-reflection* true) (clojure.core/when (clojure.core/seq (quote [claro.resolution-without-batching.claro claro.resolution-with-batching.claro claro.deep-projection])) (clojure.core/apply clojure.core/require :reload (quote [claro.resolution-without-batching.claro claro.resolution-with-batching.claro claro.deep-projection]))) (perf/run-benchmarks {} (quote [claro.resolution-without-batching.claro claro.resolution-with-batching.claro claro.deep-projection]))))