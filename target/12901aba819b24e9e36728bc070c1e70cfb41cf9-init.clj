nil (do (set! *warn-on-reflection* nil) nil (clojure.core/let [failures__7394__auto__ (clojure.core/atom 0)] (clojure.core/doseq [ns__7395__auto__ (quote (claro.data.ops.chain claro.data.ops.collections claro.data.ops.fmap claro.data.ops.maps claro.data.ops.then claro.data.ops claro.data.protocols claro.data.tree.blocking-composition claro.data.tree.collection claro.data.tree.composition claro.data.tree.leaf claro.data.tree.map-entry claro.data.tree.object claro.data.tree.utils claro.data.tree claro.data claro.engine.adapter claro.engine.core claro.engine.protocols claro.engine.resolver claro.engine claro.middleware.override claro.middleware.trace claro.projection.alias claro.projection.bind claro.projection.case claro.projection.conditional claro.projection.level claro.projection.maps claro.projection.maybe claro.projection.objects claro.projection.parameters claro.projection.protocols claro.projection.sequential claro.projection.sets claro.projection.transform claro.projection.union claro.projection.value claro.projection claro.runtime.application claro.runtime.caching claro.runtime.impl.core-async claro.runtime.impl.manifold claro.runtime.impl claro.runtime.inspection claro.runtime.mutation claro.runtime.resolution claro.runtime.selection claro.runtime))] (clojure.core/let [ns-file__7396__auto__ (clojure.core/-> (clojure.core/str ns__7395__auto__) (.replace \- \_) (.replace \. \/))] (clojure.core/binding [clojure.core/*out* clojure.core/*err*] (clojure.core/println "Compiling namespace" ns__7395__auto__)) (try (clojure.core/binding [clojure.core/*warn-on-reflection* true] (clojure.core/load ns-file__7396__auto__)) (catch java.lang.ExceptionInInitializerError e__7397__auto__ (clojure.core/swap! failures__7394__auto__ clojure.core/inc) (.printStackTrace e__7397__auto__))))) (clojure.core/if-not (clojure.core/zero? (clojure.core/deref failures__7394__auto__)) (java.lang.System/exit (clojure.core/deref failures__7394__auto__)))))