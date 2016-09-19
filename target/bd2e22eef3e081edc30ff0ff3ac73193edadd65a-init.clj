nil (do (set! *warn-on-reflection* nil) (clojure.core/require (quote codox.main)) (codox.main/generate-docs (quote {:description "claro que sí", :package claro/claro, :source-uri "https://github.com/xsc/claro/blob/master/{filepath}#L{line}", :namespaces [claro.data claro.data.ops claro.engine claro.engine.adapter #"^claro\.middlewares\..*" claro.projection], :output-path "/git/github/claro/target/doc", :name "claro", :source-paths ["src"], :themes {:highlight :atelier-sulphurpool-light}, :project {:name "claro"}, :root-path "/git/github/claro", :version "0.2.0-SNAPSHOT", :metadata {:doc/format :markdown}})))