(defproject scm "0.1.0-SNAPSHOT"
  :description "75.14 Lenguajes Formales - FIUBA - 2do Cuatrimestre 2021"
  :url ""
  :license {:name "Axel Mauro Perez Machado, padron: 101127"
            :url ""}
  :dependencies [[org.clojure/clojure "1.10.3"]]
  :main ^:skip-aot scm.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
