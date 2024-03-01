(ns build
  (:refer-clojure :exclude [test])
  (:require [clojure.tools.build.api :as b]))

(def lib 'hiqa-reports-reader.hiqa-reports-reader)
(def version "0.1.0")
(def main 'hiqa-reports-reader.hiqa-reports-reader)
(def class-dir "target/classes")

(defn- uber-opts [opts]
  (assoc opts
         :lib lib :main main
         :uber-file (format "target/%s-%s.jar" lib version)
         :basis (b/create-basis {})
         :class-dir class-dir
         :src-dirs ["src"]
         :ns-compile [main]
         :java-opts ["-Dorg.apache.commons.logging.Log=org.apache.commons.logging.impl.NoOpLog"]))

(defn make "Build the uberjar." [opts]
  (b/delete {:path "target"})
  (let [opts (uber-opts opts)]
    (println "\nCopying source...")
    (b/copy-dir {:src-dirs ["resources" "src"] :target-dir class-dir})
    (println (str "\nCompiling " main "..."))
    (b/compile-clj opts)
    (println "\nBuilding JAR...")
    (b/uber opts))
  opts)
