(ns hiqa-reports-reader.hiqa-reports-reader
  (:gen-class)
  (:require [hiqa-reports-reader.reader.main :refer [process-and-write-pdfs!]]))

(defn greet
  "Callable entry point to the application."
  [data]
  (println (str "Hello, " (or (:name data) "World") "!")))

(defn -main
  [pdf-directory]
  (greet {:name "Test"})
  (process-and-write-pdfs! pdf-directory)
  (shutdown-agents))
