(ns hiqa-reports-reader.hiqa-reports-reader
  (:gen-class)
  (:require [hiqa-reports-reader.ui.main :as ui]
            [cljfx.api :as fx]))

(defn -main
  []
  (fx/mount-renderer ui/*state ui/renderer))
