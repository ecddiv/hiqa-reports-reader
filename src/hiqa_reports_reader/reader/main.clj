(ns hiqa-reports-reader.reader.main
  (:gen-class)
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [tablecloth.api :as tc]
   [java-time.api :as jt]
   [hiqa-reports-reader.reader.datasets :as ds]
   [hiqa-reports-reader.reader.parsers :as p]))


(defn- list-and-check-files [dir]
  (if (.exists (io/file dir))
    (filter #(re-find #".pdf" (str %))
            (rest (file-seq (io/file dir))))
    (println "Not a Directory...")))

(defn- csv-timestamp []
  (-> (str/split (str (jt/instant)) #"\.")
      first
      (str/replace #":" "")))

(defn- full-csv-write! [DS DS-most-recents info-file]
  (let [timestamp (csv-timestamp)
        main-out (str timestamp "_hiqa_reports.csv")
        most-recents-out (str timestamp "_most_recent_inspections_only.csv")
        info-out (str timestamp "_table_info.csv")]
    (tc/write! DS main-out)
    (tc/write! DS-most-recents most-recents-out)
    (tc/write! info-file info-out)))


(defn table-info [ds]
  {:table-info (tc/info ds)
   :row-count (tc/row-count ds)})

(defn make-pdf-table [pdf-directory]
  (let [reports           (list-and-check-files pdf-directory)
        table             (pmap p/process-pdf reports)
        table-ds          (ds/prepare-main-table table)
        table-most-recent (ds/filter-most-recent-inspections-per-centre table-ds)]
    {:table         table-ds
     :table-recents table-most-recent
     :info          (table-info (-> table-ds
                                    (tc/drop-columns :observations)))}))


(defn process-and-write-outputs!
  "Entry point."
  [table-ds table-recents-ds info-ds]
  (do
    (full-csv-write! table-ds table-recents-ds info-ds)
    (println "Done")))

(comment
  (let [{:keys [table info table-recents]} (make-pdf-table "reports")
        {:keys [row-count table-info]} info]
    (process-and-write-outputs! table table-recents table-info)
    (println (str "Done. " row-count " reports processed."))))



