(ns hiqa-reports-reader.ui.main
  (:gen-class)
  (:require
   [cljfx.api :as fx]
   [clojure.java.io :as io]
   [hiqa-reports-reader.reader.main :as reader])
  (:import
   (javafx.event ActionEvent)
   (javafx.scene Node)
   (javafx.stage DirectoryChooser)))



(def *state
  (atom {:file nil
         :pdf-count nil
         :logs nil
         :runningp false}))

(defn count-pdfs [file]
  (->> file
       io/file
       file-seq
       rest
       (filter #(re-find #".pdf" (str %)))
       count))

(defn seconds->minutes [seconds]
  (if (< seconds 60)
    (str (int seconds) " seconds")
    (let [mins (int (Math/floor (/ seconds 60)))
          secs (int (mod seconds 60))]
      (str mins (if (= mins 1) " minute and " "minutes and ") secs " seconds"))))

;; Avg rate of running - 22s per 1000 pdfs
(defn estimate-completion-time []
  (let [pdf-count (:pdf-count @*state)]
    (if pdf-count
      (seconds->minutes
       (* 2.2 (/ pdf-count 100)))
      "0")))

(defmulti handle ::event)

(defmethod handle ::select-directory [{:keys [^ActionEvent fx/event]}]
  (let [window (.getWindow (.getScene ^Node (.getTarget event)))
        chooser (doto (DirectoryChooser.)
                  (.setTitle "Select folder containing report pdfs"))]
    (when (not (:runningp *state))
      (when-let [file (.showDialog chooser window)]
        {:state {:file file
                 :pdf-count (count-pdfs file)}}))))


(defn process-pdfs [_]
  (when (and (not (:runningp @*state))
             (:file @*state))
    (do (swap! *state assoc :logs (str "Running... \nEstimated completion time: " (estimate-completion-time)))
        (future
          (try
            (let [{:keys [table table-recents info]} (reader/make-pdf-table (:file @*state))
                  {:keys [row-count table-info]} info]
              (do (reader/process-and-write-outputs! table table-recents table-info)
                  (swap! *state assoc :logs (str "Done. " row-count " files processed."))))
            (catch Exception e
              (.printStackTrace e)
              (swap! *state assoc :logs "Failed.")))))))

(defn root-view [{:keys [file]}]
  {:fx/type :stage
   :title "HIQA Reports Converter"
   :showing true
   :width 800
   :height 600
   :scene {:fx/type :scene
           :root {:fx/type :v-box
                  :padding 30
                  :spacing 15
                  :children [{:fx/type :h-box
                              :spacing 15
                              :alignment :center-left
                              :children [{:fx/type :button
                                          :text "Select Folder..."
                                          :on-action {::event ::select-directory}}
                                         {:fx/type :label
                                          :text (str file)}
                                         {:fx/type :label
                                          :text (when file
                                                  (str "Folder contains " (:pdf-count @*state) " pdfs."))}]}
                             {:fx/type :button
                              :text "Run"
                              :on-action process-pdfs}
                             {:fx/type :label
                              :text (str (:logs @*state))}]}}})

(def renderer
  (fx/create-renderer
    :middleware (fx/wrap-map-desc #(root-view %))
    :opts {:fx.opt/map-event-handler
           (-> handle
               (fx/wrap-co-effects {:state (fx/make-deref-co-effect *state)})
               (fx/wrap-effects {:state (fx/make-reset-effect *state)
                                 :dispatch fx/dispatch-effect}))}))

(comment
  (fx/mount-renderer *state renderer))
