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
         :logs nil}))

(defmulti handle ::event)

(defmethod handle ::select-directory [{:keys [^ActionEvent fx/event]}]
  (let [window (.getWindow (.getScene ^Node (.getTarget event)))
        chooser (doto (DirectoryChooser.)
                  (.setTitle "Select folder containing report pdfs"))]
    (when-let [file (.showDialog chooser window)]
      {:state {:file file}})))

(defn count-files [file]
  (->> file
       io/file
       file-seq
       rest
       (filter #(re-find #".pdf" (str %)))
       count))

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
                                          :text "Open file..."
                                          :on-action {::event ::select-directory}}
                                         {:fx/type :label
                                          :text (str file)}
                                         {:fx/type :label
                                          :text (when file (str "Folder contains " (count-files file) " pdfs."))}]}
                             {:fx/type :button
                              :text "Run"
                              :on-action (fn [_]
                                           (let [{:keys [table info]} (reader/make-pdf-table (:file @*state))
                                                 {:keys [row-count table-info]} info]
                                             (do (reader/process-and-write-outputs! table table-info)
                                                 (swap! *state assoc :logs (str "Done. " row-count " files processed.")))))}
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
