(ns hiqa-reports-reader.ui.main
  (:require [cljfx.api :as fx]
            [hiqa-reports-reader.reader.main :as reader])

  (:import [javafx.stage DirectoryChooser]
           [javafx.event ActionEvent]
           [javafx.scene Node]))

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
                                          :text (str file)}]}
                             {:fx/type :button
                              :text "Run"
                              :on-action (fn [_] (do (reader/process-and-write-pdfs! (:file @*state))
                                                     (swap! *state assoc :logs "Done.")))}
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
