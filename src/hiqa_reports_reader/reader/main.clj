(ns hiqa-reports-reader.reader.main
  (:gen-class)
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [tablecloth.api :as tc]
   [java-time.api :as jt]
   [pantomime.extract :as extract]))

(def frontmatter-fields ["Name of designated centre:"
                         "Name of provider:"
                         "Address of centre:"
                         "Type of inspection:"
                         "Date of inspection:"
                         "Centre ID:"
                         "Fieldwork ID:"])

(def frontmatter-fields-keywords [:name-of-designated-centre
                                  :name-of-provider
                                  :address-of-centre
                                  :type-of-inspection
                                  :date-of-inspection
                                  :centre-ID-OSV
                                  :fieldwork-ID])

(def temp-sample-pdfs (take 1000 (drop 1000 (rest (file-seq (clojure.java.io/file "inspection_reports"))))))

(def hiqa-regulations
  (merge
   ;; Capacity and Capability
   {3 "Statement of purpose"
    4 "Written policies and procedures"
    14 "Person in charge"
    15 "Staffing"
    16 "Training and staff development"
    19 "Directory of residents"
    21 "Records"
    22 "Insurance"
    23 "Governance and management"
    24 "Admissions and contract for the provision of services"
    30 "Volunteers"
    31 "Notification of incidents"
    32 "Notifications of periods when person in charge is absent"
    33 "Notifications of procedures and arrangements for periods when person in charge is absent"
    34 "Complaints procedure"}
   ;; Quality and Safety
   {5 "Individualised assessment and personal plan"
    6 "Healthcare"
    7 "Positive behaviour support"
    8 "Protection"
    9 "Residents' rights"
    10 "Communication"
    11 "Visits"
    12 "Personal possessions"
    13 "General welfare and development"
    17 "Premises"
    18 "Food and nutrition"
    20 "Information for residents"
    25 "Temporary absence, transition and discharge of residents"
    26 "Risk management procedures"
    27 "Protections against infection"
    28 "Fire precautions"
    29 "Medicines and pharmaceutical services"}))

(defn- DOI->dateobj
  "Converts description of 'date of inspection' (e.g., '3 October 2023')
  into date object.
  In cases where there are two inspection days, chooses second one."
  [date-of-inspection]
  (when date-of-inspection
    (let [dt-str
          (if (re-find #"and|&" date-of-inspection)
            (second (str/split date-of-inspection #"and |& "))
            date-of-inspection)]
      (when dt-str
        (jt/local-date "ddMMMMyyyy" (str/replace dt-str #" " ""))))))

(defn- reformat-frontm-id
  "Extract ID compatible with 'centre-id' from the longer IDs used in the reports"
  [osvID]
  (->> osvID
       reverse
       (take 4)
       reverse
       (apply str)))

(defn- generate-report-id
  "Generate custom report id, based on center ID joined with date of report yyyyMMdd.
  Date input is based on report frontmatter, example '01 January 1900'  "
  [date centre-id]
  (when date
    (str centre-id "-" (jt/format "yyyyMMdd" date))))

(defn- parse-frontmatter
  "Reads info from page 1 of report."
  [pdf-text]
  (let [page-1 (first (rest (str/split pdf-text #"Page")))
        info   (->> frontmatter-fields
                    (str/join "|")
                    re-pattern
                    (str/split (str/replace page-1 #"\n" ""))
                    reverse
                    (take (count frontmatter-fields))
                    reverse
                    (map str/trim))
        data (zipmap frontmatter-fields-keywords info)]
    (if (= (:name-of-provider data) "St John of God Community Services Company Limited By Guarantee")
      (assoc data :name-of-provider "St John of God Community Services CLG")
      data)))

(defn- number-of-residents-present [pdf-text]
  (let [[_ no-of-residents] (str/split pdf-text
                                       #"Number of residents on the \n\ndate of inspection:")]
    (when no-of-residents
      (parse-long (re-find #"\d+" no-of-residents)))))

(defn- what-residents-told-us
  "Reads the long 'Inspector observations' section of
  the report."
  [pdf-text]
  (-> pdf-text
      (str/split #"What residents told us and what inspectors observed \n|Views of people who use the service|Capacity and capability \n")
      second
      (str/replace #"\n" "")
      str/trim
      (str/replace #"  Page \d+ of \d+  " "")))

(defn keywordize-reg-no [reg-no]
  (keyword
   (str  "Regulation_" reg-no "_"
         (when reg-no (str/replace (hiqa-regulations (parse-long reg-no)) " " "_")))))
  

;; TODO Also track the 'registration' regulations - see this report:https://www.hiqa.ie/system/files?file=inspectionreports/3282-the-childrens-sunshine-home-operating-as-lauralynn-childrens-hospice-03-may-2023.pdf
(defn- parse-compliance-table [pdf-text]
  (let [match (or
               (re-find #"Regulation Title(.*)Compliance Plan"
                        (str/replace pdf-text #"\n" " "))
               (re-find #"Regulation Title(.*)" ;; For cases where compliance table is at end of report
                        (str/replace pdf-text #"\n" " ")))]
    (when match
      (into (sorted-map)
            (for [line (map str/trim (str/split (first match) #"Regulation"))

                  :when (int? (parse-long (str (first line))))

                  :let [[_ reg-no _ judgement] (first (re-seq #"(\d{1,2}):(.*)(Substantially|Not|Compliant)" line))
                        reg-label (keywordize-reg-no reg-no)
                        full-judgement (case judgement
                                         "Substantially" "Substantially compliant"
                                         "Not"           "Not compliant"
                                         "Compliant"     "Compliant"
                                         :error)]]
              [reg-label full-judgement])))))

(def respite-matchers
  [#"residential and respite|respite and residential"
   #"respite"])

(def about-section-preface "The following information has been submitted by the registered provider and describes the service they provide.")

(defn- respite-based?
  "Also returns full 'about' text for context."
  [pdf-text]
  (let [[_ match _] (str/split pdf-text
                               #"About the designated centre|The following information outlines some additional data on this centre.")]
    (when match
      (let [match (str/trim (str/replace (str/replace match #"\n" "") (re-pattern about-section-preface) ""))
            res-and-respite (re-find (first respite-matchers) (str/lower-case match))
            respite (when (not res-and-respite) (re-find (second respite-matchers) (str/lower-case match)))]
        [match
         (if res-and-respite "Residential and Respite"
             (when respite "Respite"))]))))




(defn- process-pdf [pdf-file]
  (let [text                     (:text (extract/parse pdf-file))
        frontmatter              (parse-frontmatter text)
        centre-id                (reformat-frontm-id (:centre-ID-OSV frontmatter))
        date                     (DOI->dateobj (:date-of-inspection frontmatter))
        report-id                (generate-report-id date centre-id)
        year                     (when date (jt/as date :year))
        residents-present        (number-of-residents-present text)
        compliance               (parse-compliance-table text)
        observations             (what-residents-told-us text)
        [about respite-category] (respite-based? text)]
    (merge
     frontmatter
     compliance
     {:report-id                   report-id
      :centre-id                   (parse-long centre-id)
      :number-of-residents-present residents-present
      :observations                observations
      :year                        year
      :date                        (when date (jt/format "YYYY-MM-dd" (jt/zoned-date-time date "UTC")))
      :about                       about
      :respite-category            respite-category})))

(defn prepare-table [DS]
  (-> (tc/dataset DS)
      (tc/reorder-columns [:centre-id
                           :centre-ID-OSV
                           :fieldwork-ID
                           :year
                           :date-of-inspection
                           :date
                           :name-of-designated-centre
                           :address-of-centre
                           :name-of-provider
                           :type-of-inspection
                           :number-of-residents-present
                           :report-id
                           :about
                           :respite-category
                           :observations])
      (tc/drop-missing :centre-id)))

(def temp-ds
  (prepare-table
   (pmap process-pdf temp-sample-pdfs)))

(defn reg-map-cols
  "Returns function for use with Tablecloth, for summarising compliance levels."
  [type]
  (fn [rows]
    (reduce #(if (= %2 type) (inc %1) %1) 0 rows)))

(defn- aggregate-compliance-levels [DS]
  (-> DS
      (tc/map-columns :num-compliant
                      (tc/column-names DS #":Regulation.*")
                      (fn [& rows]
                        ((reg-map-cols "Compliant") rows)))
      (tc/map-columns :num-notcompliant
                      (tc/column-names DS #":Regulation.*")
                      (fn [& rows]
                        ((reg-map-cols "Not compliant") rows)))
      (tc/map-columns :num-substantiallycompliant
                      (tc/column-names DS #":Regulation.*")
                      (fn [& rows]
                        ((reg-map-cols "Substantially compliant") rows)))))



(defn filter-most-recent-inspections-per-centre [DS]
  (reduce (fn  [new-ds centre-row]
            (tc/concat new-ds centre-row))
          (-> DS
              (tc/group-by :centre-id)
              (tc/order-by :date :desc)
              (tc/select-rows 0)
              :data)))



(defn- list-and-check-files [dir]
  (if (.exists (io/file dir))
    (filter #(re-find #".pdf" (str %))
            (rest (file-seq (io/file dir))))
    (println "Not a Directory...")))

(defn- csv-timestamp []
  (-> (str/split (str (jt/instant)) #"\.")
      first
      (str/replace #":" "")))

(defn- full-csv-write! [DS info-file]
  (let [timestamp (csv-timestamp)
        main-out (str timestamp "_hiqa_reports.csv")
        info-out (str timestamp "_table_info.csv")]
    (tc/write! DS main-out)
    (tc/write! info-file info-out)))


(defn table-info [ds]
  {:table-info (tc/info ds)
   :row-count (tc/row-count ds)})

(defn make-pdf-table [pdf-directory]
  (let [reports (list-and-check-files pdf-directory)
        table (pmap process-pdf reports)
        table-ds (prepare-table table)]
    {:table table-ds
     :info (table-info (-> table-ds
                           (tc/drop-columns :observations)))}))

(defn process-and-write-outputs!
  "Entry point. Takes a directory of pdfs
  and processes them into a table,
  which is then written to 'output-file'"
  [table-ds info-ds]
  (do
    (full-csv-write! table-ds info-ds)
    (println "Done")))

(comment
  (let [{:keys [table info]} (make-pdf-table "reports")
        {:keys [row-count table-info]} info]
    (process-and-write-outputs! table table-info)
    (println (str "Done. " row-count " reports processed."))))


