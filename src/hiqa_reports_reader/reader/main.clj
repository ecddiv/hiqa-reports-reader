(ns hiqa-reports-reader.reader.main
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

(def hiqa-regulations
  {:capacity-and-capability
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
   :quality-and-safety
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
    29 "Medicines and pharmaceutical services"}})

(defn- DOI->dateobj
  "In cases where there are two inspection days, chooses second one."
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

(defn- parse-frontmatter [pdf-text]
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
  [pdf-text]
  (-> pdf-text
      (str/split #"What residents told us and what inspectors observed \n|Views of people who use the service|Capacity and capability \n")
      second
      (str/replace #"\n" "")
      str/trim
      (str/replace #"  Page \d+ of \d+  " "")))


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
                        reg-label (str "Regulation_" reg-no)
                        full-judgement (case judgement
                                         "Substantially" "Substantially compliant"
                                         "Not"           "Not compliant"
                                         "Compliant"     "Compliant"
                                         :error)]]
              [reg-label full-judgement])))))

(defn- process-pdf [pdf-file]
  (let [text                     (:text (extract/parse pdf-file))
        frontmatter              (parse-frontmatter text)
        centre-id                (reformat-frontm-id (:centre-ID-OSV frontmatter))
        date                     (DOI->dateobj (:date-of-inspection frontmatter))
        report-id                (generate-report-id date centre-id)
        year                     (when date (jt/as date :year))
        residents-present        (number-of-residents-present text)
        compliance               (parse-compliance-table text)
        observations             (what-residents-told-us text)]
    (merge
     frontmatter
     compliance
     {:report-id                   report-id
      :centre-id                   (parse-long centre-id)
      :number-of-residents-present residents-present
      :observations                observations
      :year                        year
      :date                        (when date (jt/format "YYYY-MM-dd" (jt/zoned-date-time date "UTC")))})))


(defn- full-csv-write! [DS output-file]
  (-> (tc/dataset DS)
      (tc/reorder-columns [:centre-id
                           :centre-ID-OSV
                           :year
                           :date-of-inspection
                           :date
                           :name-of-designated-centre
                           :address-of-centre
                           :name-of-provider
                           :type-of-inspection
                           :number-of-residents-present
                           :report-id])
      (tc/drop-missing :centre-id)
      (tc/write! output-file)))

(defn- list-and-check-files [dir]
  (if (.exists (io/file dir))
    (filter #(re-find #".pdf" (str %))
            (rest (file-seq (io/file dir))))
    (println "Not a Directory...")))

(defn- csv-timestamp []
  (-> (str/split (str (jt/instant)) #"\.")
      first))

(defn process-and-write-pdfs!
  "Entry point. Takes a directory of pdfs
  and processes them into a table,
  which is then written to 'output-file'"
  [pdf-directory]
  (let [reports (list-and-check-files pdf-directory)
        table (pmap process-pdf reports)
        output-file (str (csv-timestamp) "_hiqa_reports.csv")]
    (do
      (full-csv-write! table output-file)
      (println "File written to " output-file))))
