(ns hiqa-reports-reader.reader.parsers
  (:require
   [clojure.string :as str]
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

(def respite-matchers
  [#"residential and respite|respite and residential"
   #"respite"])

(def about-section-preface "The following information has been submitted by the registered provider and describes the service they provide.")


;; Utils
(defn keywordize-reg-no [reg-no]
  (keyword
   (str  "Regulation_" reg-no "_"
         (when reg-no (str/replace (hiqa-regulations (parse-long reg-no)) " " "_")))))



;; Time and IDs

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

;; Parsers

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

(defn parse-frontmatter
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

;; Main Function:

(defn process-pdf
  "Main function that calls the other parsers. 'Observations' is optional, when passed as non-nil,
  inspector observations (large) section will be added."
  [pdf-file & observations]
  (let [text                     (:text (extract/parse pdf-file))
        frontmatter              (parse-frontmatter text)
        centre-id                (reformat-frontm-id (:centre-ID-OSV frontmatter))
        date                     (DOI->dateobj (:date-of-inspection frontmatter))
        report-id                (generate-report-id date centre-id)
        year                     (when date (jt/as date :year))
        residents-present        (number-of-residents-present text)
        compliance               (parse-compliance-table text)
        [about respite-category] (respite-based? text)]
    (merge
     frontmatter
     compliance
     {:report-id                   report-id
      :centre-id                   (parse-long centre-id)
      :number-of-residents-present residents-present
      :year                        year
      :date                        (when date (jt/format "YYYY-MM-dd" (jt/zoned-date-time date "UTC")))
      :about                       about
      :respite-category            respite-category}
     (when observations
       {:observations (what-residents-told-us text)}))))

