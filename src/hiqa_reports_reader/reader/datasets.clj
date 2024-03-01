(ns hiqa-reports-reader.reader.datasets
  (:require
   [tablecloth.api :as tc]))

;; For creating tables/preparing datasets

(defn- reg-map-cols
  "Returns function for use with Tablecloth, for summarising compliance levels."
  [type]
  (fn [rows]
    (reduce #(if (= %2 type) (inc %1) %1) 0 rows)))

(defn aggregate-compliance-levels [DS]
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

(defn filter-and-sort-reg-cols [col-names]
  (sort
   (filter (fn [kw] (re-find #"Regulation" (name kw))) col-names)))

(defn prepare-main-table [DS]
  (let [dataset (-> (tc/dataset DS)
                    (tc/drop-missing :centre-id)
                    aggregate-compliance-levels)
        sorted-regs (-> dataset
                        tc/column-names
                        filter-and-sort-reg-cols)]
    (-> dataset
        (tc/reorder-columns (into [:centre-id
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
                                   :observations
                                   :num-compliant
                                   :num-notcompliant
                                   :num-substantiallycompliant]
                                  sorted-regs)))))

(defn filter-most-recent-inspections-per-centre [DS]
  (reduce (fn  [new-ds centre-row]
            (tc/concat new-ds centre-row))
          (-> DS
              (tc/group-by :centre-id)
              (tc/order-by :date :desc)
              (tc/select-rows 0)
              :data)))
