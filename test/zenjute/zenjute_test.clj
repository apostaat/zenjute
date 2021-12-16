(ns zenjute.zenjute-test
  (:require [clojure.test :refer :all]
            [zen.core :as zen]
            [sci.core :as sci]))

(remove-ns 'zenjute.zenjute-test)

(def paths {:paths ["test/zenjute/resources"]})
(def ctx (zen/new-context paths {:unsafe true}))

;; (zen/read-ns ctx 'mapping)
;; (zen/load-ns ctx 'mapping)

(zen/load-ns ctx {'ns 'mapping

 'NaiveMapping
 {:zen/tags #{'zen/naive-mapping}
  :zj/body "(fn [{:keys [value]}]
             {:identifier value
              :identifier_type
              (if-let [code (and value (-> value :type :coding first :code))]
                code
                (and value \"UPIN\"))})"}})

@ctx

(zen/get-symbol ctx 'mapping/NaiveMapping)
(zen/get-tag ctx 'mapping/naive-mapping)
;; => :zen/load-failed


(defmethod operation 'doc-xlsx
  [op]
  (fn [ctx]
    (-> ctx
      deref
      :ns
      (get 'data)
      vals
      (->> (mapv :docs)
           (remove nil?)
           first
           (reduce (fn [acc {:keys [title data]}]
                     (assoc acc title
                            (->> data (mapv create-single-cell) excel/table-grid))) {}))
      excel/quick-open!)))

(defn evaluation
  [ctx data-from-outside]
  (z))

(defn coerce-zen-schema
  [ctx]
  (let [op (-> ctx deref :ns (get 'docs) (get 'Documentation) operation)]
    (op ctx)))

(coerce-zen-schema ctx)

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))
