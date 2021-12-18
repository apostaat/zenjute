(ns zenjute.operation
  (:require [cheshire.core :as json]
            [zenjute.zenjute :as zj]
            [zen.core :as zen]
            [clojure.java.io :as io]
            [clojure.edn   :as edn]
            [clojure.string :as str]))

(defn ->string [body]
  (cond-> body (not (string? body)) slurp))

(def a (atom {}))

(defn read-custom [body]
  (let [[code data] (str/split body #"\,\$\$\$\,")]
  {:code code
   :data data}))

(defn read-body [{:keys [body] :as req}]
  (let [cnt (->string body)]
    (if (#{"application/json"} (get-in req [:headers "content-type"]))
        (json/parse-string cnt true)
        (read-custom cnt))))

(def ctx-test (zen/new-context {:unsafe true}))
(def input {:way {:over {:there "WIN"}}})

;;TODO: eval mapping here
;;TODO custom entry point
;;TODO zj/path
;;NOTE: body is a string of text and it must be evaluated by reader
(defn eval-mapping [{:keys [body] :as req}]
  ;;just a plug at the moment
  (try
    (if body
      (let [{:keys [data code] :as body*} (read-body req)
            _ (println body*)
            data* (edn/read-string data)
            code* (edn/read-string code)
            ztx (zen/new-context {:unsafe true})
            _ (zen/load-ns ztx code*)
            entry-point (zen/get-symbol ztx (symbol (str (get code* 'ns) "/NaiveFour")))
            res (zj/apply-mapping data* (:body (zj/make-tsar-fn ztx entry-point)))]
        {:status 200
         :body res})
      {:status 400
       :body (json/generate-string
              {:message "No body in the request"})})
    (catch Exception e {:status 500
                        :body (.getMessage e)})))

(defn mapping-demo-static [req]
  (let [static-page (-> "index.html" io/resource slurp)]
    {:status 200
     :body static-page}))

