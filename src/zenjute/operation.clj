(ns zenjute.operation
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.edn   :as edn]))

(defn read-body [{:keys [body] :as req}]
  (let [cnt (slurp body)]
    (if (#{"application/json"} (get-in req [:headers "content-type"]))
      (json/parse-string cnt true)
      (edn/read-string cnt))))

;;TODO: eval mapping here.
;;NOTE: body is a string of text and it must be evaluated by reader
(defn eval-mapping [{:keys [body] :as req}]
  ;;just a plug at the moment
  (try
    (if body
      (let [{:keys [data code] :as body*} (read-body req)
            data* (edn/read-string data)
            code* (edn/read-string code)]
        (println (type body*))
        {:status 200
         :body data})
      {:status 400
       :body (json/generate-string
              {:message "No body in the request"})})
    (catch Exception e {:status 500
                        :body (.getMessage e)})))

(defn mapping-demo-static [req]
  (let [static-page (-> "index.html" io/resource slurp)]
    {:status 200
     :body static-page}))

(type (edn/read-string "{:data some}"))
