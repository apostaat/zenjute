(ns zenjute.operation
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.edn   :as edn]))

;;TODO: eval mapping here.
;;NOTE: body is a string of text and it must be evaluated by reader
(defn eval-mapping [{:keys [body] :as req}]
  ;;just a plug at the moment
  (try
    (if body
      (let [cnt (slurp body)]
        {:status 200
         :body (edn/read-string cnt)})
      {:status 400
       :body (json/generate-string
              {:message "No body in the request"})})
    (catch Exception e {:status 500
                        :body (.getMessage e)})))

(defn mapping-demo-static [req]
  (let [static-page (-> "index.html" io/resource slurp)]
    {:status 200
     :body static-page}))
