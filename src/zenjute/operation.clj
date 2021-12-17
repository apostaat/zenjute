(ns zenjute.operation
  (:require [cheshire.core :as json]))

;;TODO: eval mapping here.
;;NOTE: body is a string of text and it must be evaluated by reader
(defn eval-mapping [{:keys [body] :as req}]
  ;;just a plug at the moment
  (if body
    {:status 200
     :body body}
    {:status 400
     :body (json/generate-string
            {:message "No body in the request"})})) 
