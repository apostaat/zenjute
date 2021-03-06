(ns zenjute.server
  (:require [ring.middleware.reload :refer [wrap-reload]]
            [ring.middleware.cors   :refer [wrap-cors]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.json   :refer [wrap-json-response wrap-json-body]]
            [org.httpkit.server :as server]
            [route-map.core  :as rm]
            [zenjute.operation]))

(def routes
  {"$eval-mapping" {:POST zenjute.operation/eval-mapping}
   "mapping-demo"  {:GET zenjute.operation/mapping-demo-static}
   })

(defn params-to-keyword [params]
  (reduce-kv (fn [acc k v]
               (assoc acc (keyword k) v)) {} params))

(defn handler [{meth :request-method uri :uri :as req}]
  (if-let [res (rm/match [meth uri] routes)]

    ((:match res) (-> (assoc req :params (params-to-keyword (:params req)))
                      (update-in [:params] merge (:params res))))
    {:status 404 :body {:error "Not found"}}))

(defn preflight
  [{meth :request-method hs :headers :as req}]
  (let [headers (get hs "access-control-request-headers")
        origin (get hs "origin")
        meth  (get hs "access-control-request-method")]
    {:status 200
     :headers {"Access-Control-Allow-Headers" headers
               "Access-Control-Allow-Methods" meth
               "Access-Control-Allow-Origin" origin
               "Access-Control-Allow-Credentials" "true"
               "Access-Control-Expose-Headers" "Location, Transaction-Meta, Content-Location, Category, Content-Type, X-total-count"}}))

(defn allow [resp req]
  (let [origin (get-in req [:headers "origin"])]
    (update resp :headers merge
            {"Access-Control-Allow-Origin" origin
             "Access-Control-Allow-Credentials" "true"
             "Access-Control-Expose-Headers" "Location, Content-Location, Category, Content-Type, X-total-count"})))

(defn mk-handler [dispatch]
  (fn [{headers :headers uri :uri :as req}]
    (if (= :options (:request-method req))
      (preflight req)
      (let [resp (dispatch req)]
        (-> resp (allow req))))))

(def app
  (-> handler
      mk-handler
      wrap-json-body
      wrap-params
      wrap-json-response
      wrap-reload))

(defonce state (atom nil))

(defn stop-server []
  (when-not (nil? @state)
    (@state :timeout 100)
    (reset! state nil)))

(defn start-server []
  (reset! state (server/run-server app {:port 9999})))

(defn restart-server [] (stop-server) (start-server))

(comment


  (restart-server)


  )
