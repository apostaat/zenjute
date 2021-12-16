(ns zenjute.zenjute-test
  (:require [clojure.test :refer :all]
            [zen.core :as zen]
            [sci.core :as sci]
            [clojure.string :as str]))

;(remove-ns 'zenjute.zenjute-test)

(def paths {:paths ["test/zenjute/resources"]})
(def ctx (zen/new-context paths {:unsafe true}))

(zen/load-ns ctx 'mapping-two)

(zen/load-ns ctx {'ns 'mapping
                  'naive-mapping {:zen/tags #{'zen/tag}}

                  'NaiveMapping

                  {:zen/tags #{'naive-mapping}
                   :zj/body '(fn [v] {:id (-> v :k :j :l)})}})

(->> (zen/get-symbol ctx 'mapping/NaiveMapping)
     :zj/body)

(->> (zen/get-tag ctx 'mapping/naive-mapping)
     first
     (zen/get-symbol ctx)
     :zj/body)

@ctx

;TODOLIST:
; - load schema from file
; - load body as list of symbols
; - evaluate body via sci and wrap around data as arg

(deftest sci-test
  (testing "if args are not given - the default argument is all data"
    (let [sci-fn (get-fn-by-tag 'our-mappin)
          data {:currency "boo"}
          result (apply-sci sci-fn data)]
      (is (= "BOO" result)))))

;   or particular keys



; - make path function ^:path[:k1 :k2 :k3]
;   that works with arbitrary nestity

(deftest path-test
  (is (= :boo (check-path {:transaction_date
                           {:from
                            {:city ^zj/path[:currentDate]}}}
                          {:currentData :boo}))))

(fn [x] {:t {:f {:c (:cur x)}}})

(get-in {:k {:j [1 2 3]}} [:k :j 0])

;; (zen/get-symbol ctx 'mapping/NaiveMapping
;; (zen/read-ns ctx 'mapping)
;; (zen/load-ns ctx 'mapping)

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))
