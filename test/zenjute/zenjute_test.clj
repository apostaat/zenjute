(ns zenjute.zenjute-test
  (:require [clojure.test :refer :all]
            [zen.core :as zen]
            [sci.core :as sci]
            [clojure.string :as str]
            [clojure.walk :as w]))

;(remove-ns 'zenjute.zenjute-test)

(def paths {:paths ["test/zenjute/resources"]})
(def ctx (zen/new-context paths {:unsafe true}))

(defn sanitize-body [body]
  (w/postwalk (fn [el] (if (symbol? el)
                         (if (or (= "fn" (name el))
                                 (= "->" (name el))
                                 (= "->>" (name el)))
                           (symbol (name el))
                           (symbol (str (namespace el)
                                        "-" (name el))))
                         el)) body))
(defn get-zen-symbol [ctx tag]
  (->> tag
       (zen/get-tag ctx)
       first
       (zen/get-symbol ctx)))

(defn expand-let
  [_let_ body]
  (w/postwalk (fn [el] (if-let [our-let (get _let_ el)]
                           our-let
                           el)) body))
(expand-let
 {'k (fn [x] {:id x})
  'j :j}
 '(fn [v] {:id (-> v k j :l)}))


(defn recursive-search-import
  [ctx {ifns :zj/import-fn}]
  (->> ifns
       (mapv (partial get-zen-symbol ctx))
       (mapv (fn [y] (if-let [ifn (:zj/import-fn y)]
                       (recursive-search-import ctx ifn)
                       y)))))


;; TODO sanitize remove quote

(deftest recursive-search-import-test
  (testing "we create three 'nested' namespaces"
    (let [_ (zen/load-ns ctx {'ns 'mapping
                              'naive-mapping {:zen/tags #{'zen/tag}}

                              'NaiveMapping

                              {:zen/tags #{'naive-mapping}
                               :zj/let {'k :k}
                               :zj/body '(fn [v] {:id (-> v 'k :j :l)})}})

          _ (zen/load-ns ctx {'ns 'mtw
                              'naive-mapping {:zen/tags #{'zen/tag}}

                              'NaiveTwo

                              {:zen/tags #{'naive-two}
                               :zj/import-fn #{'mapping/naive-mapping}
                               :zj/let {'k :k}
                               :zj/body '(fn [v] {:id (-> v 'k :j :l)})}})

          _ (zen/load-ns ctx {'ns 'mt
                              'naive-three {:zen/tags #{'zen/tag}}

                              'NaiveThree
                              {:zen/tags #{'naive-three}
                               :zj/import-fn #{'mtw/naive-two
                                               'mapping/naive-mapping}
                               :zj/let {'foo '(fn [x] (merge {:id x}
                                                              {:k 5}))}
                               :zj/body {:id 'foo
                                         :boo 'foo}}})]
      (is (= 5 (recursive-search-import ctx (get-zen-symbol ctx 'mt/naive-three)))))))


(let [x1 {:tag :x1
          :import [:x2 :x3]
          :let {:j []}
          :body []}])

{'ns 'mt
 'naive-three {:zen/tags #{'zen/tag}}

 'NaiveThree
 {:zen/tags #{'naive-three}
  :zj/import-fn #{{{:zen/tags #{'naive-two}
                    :zj/import-fn #{{:zen/tags #{'naive-mapping}
                                     :zj/let {'k :k}
                                     :zj/body '(fn [v] {:id (-> v 'k :j :l)})}}
                    :zj/let {'k :k}
                    :zj/body '(fn [v] {:id (-> v 'k :j :l)})}}}
  :zj/let {'foo '(fn [x] (merge {:id x}
                                {:k 5}))}
  :zj/body {:id 'foo
            :boo 'foo}}}

;(TODO)
; we get first import
; we get to the bottom import
; when we reached the bottom
; we expand bottom let and make namespaced-fn
; then we go one level up and start expanding
; current node:
;; we substitute all instances of first import-fn symbol
;; in let and body
;; we substite all instances of second import-fn but beforehand we do the same
;; reaching the bottom
;; and we do it until we run out of import-fn
;; and after that we expand current level let and pass it towards upper level
;; of recursion

#_(defn expand-import-fn
  [{body :zj/body import-fn :zj/import-fn :as proto-mapping}]
 ;check if imported ns also cotains imported-fn and import them
 ;until imported ns does not contain imported-fn
 ;this operation is done for each symbol in imported-fn vec

  (recursive-search-by-tag import-fn))

#_(defn create-tsar-fn [ctx zen-tag]
  (let [proto-mapping (get-zen-symbol ctx zen-tag)
        x (substitute-symbols proto-mapping)]))

(deftest symbol-test
  (testing "if symbol is substituted to namespace-fn"
    (let [_ (zen/load-ns ctx {'ns 'mapping
                              'naive-mapping {:zen/tags #{'zen/tag}}

                              'NaiveMapping

                              {:zen/tags #{'naive-mapping}
                               :zj/let ['k :k]
                               :zj/body '(fn [v] {:id (-> v 'k :j :l)})}})

          _ (zen/load-ns ctx {'ns 'mt
                              'naive-three {:zen/tags #{'zen/tag}}

                              'NaiveThree
                              {:zen/tags #{'naive-three}
                               :zj/import-fn ['naive-mapping]
                               :zj/let ['foo (fn [x] (merge {:id x}
                                                              {:k 5}))]
                               :zj/body {:id 'foo
                                         :boo 'foo}}})]
      (= '(fn [d] {:id (fn [mt-x] (merge {:id mt-x}
                                         {:k 5}))
                   :boo (fn [mt-x] (merge {:id mt-x}
                                          {:k 5}))})
         (create-tsar-fn ctx 'naive-three)))))

(defn apply-mapping [ctx tag data]
  (let [our-fn (->> tag
                    (zen/get-tag ctx)
                    first
                    (zen/get-symbol ctx)
                    :zj/body
                    sanitize-body
                    str)
        sci-fn (str "(" our-fn data ")")]
    (sci/eval-string sci-fn)))

(zen/get-tag ctx 'mapping-two/naive-mapping-two)

(apply-mapping ctx 'mapping/naive-mapping {:k {:j {:l 5}}})

;;; THOUGHTS
;;; pseudonamespaced-fns (fn [x] -> fn [ns-x]) (done)
;;; tsar-fn
;;; zj/body
;;; zj/path
;;; import-fn
;;; symbol

;TODOLIST:
; - load schema from file DONE
; - load body as list of non namespaced symbols DONE
; - evaluate body via sci and wrap around data as arg DONE

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
                            {:city ^zj/path [:currentDate]}}}
                          {:currentData :boo}))))
