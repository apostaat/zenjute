(ns zenjute.zenjute
  (:require [zen.core :as zen]
            [sci.core :as sci]
            [clojure.walk :as w])
  (:gen-class))

(defn sanitize-body [body]
  (w/postwalk (fn [el] (if (symbol? el)
                         (if (or (= "fn" (name el))
                                 (= "->" (name el))
                                 (= "->>" (name el))
                                 (= "merge" (name el)))
                           (symbol (name el))
                           (symbol (str (namespace el)
                                        "-" (name el))))
                         el)) body))

(defn get-zen-symbol [ctx tag]
  (->> tag
       (zen/get-tag ctx)
       first
       (zen/get-symbol ctx)))


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

(defn apply-mapping [data tsar-fn]
  (let [our-fn (->> tsar-fn
                    sanitize-body
                    str)

        sci-fn (str "(" our-fn data ")")]
    ;; our-fn
    (sci/eval-string sci-fn)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  nil)

(defn expand
  [_let_ body]
  (w/postwalk (fn [el] (if-let [our-let (get _let_ el)]
                         our-let
                         el)) body))

;; TODO sanitize remove quote

;; ;;{ifn body-fn}
;; (defn expand-import-fn
;;   [_let_ body]

;;   )

(def a (atom {}))

(def counter (atom 0))

;; (def tags #{'tag})

;; zen/tags =>
(defn recursive-search-by-tag
  [v ctx tags]
  (swap! counter inc)
  (let [schema (get-zen-symbol ctx (first tags))
        imports (:zj/import-fn schema)]
    (if (not-empty imports)
      ;;tags
      (let [exp-imports
            (mapv (fn [t]
                    (let [import-schema (get-zen-symbol ctx t)]
                      (let [res (recursive-search-by-tag v ctx (:zen/tags import-schema))]
                        res)

                      ))
                  imports)

            _ (swap! counter dec)
            ;;only one import for now

            imp (first exp-imports)
            tag-body {(first (:tag imp)) (:body imp)}


            exp-schema (->> (-> (expand tag-body (dissoc schema :zj/import-fn))
                                (assoc :zj/import-fn (:zj/import-fn schema)))
                            )

            ;; body (expand tag-body (:zj/body exp-schema))
            body (expand (:zj/let exp-schema) (:zj/body exp-schema))
            _ (vswap! v conj {(first (:zen/tags schema))
                              body})
            ]

        (swap! a assoc (keyword (gensym "expanded-")) {:step @counter
                                                       :vol @v
                                                       :tag (:zen/tags schema)
                                                       :tag-body tag-body
                                                       :body (:zj/body schema)
                                                       :exp-schema exp-schema
                                                       :exp-body body
                                                       })
        {:tag (:zen/tags schema)
         :body (get @v (first (:zen/tags schema)))}
        )
      (do
        (if-let [_let_ (:zj/let schema)]
          (do
            (prn "1")
            (vswap! v conj {(first (:zen/tags schema))
                            (expand _let_ (:zj/body schema))})
            {:tag (:zen/tags schema)
           :body (expand _let_ (:zj/body schema))})
          (do
            (prn "2")
            (vswap! v conj {(first (:zen/tags schema))
                              (:zj/body schema)})
            {:tag (:zen/tags schema)
             :body (:zj/body schema)}))))))

(defn make-tsar-fn
  [ctx {body :zj/body import-fn :zj/import-fn zen-tags :zen/tags :as proto-mapping}]
                                        ;check if imported ns also cotains imported-fn and import them
                                        ;until imported ns does not contain imported-fn
                                        ;this operation is done for each symbol in imported-fn vec

  (let [k (volatile! {})]
    (recursive-search-by-tag k ctx zen-tags)))

(comment


  (def paths {:paths ["test/zenjute/resources"]})

  (def ctx (zen/new-context {:unsafe true}))

  @ctx

(let [_ (zen/load-ns ctx {'ns 'one
                              'naive-mapping {:zen/tags #{'zen/tag}}
                              'NaiveMapping
                              {:zen/tags #{'naive-mapping}
                               :zj/let {'k :way}
                               :zj/body '(fn [v] {:go (-> v k :over :there)})}})
      _ (zen/load-ns ctx {'ns 'two
                          'naive-two {:zen/tags #{'zen/tag}}

                          'NaiveTwo
                          {:zen/tags #{'naive-two}
                           :zj/import-fn #{'one/naive-mapping}
                           :zj/body '(fn [v] {:one (one/naive-mapping v)})}})
      _ (zen/load-ns ctx {'ns 'three
                          'naive-three {:zen/tags #{'zen/tag}}
                          'naive-two {:zen/tags #{'zen/tag}}

                          'NaiveTwo
                          {:zen/tags #{'naive-two}
                           :zj/import-fn #{'one/naive-mapping}
                           :zj/body '(fn [v] {:one (one/naive-mapping v)})}

                          'NaiveThree
                          {:zen/tags #{'naive-three}
                           :zj/import-fn #{'three/naive-two}
                           :zj/let {'foo '(fn [x] (merge {:two (three/naive-two x)}
                                                         {:stuff "stuff"}))}
                           :zj/body '(fn [v] {:three (foo v)
                                              :one-more-time (three/naive-two v)})}})
      _ (zen/load-ns ctx {'ns 'four
                          'naive-four {:zen/tags #{'zen/tag}}
                          'NaiveFour
                          {:zen/tags #{'naive-four}
                           :zj/import-fn #{'three/naive-three}
                           :zj/let {'foo '(fn [x] (three/naive-three x))}
                           :zj/body '(fn [v] {:four (foo v)})}})
      res {:four {:three {:two {:one {:go "WIN"}}
                          :stuff "stuff"}
                  :one-more-time {:one {:go "WIN"}}}}
      input {:way {:over {:there "WIN"}}}]
  #_(get-zen-symbol ctx 'mt/naive-three)
  #_(make-tsar-fn ctx (get-zen-symbol ctx 'three/naive-three))
  (get-in (make-tsar-fn ctx (get-zen-symbol ctx 'four/naive-four))
          [:body]))



@a



  ; (TODO)
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

#_(if-let [ifns (:zj/import-fn import-schema)]
                        ;;#{one/naive-mapping}
                        (do
                          ;; (swap! a assoc (keyword (gensym "ifns-")) {:ifns ifns
                          ;;                                            :step @counter})
                          (let [res (recursive-search-by-tag v ctx ifns)
                                body-fn (:body res)
                                _tag_ (first (:tag res))
                                ;; _ (swap! counter dec)
                                ;; _ (swap! a assoc (keyword (gensym "bodyfns-")) {:body-fn body-fn
                                ;;                                                 :step @counter})
                                ;; _ (swap! a assoc (keyword (gensym "_tag_-")) _tag_)
                                ;; _ (swap! a assoc (keyword (gensym "ifns2-")) {:ifns ifns
                                ;;                                               :step @counter})
                                current-ifn (first (filter #(= _tag_ %) ifns))
                                expanded-imports {current-ifn body-fn}
                                ;; _ (swap! a assoc (keyword (gensym "exp-imports-")) {:exp-imports expanded-imports
                                ;;                                                     :step @counter})
                                body (expand expanded-imports (:zj/body import-schema))
                                body (if-let [_let_ (:zj/let import-schema)]
                                       {:tag t
                                        :body (expand _let_ body)}
                                       {:tag t
                                        :body body})]
                            (vswap! v conj {t (:body body)})
                            body))
                        (do
                          ;; (swap! a assoc (keyword (gensym "dead-zone-")) {:tags (:zen/tags import-schema)
                          ;;                                                 :step @counter})
                          (if-let [_let_ (:zj/let import-schema)]
                            (do (vswap! v conj {(first tags) (expand _let_ (:zj/body import-schema))})
                                {:body (expand _let_ (:zj/body import-schema))
                                 :tag (:zen/tags import-schema)})
                            (do (vswap! v conj {(first tags) (:zj/body import-schema)})
                                {:body (:zj/body import-schema)
                                 :tag (:zen/tags import-schema)}))))

  )
