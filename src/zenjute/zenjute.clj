(ns myname.myapp
  (:require [zen.core :as zen]
            [sci.core :as sci]
            [clojure.walk :as w])
  (:gen-class))

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
  [ctx tags]
  (swap! counter inc)
  (swap! a assoc (keyword (gensym "tags-")) {:tags tags
                                             :step @counter})
  (let [schema (get-zen-symbol ctx (first tags))
        imports (:zj/import-fn schema)]
    (swap! a assoc (keyword (gensym "imports-")) {:imports imports
                                                  :step @counter})
    (if (not-empty imports)
      ;;tags
      (mapv (fn [t]
              (let [import-schema (get-zen-symbol ctx t)]
                (if-let [ifns (:zj/import-fn import-schema)]
                  ;;#{one/naive-mapping}
                  (do
                    (swap! a assoc (keyword (gensym "ifns-")) {:ifns ifns
                                                               :step @counter})
                    (let [res (recursive-search-by-tag ctx ifns)
                          body-fn (:body res)
                          _tag_ (first (:tag res))
                          _ (swap! counter dec)
                          _ (swap! a assoc (keyword (gensym "bodyfns-")) {:body-fn body-fn
                                                                        :step @counter})
                          _ (swap! a assoc (keyword (gensym "_tag_-")) _tag_)
                          _ (swap! a assoc (keyword (gensym "ifns2-")) {:ifns ifns
                                                                        :step @counter})
                          current-ifn (first (filter #(= _tag_ %) ifns))
                          expanded-imports {current-ifn body-fn}
                          _ (swap! a assoc (keyword (gensym "exp-imports-")) {:exp-imports expanded-imports
                                                                              :step @counter})
                          body (expand expanded-imports (:zj/body import-schema))
                          body (if-let [_let_ (:zj/let import-schema)]
                                 {:tag t
                                  :body (expand _let_ body)}
                                 {:tag t
                                  :body body})]
                      body))
                  (do
                    (swap! a assoc (keyword (gensym "dead-zone-")) {:tags (:zen/tags import-schema)
                                                                    :step @counter})
                    (if-let [_let_ (:zj/let import-schema)]
                     (expand _let_ (:zj/body import-schema))
                     (:zj/body import-schema))))))
            imports)
      (do
        (swap! a assoc (keyword (gensym "recursion-exit-")) {:tags tags
                                                             :step @counter})
        (if-let [_let_ (:zj/let schema)]
          {:tag (:zen/tags schema)
           :body (expand _let_ (:zj/body schema))}
          {:tag (:zen/tags schema)
           :body (:zj/body schema)})))))

(defn make-tsar-fn
  [ctx {body :zj/body import-fn :zj/import-fn zen-tags :zen/tags :as proto-mapping}]
                                        ;check if imported ns also cotains imported-fn and import them
                                        ;until imported ns does not contain imported-fn
                                        ;this operation is done for each symbol in imported-fn vec

  (recursive-search-by-tag ctx zen-tags))

(comment


  (def paths {:paths ["test/zenjute/resources"]})

  (def ctx (zen/new-context {:unsafe true}))

  @ctx

(let [_ (zen/load-ns ctx {'ns 'one
                              'naive-mapping {:zen/tags #{'zen/tag}}
                              'NaiveMapping
                              {:zen/tags #{'naive-mapping}
                               :zj/let {'k :k}
                               :zj/body '(fn [v] {:id (-> v k :j :l)})}})
      _ (zen/load-ns ctx {'ns 'two
                          'naive-two {:zen/tags #{'zen/tag}}
                          'NaiveTwo
                          {:zen/tags #{'naive-two}
                           :zj/import-fn #{'one/naive-mapping}
                           :zj/body '(fn [v] {:key2 (one/naive-mapping v)})}})
      _ (zen/load-ns ctx {'ns 'three
                          'naive-three {:zen/tags #{'zen/tag}}
                          'NaiveThree
                          {:zen/tags #{'naive-three}
                           :zj/import-fn #{
                                           'two/naive-two
                                           #_'one/naive-mapping}
                           :zj/let {'foo '(fn [x] (merge {:id (two/naive-two x)}
                                                         {:k 5}))}
                           :zj/body '(fn [v] {:foo (foo v)
                                              #_#_:naive-mapping (one/naive-mapping v)})}})
      _ (zen/load-ns ctx {'ns 'four
                          'naive-four {:zen/tags #{'zen/tag}}
                          'NaiveFour
                          {:zen/tags #{'naive-four}
                           :zj/import-fn #{
                                           'three/naive-three
                                           #_'one/naive-mapping}
                           :zj/let {'kal '(fn [x] (three/naive-three x))}
                           :zj/body '(fn [v] {:kal (kal v)})}})
      res (fn [v] {:foo {:id {:key2 {:id (-> v :k :j :l)}}
                         :k 5}
                   #_#_:naive-mapping {:id (-> v :k :j :l)}})
      input {:k {:j {:l "WIN"}}}]
  #_(get-zen-symbol ctx 'mt/naive-three)
  #_(make-tsar-fn ctx (get-zen-symbol ctx 'three/naive-three))
  (make-tsar-fn ctx (get-zen-symbol ctx 'four/naive-four))
  )

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

  )
