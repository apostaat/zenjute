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

(defn expand-let
  [_let_ body]
  (w/postwalk (fn [el] (if-let [our-let (get _let_ el)]
                         our-let
                         el)) body))

(defn recursive-search-import
  [ctx {ifns :zj/import-fn}]
  (->> ifns
       (mapv (partial get-zen-symbol ctx))
       (mapv (fn [y] (if-let [ifn (:zj/import-fn y)]
                       (recursive-search-import ctx ifn)
                       y)))))

;; TODO sanitize remove quote

#_(defn recursive-search-by-tag
  [ctx x]
  (->> x
       (mapv (get-zen-symbol ctx))
       (mapv (fn [y] (if-let [ifn (:zj/import-fn y)]
                                        ;(recursive-search-by-tag ctx ifn)
                       (expand-symbol y))))))

(defn expand-import-fn
  [{body :zj/body import-fn :zj/import-fn :as proto-mapping}]
                                        ;check if imported ns also cotains imported-fn and import them
                                        ;until imported ns does not contain imported-fn
                                        ;this operation is done for each symbol in imported-fn vec

  (recursive-search-by-tag import-fn))

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

(comment

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
