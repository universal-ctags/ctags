(ns app.controller)

 (defn empty-fn [])

(defn function-with-body []
    (println "body"))

(clojure.core/defn core-function-with-body []
    (println "core"))

'(defn quoted-function [])
(quote quoted-function2 [])

(ns another.name)
(defn x [])
