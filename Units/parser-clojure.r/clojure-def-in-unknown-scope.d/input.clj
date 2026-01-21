(ns N)

(defn defn [n]
  1)

(ns S
  (:require [N]))

(N/defn 'g)
(clojure.core/defn h [] 1)
