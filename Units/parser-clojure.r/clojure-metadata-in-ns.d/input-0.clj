(ns ^:dynamic ns1)
(defn debug1? ^boolean
  []
  goog.DEBUG)

(ns ^String ns2)
(defn debug2? ^boolean
  []
  goog.DEBUG)

(ns ^{:doc "Utility functions."} ^{:doc1 "something"} ns00)
(defn debug00? ^boolean
  []
  goog.DEBUG)

(ns ^{:doc "Utility functions."} ^:dynamic ns01)
(defn debug01? ^boolean
  []
  goog.DEBUG)

(ns ^{:doc "Utility functions."} ^String ns02)
(defn debug02? ^boolean
  []
  goog.DEBUG)


(ns ^:dynamic ^{:doc "Utility functions."} ns10)
(defn debug10? ^boolean
  []
  goog.DEBUG)

(ns ^:dynamic ^:dynamic-something ns11)
(defn debug11? ^boolean
  []
  goog.DEBUG)

(ns ^:dynamic ^String ns12)
(defn debug12? ^boolean
  []
  goog.DEBUG)

(ns ^String ^{:doc "Utility functions."} ns20)
(defn debug20? ^boolean
  []
  goog.DEBUG)

(ns ^String ^:dynamic ns21)
(defn debug21? ^boolean
  []
  goog.DEBUG)

(ns ^String ^String-something ns22)
(defn debug22? ^boolean
  []
  goog.DEBUG)
