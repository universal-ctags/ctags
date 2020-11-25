(defn my-fn [])

(defn ^:private my-private-fn [])

(defn ^{:foo :bar} my-public-fn [])

(defn my-hinted-fn ^MyReturnType [])

(defn ^:private my-private-hinted-fn ^MyReturnType [])

(defn ^:private so-many-hints ^MyReturnType
  [^MyArgType x])
