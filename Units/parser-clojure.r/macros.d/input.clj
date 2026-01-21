;; Taken from https://clojuredocs.org/clojure.core/defmacro

(defmacro with-tree
  "works on a JTree and restores its expanded paths after executing body"
  [tree & body]
  `(let [tree# ~tree
         root# (.getRoot (.getModel tree#))
         expanded# (if-let [x# (.getExpandedDescendants
                                tree# (TreePath. root#))]
                     (enumeration-seq x#)
                     ())
         selectionpaths# (. selectionmodel# getSelectionPaths)]
     ~@body
     (doseq [path# expanded#]
       (.expandPath tree# path#))))

(defmacro unless [pred a b]
  `(if (not ~pred) ~a ~b))
