(defun a0 (n) (+ 1 n))

(defvar b0 3)

(defconstant c0 5)

(defmacro defunknown0 (s)
  `(defconstant ,s 'unknown))

(defalias 'e0 'a0)

(defvaralias 'f0 'b0)

(defsubst g0 () nil)

(define-error 'h0 (purecopy "TEST"))
(define-error (quote h1) (purecopy "TEST"))

(define-minor-mode i0 "DOC")

(define-derived-mode j0 nil "TEST")

(defcustom k0 t
  "doc"
  :type 'number
  :group 'l0
  :version "22.1")

(defgroup l0 nil "DOC" :group 'editing)

(defface m0
  '((t (:inherit region)))
  "DOC"
  :group 'l0
  :version "22.1")

(deftheme n0 "DOC")

(condition-case nil
(define-key dont-capture-me-map "m" 'ctags)
(error nil))

(defvar-local o0 1)


(define-globalized-minor-mode p0
  visual-line-mode turn-on-visual-line-mode)

(define-obsolete-function-alias 'q0 'f0)

(define-global-minor-mode r0
  subword-mode turn-on-subword-mode)

(define-inline s0 (n)
  (+ n 1))

(defun* t0 (n)
  (+ n 1))

(defmacro* defunknown1 (s)
  `(defconstant ,s 'unknown))

(defsubst* u0 () nil)

