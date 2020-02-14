(defun a0 (n) (+ 1 n))
(DEFUN A1 (n) (+ 2 n))

(defvar b0 3)
(DEFVAR B1 4)

(defconstant c0 5)
(DEFCONSTANT C1 6)

(defmacro defunknown0 (s)
  `(defconstant ,s 'unknown))

(DEFMACRO DEFUNKNOWN1 (s)
   `(defconstant ,s 'unknown))

(defunknown1 unknown)

