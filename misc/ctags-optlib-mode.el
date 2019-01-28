;;
;;  Copyright (c) 2019, Red Hat, Inc.
;;  Copyright (c) 2019, Masatake YAMATO
;;
;;  Author: Masatake YAMATO <yamato@redhat.com>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

(require 'generic)

(defun ctags-optlib-mode-setup-function ()
  (let ((st (syntax-table)))
    (modify-syntax-entry ?\' " " st)
    (modify-syntax-entry ?\" " " st)))

(define-generic-mode ctags-optlib-mode
  '(?#)
  nil
  '(;;
    ;; Language
    ;;
    ("^--\\(langdef\\)=\\([a-zA-Z0-9]+\\)"
     (1 font-lock-keyword-face t)
     (2 font-lock-type-face t))
    ("^--\\(map\\)-\\([a-zA-Z0-9]+\\)=.*"
     (1 font-lock-keyword-face t)
     (2 font-lock-type-face t))
    ("--\\(alias\\)-\\([a-zA-Z0-9]+\\)=.*"
     (1 font-lock-keyword-face t)
     (2 font-lock-type-face t))
    ;;
    ;; Kinds
    ;;
    ("^--\\(kinddef\\)-\\([^=]+\\)=\\([a-zA-Z]\\),\\([a-zA-Z0-9]+\\),\\(.*\\)$"
     (1 font-lock-keyword-face t)
     (2 font-lock-type-face t)
     (3 font-lock-constant-face t)
     (4 font-lock-variable-name-face t)
     (5 font-lock-doc-face t))
    ("^--\\(kinds\\)-\\([^=]+\\)=[+-]?\\([a-zA-Z]+\\)"
     (1 font-lock-keyword-face t)
     (2 font-lock-type-face t)
     (3 font-lock-constant-face t))
    ;;
    ;; Singe line regex
    ;;
    ("^--\\(regex\\)-\\([^=]+\\)="
     (1 font-lock-keyword-face t)
     (2 font-lock-type-face t))
    ;;
    ;; Mline regex
    ;;
    ("^--\\(mline-regex\\)-\\([^=]+\\)="
     (1 font-lock-keyword-face t)
     (2 font-lock-type-face t))
    ;;
    ;; Mtable regex
    ;;
    ("^--\\(_tabledef\\)-\\([^=]+\\)=\\([a-zA-Z0-9_]+\\)"
     (1 font-lock-keyword-face t)
     (2 font-lock-type-face t)
     (3 font-lock-function-name-face t)
     )
    ("^--\\(_mtable-regex\\)-\\([^=]+\\)=\\([a-zA-Z0-9_]+\\)/\\(.*\\)$"
     (1 font-lock-keyword-face t)
     (2 font-lock-type-face t)
     (3 font-lock-function-name-face t)
     (4 nil t))
    ("^--\\(_mtable-extend\\)-\\([^=]+\\)=\\([a-zA-Z0-9_]+\\)\\+\\([a-zA-Z0-9_]+\\)"
     (1 font-lock-keyword-face t)
     (2 font-lock-type-face t)
     (3 font-lock-function-name-face t)
     (4 font-lock-function-name-face t))
    ;;
    ;; Fields
    ;;
    ("^--\\(_fielddef\\)-\\([a-zA-Z0-9]+\\)=\\([a-zA-Z0-9]+\\),\\(.*\\)$"
     (1 font-lock-keyword-face t)
     (2 font-lock-type-face t)
     (3 font-lock-variable-name-face t)
     (4 font-lock-doc-face t))
    ("^--\\(fields\\)-\\([a-zA-Z0-9]+\\)=.?{\\([a-zA-Z0-9]+\\)}"
     (1 font-lock-keyword-face t)
     (2 font-lock-type-face t)
     (3 font-lock-variable-name-face t))
    ;;
    ;; Roles
    ;;
    ("^--\\(_roledef\\)-\\([a-zA-Z0-9]+\\)=\\([a-zA-Z]\\)\\.\\([a-zA-Z0-9]+\\),\\(.*\\)$"
     (1 font-lock-keyword-face t)
     (2 font-lock-type-face t)
     (3 font-lock-constant-face t)
     (4 font-lock-constant-face t)
     (5 font-lock-doc-face t))
    ;;
    ;; Extras
    ;;
    ("^--\\(_extradef\\)-\\([a-zA-Z0-9]+\\)=\\([a-zA-Z0-9]+\\),\\(.*\\)$"
     (1 font-lock-keyword-face t)
     (2 font-lock-type-face t)
     (3 font-lock-variable-name-face t)
     (4 font-lock-doc-face t))
    ("^--\\(extras\\)-\\([a-zA-Z0-9]+\\)=.?{\\([a-zA-Z0-9]+\\)}"
     (1 font-lock-keyword-face t)
     (2 font-lock-type-face t)
     (3 font-lock-variable-name-face t))
    ;;
    ;; Flags
    ;;
    (".*{\\(tenter\\|tjump\\)=\\([a-zA-Z0-9_]+\\)}"
     (1 font-lock-keyword-face t)
     (2 font-lock-function-name-face t))
    (".*{\\(tenter\\)=\\([a-zA-Z0-9_]+\\)\\(,\\([a-zA-Z0-9_]+\\)\\)}"
     (1 font-lock-keyword-face t)
     (2 font-lock-function-name-face t)
     (4 font-lock-function-name-face t))
    ("{\\(_field\\)=\\([a-zA-Z0-9_]+\\):[^}]+}"
     (1 font-lock-keyword-face t)
     (2 font-lock-variable-name-face t))
    ("{\\(_role\\)=\\([a-zA-Z0-9_]+\\)}"
     (1 font-lock-keyword-face t)
     (2 font-lock-constant-face t))
    ("{\\(_extra\\)=\\([a-zA-Z0-9_]+\\)}"
     (1 font-lock-keyword-face t)
     (2 font-lock-variable-name-face t))
    ("{\\(base\\)=\\([a-zA-Z0-9]+\\)}"
     (1 font-lock-keyword-face t)
     (2 font-lock-type-face t))
    ("{\\(_advanceTo\\)=[0-9]+\\(start\\|end\\)}"
     (1 font-lock-keyword-face t)
     (2 font-lock-builtin-face t))
    ("{\\(scope\\)=\\(pop\\|ref\\|set\\|push\\|clear\\)}"
     (1 font-lock-keyword-face t)
     (2 font-lock-builtin-face t))
    ("{\\(icase\\|exclusive\\|tleave\\|placeholder\\|tquit\\|mgroup\\|dedicated\\|shared\\|_trace\\)[^}]*}"
     (1 font-lock-keyword-face t))
    ("/\\([a-zA-Z]\\)/"
     (1 font-lock-constant-face t))
    )
  '("\\.ctags\\'")
  '(ctags-optlib-mode-setup-function)
  "Mode for editing .ctags optlib parser file")

(provide 'ctags-optlib-mode)
