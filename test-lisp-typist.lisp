(load "lisp-typist.lisp")

(in-package :lisp-typist)
(setf *current-row-index* 1
      *current-word-index* 0)
(unless (equal '("abc" "def" "ghi")
               (get-done-words '(("abc" "def") ("ghi"))))
  (error))
