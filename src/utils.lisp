(defpackage kaitai-struct-clisp-runtime
  (:use :cl)
  (:export  :with-gensyms))

(in-package :kaitai-struct-clisp-runtime)

;; General utility functions can go in here

;; The version.  It should match the version the compiler uses.
(defvar *version* "0.1.0")

;; Provide a context to use generated symbols so we don't pollute or
;; interfere with the namespace with temporary variables in macros
;; From Practical Common Lisp
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))
