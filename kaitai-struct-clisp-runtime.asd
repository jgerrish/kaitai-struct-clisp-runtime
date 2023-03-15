;; System definition for kaitai-struct-clisp-runtime
;; A runtime for the Kaitai parsing framework

;; Features for the system

;; Disable floating support for now
;; Parsing negative infinity values is not working
;; (push :floating-point cl:*features*)

;; Disable unicode support for now
;; (push :kaitai-unicode cl:*features*)

;; Developers interested in adding support for Unicode should look at
;; the read-next-term method in the kaitai-stream class.  Help is
;; welcome.

;; system definition for the runtime, including the base kaitai-stream
;; and kaitai-struct classes.
(defsystem "kaitai-struct-clisp-runtime"
  :version "0.1.0"
  :author "Joshua Gerrish <jgerrish@gmail.com>"
  :license "MIT"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "utils")
		 (:file "kaitai-stream")
		 (:file "kaitai-struct"))))
  :description "Common LISP Runtime for Kaitai"
  :in-order-to ((test-op (test-op "kaitai-struct-clisp-runtime/tests"))))

;; system definition for tests using rove
(defsystem "kaitai-struct-clisp-runtime/tests"
  :author "Joshua Gerrish <jgerrish@gmail.com>"
  :license "MIT"
  :depends-on ("kaitai-struct-clisp-runtime"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "utils")
		 (:file "kaitai-stream")
		 (:file "kaitai-stream-read-bytes-term")
		 (:file "kaitai-struct"))))
  :description "Test system for kaitai-struct-clisp-runtime"
  :perform (test-op (op c) (symbol-call :rove :run c)))

;;
;; This is the system definition for tests using fiveam
;;
;; (defsystem "kaitai-struct-clisp-runtime/tests"
;;   :author "Joshua Gerrish <jgerrish@gmail.com>"
;;   :license "MIT"
;;   :depends-on ("kaitai-struct-clisp-runtime"
;; 	       "fiveam")
;;   :components ((:module "tests"
;;                 :components
;;                 ((:file "kaitai-stream")
;; 		 (:file "kaitai-struct"))))
;;   :description "Test system for kaitai-struct-clisp-runtime"
;;   :perform (test-op (o c) (symbol-call :fiveam '#:run! :foobar)))
