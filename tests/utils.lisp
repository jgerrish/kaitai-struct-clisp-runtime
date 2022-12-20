(defpackage kaitai-struct-clisp-runtime/tests/utils
  (:use :cl :kaitai-struct-clisp-runtime :rove))

(in-package :kaitai-struct-clisp-runtime/tests/utils)

;; NOTE: To run this test file, execute `(asdf:test-system :kaitai-struct-clisp-runtime)' in your Lisp.

;; Some simple tests to test the truthiness and other properties of
;; variables with ok
(deftest ok-works-as-expected
  (testing "ok succeeds for t"
    (ok t))
  (testing "ok succeeds for null nil"
    (ok (null nil)))
  (testing "ok succeeds for not null t"
    (ok (not (null t))))
  (testing "ok succeeds for function"
    (ok (lambda () 3)))
  (testing "ok succeeds for zero"
    (ok 0)))

;; Some simple tests to test the falsiness and other properties of
;; variables with ng
(deftest ng-works-as-expected
  (testing "ng succeeds for nil"
    (ng nil))
  (testing "ng succeeds for not null nil"
    (ng (not (null nil))))
  (testing "ng succeeds for not t"
    (ng (not t)))
  (testing "ng succeeds for not function"
    (ng (not (lambda () 3))))
  (testing "ng succeeds for not zero"
    (ng (not 0))))
