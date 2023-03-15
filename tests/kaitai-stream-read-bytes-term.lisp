;; Tests for kaitai-stream read-bytes-term
;;
;; This package contains all of the tests for read-bytes-term.
;;
;; It is arranged around a test plan outlined in the table below.
;;
;; Tests are named systematically with the following pattern:
;;
;; test-read-bytes-{encoding}-{has-term?}-{include-term}-{consume-term}-{eos-err}?
;; For example: test-read-bytes-ascii-yes-t-t-f
;; This would be test an ascii encoding, where the term occurs in the stream,
;; the term should be included in the return, the term should be consumed and
;; missing term not raise an exception.
(defpackage kaitai-struct-clisp-runtime/tests/kaitai-stream/read-bytes-term
  (:use :cl :kaitai-stream :rove :kaitai-struct-clisp-runtime/tests/kaitai-stream))

(in-package :kaitai-struct-clisp-runtime/tests/kaitai-stream/read-bytes-term)

(deftest read-bytes-term
  (progn
    (kaitai-struct-clisp-runtime/tests/kaitai-stream:test-full-file
     #'test-read-bytes-term-ascii-no-term-nil-nil-nil)
    (kaitai-struct-clisp-runtime/tests/kaitai-stream:test-full-file
     #'test-read-bytes-term-ascii-no-term-nil-nil-t)
    (kaitai-struct-clisp-runtime/tests/kaitai-stream:test-full-file
     #'test-read-bytes-term-ascii-no-term-nil-t-nil)
    (kaitai-struct-clisp-runtime/tests/kaitai-stream:test-full-file
     #'test-read-bytes-term-ascii-no-term-nil-t-t)
    (kaitai-struct-clisp-runtime/tests/kaitai-stream:test-full-file
     #'test-read-bytes-term-ascii-no-term-t-nil-nil)
    (kaitai-struct-clisp-runtime/tests/kaitai-stream:test-full-file
     #'test-read-bytes-term-ascii-no-term-t-nil-t)
    (kaitai-struct-clisp-runtime/tests/kaitai-stream:test-full-file
     #'test-read-bytes-term-ascii-no-term-t-t-nil)
    (kaitai-struct-clisp-runtime/tests/kaitai-stream:test-full-file
     #'test-read-bytes-term-ascii-no-term-t-t-t)
    (kaitai-struct-clisp-runtime/tests/kaitai-stream:test-full-file
     #'test-read-bytes-term-ascii-has-term-nil-nil-nil)
    (kaitai-struct-clisp-runtime/tests/kaitai-stream:test-full-file
     #'test-read-bytes-term-ascii-has-term-nil-nil-t)
    (kaitai-struct-clisp-runtime/tests/kaitai-stream:test-full-file
     #'test-read-bytes-term-ascii-has-term-nil-t-nil)
    (kaitai-struct-clisp-runtime/tests/kaitai-stream:test-full-file
     #'test-read-bytes-term-ascii-has-term-nil-t-t)
    (kaitai-struct-clisp-runtime/tests/kaitai-stream:test-full-file
     #'test-read-bytes-term-ascii-has-term-t-nil-nil)
    (kaitai-struct-clisp-runtime/tests/kaitai-stream:test-full-file
     #'test-read-bytes-term-ascii-has-term-t-nil-t)
    (kaitai-struct-clisp-runtime/tests/kaitai-stream:test-full-file
     #'test-read-bytes-term-ascii-has-term-t-t-nil)
    (kaitai-struct-clisp-runtime/tests/kaitai-stream:test-full-file
     #'test-read-bytes-term-ascii-has-term-t-t-t)

    ;; Test non-ascii encodings

    ;; utf-8
    (kaitai-struct-clisp-runtime/tests/kaitai-stream:test-full-file
     #'test-read-bytes-term-utf-8-has-term-t-t-t)
    ;; unknown encoding
    (kaitai-struct-clisp-runtime/tests/kaitai-stream:test-full-file
     #'test-read-bytes-term-blargh-has-term-t-t-t)))

(defun test-read-bytes-term-ascii-no-term-nil-nil-nil (ks)
  (progn
    (testing "test-read-bytes-term no-term nil nil nil should not raise error"
      (ok (equalp (read-bytes-term ks "ascii" (code-char 45) t nil nil)
		  #(#X48 #X65 #X6C #X6C #X6F #X20 #X57 #X6F #X72 #X6C #X64 #X21))))
    (testing "test-read-bytes-term no-term nil nil nil should not raise error"
      (ok (= (pos ks) 12)))))

(defun test-read-bytes-term-ascii-no-term-nil-nil-t (ks)
  (handler-case
      (progn
	(read-bytes-term ks "ascii" (code-char 45) nil t t)
	;; We shouldn't get this far
	(testing
	    "test-read-bytes-term no-term nil nil t should raise error"
	  (ok nil)))
    (kaitai-stream-eof-error (e)
      (testing "test-read-bytes-term no-term t nil nil should raise error"
	(ok (= (pos ks) 12))))))

(defun test-read-bytes-term-ascii-no-term-nil-t-nil (ks)
  (progn
    (testing "test-read-bytes-term no-term nil t nil should not raise error"
      (ok (equalp (read-bytes-term ks "ascii" (code-char 45) t nil nil)
		  #(#X48 #X65 #X6C #X6C #X6F #X20 #X57 #X6F #X72 #X6C #X64 #X21))))
    (testing "test-read-bytes-term no-term nil t nil should not raise error"
      (ok (= (pos ks) 12)))))

(defun test-read-bytes-term-ascii-no-term-nil-t-t (ks)
  (handler-case
      (progn
	(read-bytes-term ks "ascii" (code-char 45) nil t t)
	;; We shouldn't get this far
	(testing
	    "test-read-bytes-term no-term nil nil t should raise error"
	  (ok nil)))
    (kaitai-stream-eof-error (e)
      (testing "test-read-bytes-term no-term nil t t should raise error"
	(ok (= (pos ks) 12))))))

(defun test-read-bytes-term-ascii-no-term-t-nil-nil (ks)
  (progn
    (testing "test-read-bytes-term no-term t nil nil should not raise error"
      (ok (equalp (read-bytes-term ks "ascii" (code-char 45) t nil nil)
		  #(#X48 #X65 #X6C #X6C #X6F #X20 #X57 #X6F #X72 #X6C #X64 #X21))))
    (testing "test-read-bytes-term no-term t nil nil should not raise error"
      (ok (= (pos ks) 12)))))

;; 6
(defun test-read-bytes-term-ascii-no-term-t-nil-t (ks)
  (handler-case
      (progn
	(read-bytes-term ks "ascii" (code-char 45) t t t)
	;; Shouldn't get here
	(testing "test-read-bytes-term t nil t no term should raise kaitai-stream-eof-error"
	  (ok nil)))
    (kaitai-stream-eof-error (e)
      (testing "test-read-bytes-term t nil t no term should raise kaitai-stream-eof-error"
	(ok (string= "end of stream reached, but no terminator - found"
		     (kaitai-stream-eof-error-text e))))
      (testing "test-read-bytes-term t eos-error no term should set file stream position"
	(ok (= (pos ks) 12))))))

(defun test-read-bytes-term-ascii-no-term-t-t-nil (ks)
  (progn
    (testing "test-read-bytes-term nil eos-error no term t t nil should not raise error"
      (ok (equalp (read-bytes-term ks "ascii" (code-char 45) t t nil)
		  #(#X48 #X65 #X6C #X6C #X6F #X20 #X57 #X6F #X72 #X6C #X64 #X21))))
    (testing "test-read-bytes-term nil eos-error no term t t nil should not raise error"
      (ok (= (pos ks) 12)))))

(defun test-read-bytes-term-ascii-no-term-t-t-t (ks)
  (handler-case
      (progn
	(read-bytes-term ks "ascii" (code-char 45) t t t)
	;; Shouldn't get here
	(testing "test-read-bytes-term t eos-error no term should"
	  (ok nil)))
    (kaitai-stream-eof-error (e)
      (testing (format nil "test-read-bytes-term t eos-error no term should raise kaitai-stream-eof-error"
		       (kaitai-stream-eof-error-text e))
	(ok (string= "end of stream reached, but no terminator - found"
		     (kaitai-stream-eof-error-text e))))
      (testing "test-read-bytes-term t eos-error no term should set file stream position"
	(ok (= (pos ks) 12))))))

;; Test cases where the term is in the stream

(defun test-read-bytes-term-ascii-has-term-nil-nil-nil (ks)
  (progn
    (testing "test-read-bytes-term has-term nil nil nil should return right answer"
      (ok (equalp (read-bytes-term ks "ascii" (code-char 32) nil nil nil)
		  #(#X48 #X65 #X6C #X6C #X6F))))
    (testing "test-read-bytes-term should nil t t should set position correctly"
      (ok (= (kaitai-stream:pos ks) 5)))))

(defun test-read-bytes-term-ascii-has-term-nil-nil-t (ks)
  (progn
    (testing "test-read-bytes-term nil nil t should return right answer"
      (ok (equalp (read-bytes-term ks "ascii" (code-char 32) nil nil t)
		  #(#X48 #X65 #X6C #X6C #X6F))))
    (testing "test-read-bytes-term nil nil t should set position correctly"
      (ok (= (pos ks) 5)))))

(defun test-read-bytes-term-ascii-has-term-nil-t-nil (ks)
  (progn
    (testing "test-read-bytes-term nil t nil should return right answer"
      (ok (equalp (read-bytes-term ks "ascii" (code-char 32) nil t nil)
		  #(#X48 #X65 #X6C #X6C #X6F))))
    (testing "test-read-bytes-term nil t nil should set position correctly"
      (ok (= (pos ks) 6)))))

(defun test-read-bytes-term-ascii-has-term-nil-t-t (ks)
  (progn
    (testing "test-read-bytes-term has term nil t t should return right answer"
      (ok (equalp (read-bytes-term ks "ascii" (code-char 32) nil t t)
		  #(#X48 #X65 #X6C #X6C #X6F))))
    (testing "test-read-bytes-term has term nil t t should set position correctly"
      (ok (= (pos ks) 6)))))

(defun test-read-bytes-term-ascii-has-term-t-nil-nil (ks)
  (progn
    (testing "test-read-bytes-term has term t nil nil should return right answer"
      (ok (equalp (read-bytes-term ks "ascii" (code-char 32) t nil nil)
		  #(#X48 #X65 #X6C #X6C #X6F #X20))))
    (testing "test-read-bytes-term has term t nil nil should set position correctly"
      (ok (= (pos ks) 5)))))

(defun test-read-bytes-term-ascii-has-term-t-nil-t (ks)
  (progn
    (testing "test-read-bytes-term has term t nil t should return right answer"
      (ok (equalp (read-bytes-term ks "ascii" (code-char 32) t nil t)
		  #(#X48 #X65 #X6C #X6C #X6F #X20))))
    (testing "test-read-bytes-term has term t nil t should set position correctly"
      (ok (= (pos ks) 5)))))

(defun test-read-bytes-term-ascii-has-term-t-t-nil (ks)
  (progn
    (testing "test-read-bytes-term has term t t nil should return right answer"
      (ok (equalp (read-bytes-term ks "ascii" (code-char 32) t t nil)
		  #(#X48 #X65 #X6C #X6C #X6F #X20))))
    (testing "test-read-bytes-term has term t t nil should set position correctly"
      (ok (= (pos ks) 6)))))

(defun test-read-bytes-term-ascii-has-term-t-t-t (ks)
  (progn
    (testing "test-read-bytes-term t t t should return right answer"
      (ok (equalp (read-bytes-term ks "ascii" (code-char 32) t t t)
		  #(#X48 #X65 #X6C #X6C #X6F #X20))))
    (testing "test-read-bytes-term should nil t t should set position correctly"
      (ok (= (kaitai-stream:pos ks) 6)))))

;; Test unsupported and unknown encodings

(defun test-read-bytes-term-utf-8-has-term-t-t-t (ks)
  (handler-case
      (progn
	(read-bytes-term ks "utf-8" (code-char 32) t t t)
	;; We shouldn't get this far
	(testing
	    "test-read-bytes-term utf-8 has-term t t t should raise kaitai-stream-unsupported-encoding-error"
	  (ok nil)))
    (kaitai-stream:kaitai-stream-unsupported-encoding-error (e)
      (progn
	(testing "test-read-bytes-term utf-8 has-term t t t should raise kaitai-stream-unsupported-encoding-error"
	  (ok (string= "unsupported encoding: UTF-8"
		       (kaitai-stream:kaitai-stream-unsupported-encoding-error-text e))))
	(testing "test-read-bytes-term utf-8 has-term t t t should not change the stream"
	  (ok (= (pos ks) 0)))))))


(defun test-read-bytes-term-blargh-has-term-t-t-t (ks)
  (handler-case
      (progn
	(read-bytes-term ks "blargh" (code-char 32) t t t)
	;; We shouldn't get this far
	(testing
	    "test-read-bytes-term blargh has-term t t t should raise kaitai-stream-unsupported-encoding-error"
	  (ok nil)))
    (kaitai-stream:kaitai-stream-unsupported-encoding-error (e)
      (progn
	(testing "test-read-bytes-term blargh has-term t t t should raise kaitai-stream-unsupported-encoding-error"
	  (ok (string= "unsupported encoding: NIL"
		       (kaitai-stream:kaitai-stream-unsupported-encoding-error-text e))))
	(testing "test-read-bytes-term blargh has-term t t t should not change the stream"
	  (ok (= (pos ks) 0)))))))
