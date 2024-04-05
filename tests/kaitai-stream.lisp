;; Tests for kaitai-stream

(defpackage kaitai-struct-clisp-runtime/tests/kaitai-stream
  (:use :cl :kaitai-stream :rove))

(in-package :kaitai-struct-clisp-runtime/tests/kaitai-stream)

;; NOTE: To run this test file, execute
;; ` (asdf:test-system :kaitai-struct-clisp-runtime)' in your Lisp.

;; We should be able to get the tests directory from the test system,
;; but it wasn't working.
;; To use merge-pathnames, the second argument needs a trailing /
;; asdf:system-relative-pathname doesn't include a trailing /
;; There are other small annoyances.
;; Anyways, the tests work now.
;; (defvar *project-directory*
;;   (asdf:system-relative-pathname :kaitai-struct-clisp-runtime "tests"))
;; (defvar *tests-subdirectory* "tests/")
;; (print (format nil "test directory: ~a" *project-directory*))

;; Test helpers and setup / teardown
;; Test align-to-byte

(defun test-full-file (helper)
  "Build test streams and run a set of tests on a \"full\" file that
   contains \"Hello, World!\".

   Pass in a helper that actually runs the tests"
  (let* ((filename
	   (asdf:system-relative-pathname
	    "kaitai-struct-clisp-runtime/tests"
	    "tests/test-bytes-hello-world.bin"))
	 (in (open filename :direction :input :element-type '(unsigned-byte 8)))
	 (kfs (make-instance 'kaitai-file-stream :io in))
	 (kbs (make-instance 'kaitai-byte-stream
			     :data #(#X48 #X65 #X6C #X6C #X6F
				     #X20 #X57 #X6F #X72 #X6C #X64 #X21))))
    (funcall helper kfs)
    (funcall helper kbs)))

(defun test-empty-file (helper)
  "Build test streams and run a set of tests on an \"empty\" file that
   contains nothing and is size zero.

   Pass in a helper that actually runs the tests"
  (let* ((filename
	   (asdf:system-relative-pathname
	    "kaitai-struct-clisp-runtime/tests"
	    "tests/test-empty-file.bin"))
	 (in (open filename :direction :input :element-type '(unsigned-byte 8)))
	 (kfs (make-instance 'kaitai-file-stream :io in))
	 (kbs (make-instance 'kaitai-byte-stream :data #())))
    (funcall helper kfs)
    (funcall helper kbs)))

(defun test-small-file (helper)
  "Build test streams and run a set of tests on a small test file that
   contains three bytes.

   Pass in a helper that actually runs the tests"
  (let* ((filename
	   (asdf:system-relative-pathname
	    "kaitai-struct-clisp-runtime/tests"
	    "tests/test-bytes-01-03.bin"))
	 (in (open filename :direction :input :element-type '(unsigned-byte 8)))
	 (kfs (make-instance 'kaitai-file-stream :io in))
	 (kbs (make-instance 'kaitai-byte-stream :data #(#X01 #X02 #X03))))
    (funcall helper kfs)
    (funcall helper kbs)))

;; The tests

;; Test Stream positioning

(deftest test-kaitai-seek
  (test-small-file #'test-kaitai-stream-seek))

(defun test-kaitai-stream-seek (ks)
  (testing "test-kaitai-stream-seek should seek correctly"
    (ok (= (pos ks) 0))
    (seek ks 1)
    (ok (= (pos ks) 1))))

(deftest test-kaitai-pos
  (test-small-file #'test-kaitai-stream-pos))

(defun test-kaitai-stream-pos (ks)
  (testing "test-kaitai-stream-pos should return the correct position"
    (ok (= (pos ks) 0))
    (seek ks 1)
    (ok (= (pos ks) 1))))

(deftest test-kaitai-eof-p
  (test-small-file #'test-kaitai-stream-eof-p))

(defun test-kaitai-stream-eof-p (ks)
  (testing "kaitai-stream-eof-p should return true at end-of-file"
    (ng (eof-p ks))
    (seek ks 1)
    (ng (eof-p ks))
    (seek ks 3)
    (ok (eof-p ks))))

(deftest test-kaitai-empty-file-eof-p
  (test-empty-file #'test-kaitai-stream-empty-file-eof-p))

(defun test-kaitai-stream-empty-file-eof-p (ks)
  (testing "kaitai-stream-empty-file-eof-p should always return true"
    (ok (eof-p ks))
    (seek ks 0)
    (ok (eof-p ks))))

(deftest test-kaitai-size
  (test-small-file #'test-kaitai-stream-size))

(defun test-kaitai-stream-size (ks)
  (testing "kaitai-stream-size should return the correct size"
    (ok (= (size ks) 3))))

(deftest test-kaitai-empty-file-size
  (test-empty-file #'test-kaitai-stream-empty-file-size))

(defun test-kaitai-stream-empty-file-size (ks)
  (testing "kaitai-stream-empty-file-size should return zero"
    (ok (= (size ks) 0))))

;; Test Integer numbers

;; Test Signed

;; Test read-s1

(deftest read-s1-zero-positive
  (testing "read-s1 with 0x00 should work"
    (let ((ks (make-instance 'kaitai-byte-stream :data #(0))))
      (ok (= (read-s1 ks) 0)))))

(deftest read-s1-127-positive
  (testing "read-s1 with 0x7F should work"
    (let ((ks (make-instance 'kaitai-byte-stream :data #(#X7F))))
      (ok (= (read-s1 ks) #X7F)))))

(deftest read-s1-128-negative
  (testing "read-s1 with 0x80 should work"
    (let ((ks (make-instance 'kaitai-byte-stream :data #(#X80))))
      (ok (= (read-s1 ks) #X00)))))

(deftest read-s1-255-negative
  (testing "read-s1 with 0x80 should work"
    (let ((ks (make-instance 'kaitai-byte-stream :data #(#XFF))))
      (ok (= (read-s1 ks) (- #X7F))))))

;; Test Big-endian Signed

;; Test read-s2be

(deftest read-s2be
  (testing "read-s2be positive should work"
    (let ((ks (make-instance 'kaitai-byte-stream :data #(#X12 #X34))))
      (ok (= (read-s2be ks) #X1234)))))

(deftest read-s2be
  (testing "read-s2be negative should work"
    (let ((ks (make-instance 'kaitai-byte-stream :data #(#X82 #X34))))
      (ok (= (read-s2be ks) (- #X0234))))))

;; Test read-s4be

(deftest read-s4be
  (testing "read-s4be positive should work"
    (let ((ks (make-instance 'kaitai-byte-stream :data #(#X12 #X34 #X56 #X78))))
      (ok (= (read-s4be ks) #X12345678)))))

(deftest read-s4be
  (testing "read-s4be negative should work"
    (let ((ks (make-instance 'kaitai-byte-stream :data #(#X82 #X34 #X56 #X78))))
      (ok (= (read-s4be ks) (- #X02345678))))))

;; Test read-s8be

(deftest read-s8be
  (testing "read-s8be positive should work"
    (let ((ks (make-instance 'kaitai-byte-stream
			     :data #(#X12 #X34 #X56 #X78 #X12 #X34 #X56 #X78))))
      (ok (= (read-s8be ks) #X1234567812345678)))))

(deftest read-s8be
  (testing "read-s8be negative should work"
    (let ((ks (make-instance 'kaitai-byte-stream
			     :data #(#X82 #X34 #X56 #X78 #X12 #X34 #X56 #X78))))
      (ok (= (read-s8be ks) (- #X0234567812345678))))))


;; Test Little-endian Signed


;; Test read-s2le

(deftest read-s2le
  (testing "read-s2le positive should work"
    (let ((ks (make-instance 'kaitai-byte-stream :data #(#X34 #X12))))
      (ok (= (read-s2le ks) #X1234)))))

(deftest read-s2le
  (testing "read-s2le positive greater than 0x7FFF should work"
    (let ((ks (make-instance 'kaitai-byte-stream :data #(#X34 #X82))))
      (ok (= (read-s2le ks) (- #X0234))))))

;; Test read-s4le

(deftest read-s4le
  (testing "read-s4le positive should work"
    (let ((ks (make-instance 'kaitai-byte-stream :data #(#X78 #X56 #X34 #X12))))
      (ok (= (read-s4le ks) #X12345678)))))

(deftest read-s4le
  (testing "read-s4le negative greater than 0x7FFFFFFF should work"
    (let ((ks (make-instance 'kaitai-byte-stream :data #(#X78 #X56 #X34 #X82))))
      (ok (= (read-s4le ks) (- #X02345678))))))

;; Test read-s8le

(deftest read-s8le
  (testing "read-s8le positive should work"
    (let ((ks (make-instance 'kaitai-byte-stream
			     :data #(#X78 #X56 #X34 #X12 #X78 #X56 #X34 #X12))))
      (ok (= (read-s8le ks) #X1234567812345678)))))

(deftest read-s8le
  (testing "read-s8le negative greater than 0x7FFFFFFFFFFFFFFF should work"
    (let ((ks (make-instance 'kaitai-byte-stream
			     :data #(#X78 #X56 #X34 #X12 #X78 #X56 #X34 #X82))))
      (ok (= (read-s8le ks) (- #X0234567812345678))))))


;; Test Unsigned

;; Test read-u1

(deftest read-u1-zero-positive
  (testing "read-u1 with 0x00 should work"
    (let ((ks (make-instance 'kaitai-byte-stream :data #(0))))
      (ok (= (read-u1 ks) 0)))))

(deftest read-u1-127-positive
  (testing "read-u1 with 0x7F should work"
    (let ((ks (make-instance 'kaitai-byte-stream :data #(#X7F))))
      (ok (= (read-u1 ks) #X7F)))))

(deftest read-u1-128-negative
  (testing "read-u1 with 0x80 should work"
    (let ((ks (make-instance 'kaitai-byte-stream :data #(#X80))))
      (ok (= (read-u1 ks) #X80)))))

(deftest read-u1-255-negative
  (testing "read-u1 with 0x80 should work"
    (let ((ks (make-instance 'kaitai-byte-stream :data #(#XFF))))
      (ok (= (read-u1 ks) #XFF)))))


;; Test Big-endian Unsigned

;; Test read-u2be

(deftest read-u2be
  (testing "read-u2be positive should work"
    (let ((ks (make-instance 'kaitai-byte-stream :data #(#X12 #X34))))
      (ok (= (read-u2be ks) #X1234)))))

(deftest read-u2be
  (testing "read-u2be positive greater than 0x7FFF should work"
    (let ((ks (make-instance 'kaitai-byte-stream :data #(#X82 #X34))))
      (ok (= (read-u2be ks) #X8234)))))

;; Test read-u4be

(deftest read-u4be
  (testing "read-u4be positive should work"
    (let ((ks (make-instance 'kaitai-byte-stream :data #(#X12 #X34 #X56 #X78))))
      (ok (= (read-u4be ks) #X12345678)))))

(deftest read-u4be
  (testing "read-u4be negative greater than 0x7FFFFFFF should work"
    (let ((ks (make-instance 'kaitai-byte-stream :data #(#X82 #X34 #X56 #X78))))
      (ok (= (read-u4be ks) #X82345678)))))

;; Test read-u8be

(deftest read-u8be
  (testing "read-u8be positive should work"
    (let ((ks (make-instance 'kaitai-byte-stream
			     :data #(#X12 #X34 #X56 #X78 #X12 #X34 #X56 #X78))))
      (ok (= (read-u8be ks) #X1234567812345678)))))

(deftest read-u8be
  (testing "read-u8be negative greater than 0x7FFFFFFFFFFFFFFF should work"
    (let ((ks (make-instance 'kaitai-byte-stream
			     :data #(#X82 #X34 #X56 #X78 #X12 #X34 #X56 #X78))))
      (ok (= (read-u8be ks) #X8234567812345678)))))


;; Test Little-endian Unsigned

;; Test read-u2le

(deftest read-u2le
  (testing "read-u2le positive should work"
    (let ((ks (make-instance 'kaitai-byte-stream :data #(#X34 #X12))))
      (ok (= (read-u2le ks) #X1234)))))

(deftest read-u2le
  (testing "read-u2le positive greater than 0x7FFF should work"
    (let ((ks (make-instance 'kaitai-byte-stream :data #(#X34 #X82))))
      (ok (= (read-u2le ks) #X8234)))))

;; Test read-u4le

(deftest read-u4le
  (testing "read-u4le positive should work"
    (let ((ks (make-instance 'kaitai-byte-stream :data #(#X78 #X56 #X34 #X12))))
      (ok (= (read-u4le ks) #X12345678)))))

(deftest read-u4le
  (testing "read-u4le negative greater than 0x7FFFFFFF should work"
    (let ((ks (make-instance 'kaitai-byte-stream :data #(#X78 #X56 #X34 #X82))))
      (ok (= (read-u4le ks) #X82345678)))))

;; Test read-u8le

(deftest read-u8le
  (testing "read-u8le positive should work"
    (let ((ks (make-instance 'kaitai-byte-stream
			     :data #(#X78 #X56 #X34 #X12 #X78 #X56 #X34 #X12))))
      (ok (= (read-u8le ks) #X1234567812345678)))))

(deftest read-u8le
  (testing "read-u8le negative greater than 0x7FFFFFFFFFFFFFFF should work"
    (let ((ks (make-instance 'kaitai-byte-stream
			     :data #(#X78 #X56 #X34 #X12 #X78 #X56 #X34 #X82))))
      (ok (= (read-u8le ks) #X8234567812345678)))))


;; Test Floating point numbers

;; TODO: Add tests for:
;;       -0, +0, -inf, +inf, -NaN, +NaN, negative, positive, first, last, largest numbers, smallest numbers

;; Floating point is disabled for now
;; It can be enabled in kaitai-struct-clisp-runtime.asd
;;
;; Individual floating point tests are feature-gated based on the
;; Common LISP implementation and the floating-point feature flag.

;; Test Big-endian Floating point numbers

(deftest read-f4be-zero
  #+(and floating-point sbcl) (testing "read-f4be zero should work"
				(let ((ks (make-instance 'kaitai-byte-stream
							 :data #(#X00 #X00 #X00 #X00))))
				  (ok (= (read-f4be ks) 0.0))))
  #-(or floating-point) (testing "read-f4be zero isn't implemented"
			  (ok t)))

(deftest read-f4be-nan-one
  #+(and floating-point sbcl) (testing "read-f4be NaN should work"
				(let ((ks (make-instance 'kaitai-byte-stream
							 :data #(#X7F #XFF #XFF #XFF))))
				  (ok (sb-ext:float-nan-p (read-f4be ks)))))
  #-(or floating-point) (testing "read-f4be NaN isn't implemented"
			  (ok t)))

(deftest read-f4be-pos-inf
  #+(and floating-point sbcl) (testing "read-f4be +inf should work"
				(let ((ks (make-instance 'kaitai-byte-stream
							 :data #(#X7F #X80 #X00 #X00))))
				  (ok (= (read-f4be ks)
					 sb-ext:single-float-positive-infinity))))
  #-(or floating-point) (testing "read-f4be +inf isn't implemented"
			  (ok t)))

;; (deftest read-f4be-neg-inf
;;   (testing "read-f4be -inf should work"
;;     (let ((ks (make-instance 'kaitai-byte-stream :data #(#XFF #X80 #X00 #X00))))
;;       (ok (= (read-f4be ks) sb-ext:single-float-negative-infinity)))))

(deftest read-f4be
  #+(and floating-point sbcl) (testing "read-f4be no exponent should work"
				(let ((ks (make-instance 'kaitai-byte-stream
							 :data #(#X3B #X5D #X2D #X08))))
				  (ok (= (read-f4be ks) 0.0033748765))))
  #-(or floating-point) (testing "read-f4be -inf not implemented"
			  (ok t)))


;; Test Big-endian Double Floating point numbers

(deftest read-f8be-zero
  #+(and floating-point sbcl) (testing "read-f8be zero should work"
    (let ((ks (make-instance 'kaitai-byte-stream
			     :data #(#X00 #X00 #X00 #X00 #X00 #X00 #X00 #X00))))
      (ok (= (read-f8be ks) 0.0))))
  #-(or floating-point) (testing "read-f8be zero not implemented"
			  (ok t)))

;; Uses functions specific to SBCL
(deftest read-f8be-nan-one
  #+(and floating-point sbcl) (testing "read-f8be NaN should work"
    (let ((ks (make-instance 'kaitai-byte-stream
			     :data #(#X7F #XFF #XFF #XFF #XFF #XFF #XFF #XFF))))
      (ok (sb-ext:float-nan-p (read-f8be ks)))))
  #-(or floating-point) (testing "read-f8be NaN not implemented"
			  (ok t)))

(deftest read-f8be-pos-inf
  #+(and floating-point sbcl) (testing "read-f8be +inf should work"
				(let ((ks (make-instance 'kaitai-byte-stream
			     :data #(#X7F #XF0 #X00 #X00 #X00 #X00 #X00 #X00))))
      (ok (= (read-f8be ks) sb-ext:double-float-positive-infinity))))
  #-(or floating-point) (testing "read-f8be +inf not implemented"
			  (ok t)))

(deftest read-f8be-neg-inf
  #+(and floating-point sbcl) (testing "read-f8be -inf should work"
    (let ((ks (make-instance 'kaitai-byte-stream
			     :data #(#XFF #XF0 #X00 #X00 #X00 #X00 #X00 #X00))))
      (ok (= (read-f8be ks) sb-ext:double-float-negative-infinity))))
  #-(or floating-point) (testing "read-f8be +inf not implemented"
			  (ok t)))

(deftest read-f8be
  #+(and floating-point sbcl) (testing "read-f8be no exponent should work"
    (let ((ks (make-instance 'kaitai-byte-stream
			     :data #(#X3F #XDD #X2D #X08  #X3B #X5D #X2D #X08))))
      (ok (= (read-f8be ks) 0.4558735446668867d0))))
  #-(or floating-point) (testing "read-f8be no exponent not implemented"
			  (ok t)))


;; Test Little-endian Floating point numbers

(deftest read-f4le-zero
  #+(and floating-point sbcl) (testing "read-f4le zero should work"
    (let ((ks (make-instance 'kaitai-byte-stream :data #(#X00 #X00 #X00 #X00))))
      (ok (= (read-f4le ks) 0.0))))
  #-(or floating-point) (testing "read-f4le zero not implemented"
			  (ok t)))

;; Uses functions specific to SBCL
(deftest read-f4le-nan-one
  #+(and floating-point sbcl) (testing "read-f4le NaN should work"
    (let ((ks (make-instance 'kaitai-byte-stream :data #(#XFF #XFF #XFF #X7F))))
      (ok (sb-ext:float-nan-p (read-f4le ks)))))
;;      (ok (equal (read-f4le ks) '#<SINGLE-FLOAT quiet NaN>)))
  #-(or floating-point) (testing "read-f4le NaN not implemented"
			  (ok t)))

(deftest read-f4le-pos-inf
  #+(and floating-point sbcl) (testing "read-f4le +inf should work"
    (let ((ks (make-instance 'kaitai-byte-stream :data #(#X00 #X00 #X80 #X7F))))
      (ok (= (read-f4le ks) sb-ext:single-float-positive-infinity))))
  #-(or floating-point) (testing "read-f4le +inf not implemented"
			  (ok t)))

(deftest read-f4le-neg-inf
  #+(and floating-point sbcl) (testing "read-f4le -inf should work"
    (let ((ks (make-instance 'kaitai-byte-stream :data #(#X00 #X00 #X80 #XFF))))
      (ok (= (read-f4le ks) sb-ext:single-float-negative-infinity))))
  #-(or floating-point) (testing "read-f4le -inf not implemented"
			  (ok t)))

(deftest read-f4le
  #+(and floating-point sbcl) (testing "read-f4le no exponent should work"
    (let ((ks (make-instance 'kaitai-byte-stream :data #(#X08 #X2D #X5D #X3B))))
      (ok (= (read-f4le ks) 0.0033748765))))
  #-(or floating-point) (testing "read-f4le no exponent not implemented"
			  (ok t)))

;; Test Little-endian Double Floating point numbers

(deftest read-f8le-zero
  #+(and floating-point sbcl) (testing "read-f8le zero should work"
    (let ((ks (make-instance 'kaitai-byte-stream
			     :data #(#X00 #X00 #X00 #X00 #X00 #X00 #X00 #X00))))
      (ok (= (read-f8le ks) 0.0))))
  #-(or floating-point) (testing "read-f8le zero not implemented"
			  (ok t)))

;; Uses functions specific to SBCL
(deftest read-f8le-nan-one
  #+(and floating-point sbcl) (testing "read-f8le NaN should work"
    (let ((ks (make-instance 'kaitai-byte-stream
			     :data #(#XFF #XFF #XFF #XFF #XFF #XFF #XFF #X7F))))
      (ok (sb-ext:float-nan-p (read-f8le ks)))))
  #-(or floating-point) (testing "read-f8le NaN not implemented"
			  (ok t)))

(deftest read-f8le-pos-inf
  #+(and floating-point sbcl) (testing "read-f8le +inf should work"
    (let ((ks (make-instance 'kaitai-byte-stream
			     :data #(#X00 #X00 #X00 #X00 #X00 #X00 #XF0 #X7F))))
      (ok (= (read-f8le ks) sb-ext:double-float-positive-infinity))))
  #-(or floating-point) (testing "read-f8le +inf not implemented"
			  (ok t)))

(deftest read-f8le-neg-inf
  #+(and floating-point sbcl) (testing "read-f8le -inf should work"
				(let ((ks (make-instance 'kaitai-byte-stream
							 :data #(#X00 #X00 #X00 #X00 #X00 #X00 #XF0 #XFF))))
				  (ok (= (read-f8le ks) sb-ext:double-float-negative-infinity))))
  #-(or floating-point) (testing "read-f8le -inf not implemented"
			  (ok t)))


(deftest read-f8le
  #+(and floating-point sbcl) (testing "read-f8le no exponent should work"
    (let ((ks (make-instance 'kaitai-byte-stream
			     :data #(#X08 #X2D #X5D #X3B #X08 #X2D #XDD #X3F))))
      (ok (= (read-f8le ks) 0.4558735446668867d0))))
  #-(or floating-point) (testing "read-f8le no exponent not implemented"
			  (ok t)))

;; Test Unaligned bit values

;; Test align-to-byte

(deftest test-align-to-byte
  (test-full-file #'test-align-to-byte-helper))

(defun test-align-to-byte-helper (ks)
  (handler-case
      (progn
	(align-to-byte ks)
	(testing "test-align-to-byte-helper should fail"
	  (ok nil)))
    (kaitai-stream-not-implemented-error (e)
      (testing (format nil "test-align-to-byte-helper should fail: ~a"
		       (kaitai-stream-not-implemented-error-text e))
	(ok t)))))

;; Test read-bits-int

(deftest test-read-bits-int
  (test-full-file #'test-read-bits-int-helper))

(defun test-read-bits-int-helper (ks)
  (handler-case
      (progn
	(read-bits-int ks 0)
	(testing "test-read-bits-int-helper should fail"
	  (ok nil)))
    (kaitai-stream-not-implemented-error (e)
      (testing (format nil "test-read-bits-int-helper should fail: ~a"
		       (kaitai-stream-not-implemented-error-text e))
	(ok t)))))

;; Test read-bits-array

(deftest test-read-bits-array
  (test-full-file #'test-read-bits-array-helper))

(defun test-read-bits-array-helper (ks)
  (handler-case
      (progn
	(read-bits-array ks 0)
	(testing "test-read-bits-array-helper should fail"
	  (ok nil)))
    (kaitai-stream-not-implemented-error (e)
      (testing
	  (format nil "test-read-bits-array-helper should fail: ~a"
		       (kaitai-stream-not-implemented-error-text e))
	(ok t)))))

;; Test Byte arrays

;; Test reading in the entire stream of length n using read-bytes n
(deftest test-read-bytes-all
  (test-full-file #'test-read-bytes-all-helper))

(defun test-read-bytes-all-helper (ks)
    (let ((buf (read-bytes ks 12)))
      (testing "test-read-bytes-all should read all bytes"
	(ng
	 (mismatch buf #(#X48 #X65 #X6C #X6C #X6F #X20 #X57 #X6F #X72 #X6C #X64 #X21))))
    (testing "test-read-bytes-all should end up in the correct position"
      (ok (= (pos ks) 12)))))

;; Test reading in some of the stream using read-bytes n
(deftest test-read-bytes-partial
  (test-full-file #'test-read-bytes-partial-helper))

(defun test-read-bytes-partial-helper (ks)
  (let ((buf (read-bytes ks 5)))
    (testing "test-read-bytes-partial should read in n bytes"
      (ng (mismatch buf #(#X48 #X65 #X6C #X6C #X6F))))
    (testing "test-read-bytes-partial should end up in the correct position"
      (ok (= (pos ks) 5)))))

;; Test reading in zero bytes of the stream using read-bytes 0
(deftest test-read-bytes-zero
  (test-full-file #'test-read-bytes-zero-helper))

(defun test-read-bytes-zero-helper (ks)
  (let ((buf (read-bytes ks 0)))
    (testing "test-read-bytes-zero should read in zero bytes"
      (ng (mismatch buf #())))
    (testing "test-read-bytes-zero all should end up in the correct position"
      (ok (= (pos ks) 0)))))


;; Test reading in more than the bytes available using read-bytes 13
(deftest test-read-bytes-beyond
  (test-full-file #'test-read-bytes-beyond-helper))

(defun test-read-bytes-beyond-helper (ks)
  (handler-case
      (let ((buf (read-bytes ks 13)))
	(testing "test-read-bytes-beyond should fail"
	  (ng (mismatch buf #())))
	(testing "test-read-bytes-beyond should ?"
	  (ok (= (pos ks) 0))))
    (kaitai-stream-eof-error (e)
      (testing (format nil "reading beyond should throw error: ~a"
		       (kaitai-stream-eof-error-text e))
	(ok t)))))

;; Test reading in all bytes from a file
(deftest read-bytes-full
  (test-full-file #'test-read-bytes-full-helper))

(defun test-read-bytes-full-helper (ks)
  (let ((buf (read-bytes-full ks)))
    (testing "test-read-bytes-full should read in all bytes"
      (ng
       (mismatch buf #(#X48 #X65 #X6C #X6C #X6F #X20 #X57 #X6F #X72 #X6C #X64 #X21))))
    (testing "test-read-bytes-full all should end up in the correct position"
      (ok (= (pos ks) 12)))))

;; Test reading in all bytes from an empty file
(deftest read-bytes-full-empty
  (test-empty-file #'test-read-bytes-full-empty-helper))

(defun test-read-bytes-full-empty-helper (ks)
  (let ((buf (read-bytes-full ks)))
    (testing "test-read-bytes-full-empty should succeed"
      (ng (mismatch buf #())))
    (testing "test-read-bytes-full-empty should end up in the correct position"
      (ok (= (pos ks) 0)))))

;; Test reading in a all bytes first, then try to read in all bytes again
(deftest read-bytes-full-eof
  (test-full-file #'test-read-bytes-full-eof-helper))

(defun test-read-bytes-full-eof-helper (ks)
  (let* ((first-buf (read-bytes-full ks))
	 (second-buf (read-bytes-full ks)))
    (testing "test-read-bytes-full-eof-helper first read should return full vector"
      (ng
       (mismatch first-buf #(#X48 #X65 #X6C #X6C #X6F #X20 #X57 #X6F #X72 #X6C #X64 #X21))))
    (testing "test-read-bytes-full-eof-helper second read should return empty vector"
      (ng (mismatch second-buf #())))
    (testing "test-read-bytes-full-empty should end up in the correct position"
      (ok (= (pos ks) 12)))))

(deftest read-bytes-term
  (test-full-file #'test-read-bytes-term-helper))

(defun test-read-bytes-term-helper (ks)
  (handler-case
      (progn
	(read-bytes-term ks "utf-8" " " t t t)
	(testing "test-read-bytes-term should fail"
	  (ok nil)))
    (kaitai-stream-not-implemented-error (e)
      (testing (format nil "read-bytes-term-helper should fail: ~a"
		       (kaitai-stream-not-implemented-error-text e))
	(ok t)))))


;; Test ensure-fixed-contents


;; This tests that the next n bytes match some expected n bytes

(deftest ensure-fixed-contents-matches-works
  (test-full-file #'test-ensure-fixed-contents-matches-helper))

(defun test-ensure-fixed-contents-matches-helper (ks)
  (let ((actual (ensure-fixed-contents
		 ks
		 #(#X48 #X65 #X6C #X6C #X6F #X20 #X57 #X6F #X72 #X6C #X64 #X21))))
    (testing "test-ensure-fixed-contents should succeed"
      (ng (mismatch actual #(#X48 #X65 #X6C #X6C #X6F #X20 #X57 #X6F #X72 #X6C #X64 #X21))))))


;; This tests that the next n bytes match some expected n bytes
;; This tests the case where we read some bytes first, then call ensure-fixed-contents

(deftest ensure-fixed-contents-after-read-matches-works
  (test-full-file #'test-ensure-fixed-contents-after-read-matches-helper))

(defun test-ensure-fixed-contents-after-read-matches-helper (ks)
  (progn
    (read-bytes ks 2)
    (let ((actual (ensure-fixed-contents
		   ks
		   #(#X6C #X6C #X6F #X20 #X57 #X6F #X72 #X6C #X64 #X21))))
      (testing "test-ensure-fixed-contents after a partial read should succeed"
	(ng (mismatch actual #(#X6C #X6C #X6F #X20 #X57 #X6F #X72 #X6C #X64 #X21)))))))


;; This tests that the next n bytes match some expected n bytes
;; If they don't match, an error should be thrown

(deftest ensure-fixed-contents-doesnt-match-throws
  (test-full-file #'test-ensure-fixed-contents-doesnt-match-helper))

(defun test-ensure-fixed-contents-doesnt-match-helper (ks)
  (handler-case
      (let ((actual (ensure-fixed-contents
		     ks
		     #(#X48 #X65 #X6C #X6C #X6F #X20 #X56 #X6F #X72 #X6C #X64 #X21))))
	(testing "test-ensure-fixed-contents with wrong expected should not succeed"
	  (ok nil)))
    (kaitai-stream-validation-not-equal-error (e)
      (testing (format nil "test-ensure-fixed-contents with wrong expected should throw error: ~a"
		       (kaitai-stream-validation-not-equal-error-text e))
	(ok t)))))

;; This tests that the next n bytes match some expected n bytes
;; If an end-of-file is reached before matching, it should also throw an
;; kaitai-stream-validation-not-equal-error

(deftest ensure-fixed-contents-eof-throws
  (test-full-file #'test-ensure-fixed-contents-eof-helper))

(defun test-ensure-fixed-contents-eof-helper (ks)
  (handler-case
      (let ((actual (ensure-fixed-contents
		     ks
		     #(#X48 #X65 #X6C #X6C #X6F #X20 #X57 #X6F #X72 #X6C #X64 #X21 #XD3))))
	(testing "test-ensure-fixed-contents with too long expected should not succeed"
	  (ok nil)))
    (kaitai-stream-validation-not-equal-error (e)
      (testing (format nil "test-ensure-fixed-contents with too long expected should throw error: ~a"
		       (kaitai-stream-validation-not-equal-error-text e))
	(ok t)))))


;; This tests that the next n bytes match some expected n bytes
;; This tests the case where we read some bytes into the stream first
;;
;; If an end-of-file is reached before matching, it should also throw an
;; kaitai-stream-validation-not-equal-error

(deftest ensure-fixed-contents-after-read-eof-throws
  (test-full-file #'test-ensure-fixed-contents-after-read-eof-helper))

(defun test-ensure-fixed-contents-after-read-eof-helper (ks)
  (progn
    (read-bytes ks 2)
    (handler-case
	(let ((actual (ensure-fixed-contents
		       ks
		       #(#X6C #X6C #X6F #X20 #X57 #X6F #X72 #X6C #X64 #X21 #XD3))))
	  (testing "test-ensure-fixed-contents after a partial read with too long expected should not succeed"
	    (ok nil)))
      (kaitai-stream-validation-not-equal-error (e)
	(testing (format nil "test-ensure-fixed-contents after a partial read with too long expected should throw error: ~a"
			 (kaitai-stream-validation-not-equal-error-text e))
	  (ok t))))))


;; Test bytes-strip-right

;; Test for no occurrences at end of stream
;; It should return the original byte vector
(deftest test-bytes-strip-right-no-matches
    (test-full-file #'test-bytes-strip-right-no-matches-helper))

(defun test-bytes-strip-right-no-matches-helper (ks)
  (handler-case
      (progn
	(let ((res (bytes-strip-right (read-bytes-full ks) #X41)))
	  (testing "test-bytes-strip-right with no matches should return the original bytes"
	    (ng
	     (mismatch res #(#X48 #X65 #X6C #X6C #X6F #X20 #X57 #X6F #X72 #X6C #X64 #X21))))))))

;; Test for one occurrence at end of stream
;; It should return the byte vector minus the last byte
(deftest test-bytes-strip-right
    (test-full-file #'test-bytes-strip-right-helper))

(defun test-bytes-strip-right-helper (ks)
  (handler-case
      (progn
	(let ((res (bytes-strip-right (read-bytes-full ks) #X21)))
	  (testing "test-bytes-strip-right with one pad character should strip it"
	    (ng
	     (mismatch res #(#X48 #X65 #X6C #X6C #X6F #X20 #X57 #X6F #X72 #X6C #X64))))))))


;; Test for two occurences at end of stream
;; It should return the byte sequence minus the last two bytes
(deftest test-bytes-strip-right-two-matches
  (let ((ks (make-instance 'kaitai-byte-stream
			   :data #(#X48 #X65 #X6C #X6C #X6F
				   #X20 #X57 #X6F #X72 #X6C #X64 #X21 #X21))))
  (handler-case
      (progn
	(let ((res (bytes-strip-right (read-bytes-full ks) #X21)))
	  (testing "test-bytes-strip-right with two pad characters should strip them both"
	    (ng
	     (mismatch res #(#X48 #X65 #X6C #X6C #X6F #X20 #X57 #X6F #X72 #X6C #X64)))))))))

;; Test for one occurence at start of stream
;; It should return the original byte vector
(deftest test-bytes-strip-right-matches-left
    (test-full-file #'test-bytes-strip-right-matches-left-helper))

(defun test-bytes-strip-right-matches-left-helper (ks)
  (handler-case
      (progn
	(let ((res (bytes-strip-right (read-bytes-full ks) #X48)))
	  (testing "test-bytes-strip-right with one pad character at the beginning (with following bytes) should ignore it"
	    (ng
	     (mismatch res #(#X48 #X65 #X6C #X6C #X6F #X20 #X57 #X6F #X72 #X6C #X64 #X21))))))))


;; Test with an empty stream
;; It should return the original byte sequence (an empty vector)
(deftest test-bytes-strip-right-empty-file
    (test-empty-file #'test-bytes-strip-right-empty-file-helper))

(defun test-bytes-strip-right-empty-file-helper (ks)
  (handler-case
      (progn
	(let ((res (bytes-strip-right (read-bytes-full ks) #X48)))
	  (testing "test-bytes-strip-right with an empty byte stream should return an empty byte stream"
	    (ng (mismatch res #())))))))


;; Test with matching on one byte on a one byte length stream
;; It should return an empty vector.
(deftest test-bytes-strip-right-one-byte-file
  (let ((ks (make-instance 'kaitai-byte-stream
			   :data #(#X48))))
    (handler-case
	(progn
	  (let ((res (bytes-strip-right (read-bytes-full ks) #X48)))
	    (testing "test-bytes-strip-right with two pad characters should strip them both"
	      (ng
	       (mismatch res #()))))))))


;; Test with matching on one byte on a one byte length stream
;; It should return an the original bytes.
(deftest test-bytes-strip-right-one-byte-no-match-file
  (let ((ks (make-instance 'kaitai-byte-stream
			   :data #(#X48))))
    (handler-case
	(progn
	  (let ((res (bytes-strip-right (read-bytes-full ks) #X21)))
	    (testing "test-bytes-strip-right with two pad characters should strip them both"
	      (ng
	       (mismatch res #(#X48)))))))))



;; Test bytes-terminate

(deftest test-bytes-terminate
    (handler-case
	(progn
	  (testing "empty byte vector not including term should return empty byte vector"
	    (ng (mismatch (bytes-terminate #() 0 nil) #())))
	  (testing "empty byte vector including term should return empty byte vector"
	    (ng (mismatch (bytes-terminate #() 0 t) #())))
	  (testing "one byte vector without match not including term should return original byte vector"
	    (ng (mismatch (bytes-terminate #(#X68) #X67 nil) #(#X68))))
	  (testing "one byte vector with match not including term should return empty byte vector"
	    (ng (mismatch (bytes-terminate #(#X68) #X68 nil) #())))
	  (testing "one byte vector without match including term should return original byte vector"
	    (ng (mismatch (bytes-terminate #(#X68) #X67 t) #(#X68))))
	  (testing "one byte vector with match including term should return match byte in byte vector"
	    (ng (mismatch (bytes-terminate #(#X68) #X68 t) #(#X68))))
	  (testing "multi byte vector with match not including term should return up to but not including match byte"
	    (ng (mismatch
		 (bytes-terminate #(#X68 #X65 #X6C #X6C #X6F) #X6C nil)
		 #(#X68 #X65))))
	  (testing "multi byte vector with match including term should return up to and including match byte"
	    (ng (mismatch
		 (bytes-terminate #(#X68 #X65 #X6C #X6C #X6F) #X6C t)
		 #(#X68 #X65 #X6C))))
	  (testing "multi byte vector without match not including term should return original byte vector"
	    (ng (mismatch
		 (bytes-terminate #(#X68 #X65 #X6C #X6C #X6F) #X6D nil)
		 #(#X68 #X65 #X6C #X6C #X6F))))
	  (testing "multi byte vector without match including term should return original byte vector"
	    (ng (mismatch
		 (bytes-terminate #(#X68 #X65 #X6C #X6C #X6F) #X6D t)
		 #(#X68 #X65 #X6C #X6C #X6F))))
	  (testing "multi byte vector with match on first byte not including term should return empty byte vector"
	    (ng (mismatch
		 (bytes-terminate #(#X68 #X65 #X6C #X6C #X6F) #X68 nil)
		 #())))
	  (testing "multi byte vector with match on first byte including term should return first byte"
	    (ng (mismatch
		 (bytes-terminate #(#X68 #X65 #X6C #X6C #X6F) #X68 t)
		 #(#X68))))
	  (testing "multi byte vector with match on last byte not including term should return up to but not including the last byte"
	    (ng (mismatch
		 (bytes-terminate #(#X68 #X65 #X6C #X6C #X6F) #X6F nil)
		 #(#X68 #X65 #X6C #X6C))))
	  (testing "multi byte vector with match on last byte including term should return entire byte vector"
	    (ng (mismatch
		 (bytes-terminate #(#X68 #X65 #X6C #X6C #X6F) #X6F t)
		 #(#X68 #X65 #X6C #X6C #X6F)))))))

;; Test bytes-to-str

(deftest test-bytes-to-str
  (handler-case
      (progn
	(bytes-to-str #() "utf-8")
	(testing "test-bytes-to-str should fail"
	  (ok nil)))
    (kaitai-stream-not-implemented-error (e)
      (testing (format nil "test-bytes-to-str should fail: ~a"
		       (kaitai-stream-not-implemented-error-text e))
	(ok t)))))

;; Test Byte array processing

;; Test Byte array processing - process-xor

(deftest test-process-xor
  (handler-case
      (progn
	(process-xor (make-instance 'kaitai-stream) #() 0)
	(testing "test-process-xor should fail"
	  (ok nil)))
    (kaitai-stream-not-implemented-error (e)
      (testing (format nil "test-process-xor should fail: ~a"
		       (kaitai-stream-not-implemented-error-text e))
	(ok t)))))

;; Test Byte array processing - process-xor-one

(deftest test-process-xor-one
  (handler-case
      (progn
	(process-xor-one (make-instance 'kaitai-stream) #() 0)
	(testing "test-process-xor-one should fail"
	  (ok nil)))
    (kaitai-stream-not-implemented-error (e)
      (testing (format nil "test-process-xor-one should fail: ~a"
		       (kaitai-stream-not-implemented-error-text e))
	(ok t)))))

;; Test Byte array processing - process-xor-many

(deftest test-process-xor-many
  (handler-case
      (progn
	(process-xor-many (make-instance 'kaitai-stream) #() 0)
	(testing "test-process-xor-many should fail"
	  (ok nil)))
    (kaitai-stream-not-implemented-error (e)
      (testing (format nil "test-process-xor-many should fail: ~a"
		       (kaitai-stream-not-implemented-error-text e))
	(ok t)))))

;; Test Byte array processing - process-rotate-left

(deftest test-process-rotate-left
  (handler-case
      (progn
	(process-rotate-left (make-instance 'kaitai-stream) #() 0 0)
      (testing "test-process-rotate-left should fail"
	(ok nil)))
    (kaitai-stream-not-implemented-error (e)
      (testing (format nil "test-process-rotate-left should fail: ~a"
		       (kaitai-stream-not-implemented-error-text e))
	(ok t)))))

;; Test Byte array processing - process-zlib
(deftest test-process-zlib
  (handler-case
      (progn
	(process-zlib (make-instance 'kaitai-stream) #())
	(testing "test-process-zlib should fail"
	  (ok nil)))
    (kaitai-stream-not-implemented-error (e)
      (testing (format nil "test-process-zlib should fail: ~a"
		       (kaitai-stream-not-implemented-error-text e))
	(ok t)))))

;; Misc runtime operations

;; Test mod

;; Behavior of mod isn't standard between runtimes

;; The Python runtime doesn't provide a custom implementation of mod,
;; instead using
(deftest test-kaitai-stream-mod
  (handler-case
      (progn
	(kaitai-stream-mod (make-instance 'kaitai-stream) #())
	(testing "test-kaitai-stream-mod should fail"
	  (ok nil)))
    (kaitai-stream-not-implemented-error (e)
      (testing (format nil "test-kaitai-stream-mod should fail: ~a"
		       (kaitai-stream-not-implemented-error-text e))
	(ok t)))))
