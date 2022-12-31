;; Tests for kaitai-struct

(defpackage kaitai-struct-clisp-runtime/tests/kaitai-struct
  (:use :cl :kaitai-struct :kaitai-stream :rove))

(in-package :kaitai-struct-clisp-runtime/tests/kaitai-struct)

;; A simple test kaitai-struct subclass we can use
(defclass fake-image (kaitai-struct)
  ((image-width :type (integer * *) :initarg :image-width :initform 0)
   (image-height :type (integer * *) :initarg :image-height :initform 0)
   ;; simple example of a composite attribute,
   ;; for testing of using other attributes to build an attribute.
   (image-size :type (integer * *) :initarg :image-size :initform 0)))


;; Define a print-object method so the print function will print our
;; object
(defmethod print-object ((object fake-image) stream)
  (call-next-method)
  (format stream ", ")
  (print-unreadable-object (object stream :type t)
    (with-slots (image-width image-height image-size) object
      (format stream ":image-width ~d :image-height ~d :image-size ~d"
	      image-width image-height image-size)))
  ;; emit a newline
  (format stream "~%"))

(defmethod init ((ks kaitai-struct))
  (progn
    (register-attribute ks 'image-width
			#'(lambda () (read-u2le (slot-value ks 'kaitai-struct::ks))))
    (register-attribute ks 'image-height
			#'(lambda () (read-u2le (slot-value ks 'kaitai-struct::ks))))))

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
	 (kbs (make-instance 'kaitai-byte-stream
			     :data #(#X01 #X02 #X03)))
	 (kf-struct (make-instance 'fake-image :stream kfs))
	 (kb-struct (make-instance 'fake-image :stream kbs)))
    (funcall helper kf-struct)
    (funcall helper kb-struct)))


(defun test-bytes-01-08-file (helper)
  "Build test streams and run a set of tests on an eight-byte file
   containing 0x01-0x08"
  (let* ((filename
	   (asdf:system-relative-pathname
	    "kaitai-struct-clisp-runtime/tests"
	    "tests/test-bytes-01-08.bin"))
	 (in (open filename :direction :input :element-type '(unsigned-byte 8)))
	 (kfs (make-instance 'kaitai-file-stream :io in))
	 (kbs (make-instance 'kaitai-byte-stream
			     :data #(#X01 #X02 #X03 #X04 #X05 #X06 #X07 #X08)))
	 (kf-struct (make-instance 'fake-image :stream kfs))
	 (kb-struct (make-instance 'fake-image :stream kbs)))
    (funcall helper kf-struct)
    (funcall helper kb-struct)))

(defun test-bytes-hello-world-file (helper)
  "Build test streams and run a set of tests on an eight-byte file
   containing Hello, World!"
  (let* ((filename
	   (asdf:system-relative-pathname
	    "kaitai-struct-clisp-runtime/tests"
	    "tests/test-bytes-hello-world.bin"))
	 (in (open filename :direction :input :element-type '(unsigned-byte 8)))
	 (kfs (make-instance 'kaitai-file-stream :io in))
	 (kbs (make-instance 'kaitai-byte-stream
			     :data #(#X48 #X65 #X6C #X6C #X6F #X20 #X57 #X6F
				     #X72 #X6C #X64 #X21)))
	 (kf-struct (make-instance 'fake-image :stream kfs))
	 (kb-struct (make-instance 'fake-image :stream kbs)))
    (funcall helper kf-struct)
    (funcall helper kb-struct)))


;; Test that a kaitai-struct is created correctly and can be read from
;; Test that the stream position is updated
(deftest test-basic-kaitai-struct
  (test-small-file #'test-basic-kaitai-struct-helper))

(defun test-basic-kaitai-struct-helper (ks)
  (let ((short-val (read-u2le (slot-value ks 'kaitai-struct::ks))))
    (progn
    (testing "test-basic-kaitai-struct should read data"
      (ok (= short-val #X0201)))
    (testing "test-basic-kaitai-struct should get position correctly"
      (ok (= (kaitai-stream:pos (slot-value ks 'kaitai-struct::ks)) 2))))))


;; Test that kaitai-stream-read chains reads from kaitai-struct to kaitai-stream
(deftest test-kaitai-stream-read
  (test-small-file #'test-kaitai-stream-read-helper))

(defun test-kaitai-stream-read-helper (ks)
  (progn
    (testing "kaitai-stream-read chains read"
      (ok (= (kaitai-stream-read ks 'read-u2le) #X0201)))))

;; Test that attribute readers can be registered and used
(deftest test-kaitai-struct-read-attribute
  (let* ((filename
	   (asdf:system-relative-pathname
	    "kaitai-struct-clisp-runtime/tests"
	    "tests/test-bytes-01-03.bin"))
	 (in (open filename :direction :input :element-type '(unsigned-byte 8)))
	 (kfs (make-instance 'kaitai-file-stream :io in))
	 (kbs (make-instance 'kaitai-byte-stream :data #(#X01 #X02 #X03)))
	 (kf-struct (make-instance 'fake-image :stream kfs))
	 (kb-struct (make-instance 'fake-image :stream kbs)))
    (test-kaitai-struct-read-attribute-helper kf-struct)
    (test-kaitai-struct-read-attribute-helper kb-struct)))

(defun test-kaitai-struct-read-attribute-helper (ks)
  (progn
    (register-attribute ks 'image-width
			#'(lambda () (kaitai-struct:kaitai-stream-read ks #'read-u2le)))
    (let ((reader (gethash 'image-width (slot-value ks 'kaitai-struct::slot-readers))))
      (testing "test-kaitai-struct-read-attribute should register attribute reader"
	(ok reader)))
    (let ((short-val (read-attribute ks 'image-width)))
      (testing "test-kaitai-struct-read-attribute image-width should read data"
	(ok (= short-val #X0201)))
      (testing "test-kaitai-struct-read-attribute image-width should update image-width slot"
	(ok (= (slot-value ks 'image-width) #X0201)))
      (testing "test-kaitai-struct-read-attribute image-width should not update image-height slot"
	(ok (= (slot-value ks 'image-height) #X0000)))
      (testing "test-kaitai-struct-read-attribute image-width should get position correctly"
	(ok (= (kaitai-stream:pos (slot-value ks 'kaitai-struct::ks)) 2))))))

;; Test that attribute readers can be registered and used
;; Tests reading two attributes
(deftest test-kaitai-struct-read-two-attributes
  (test-bytes-01-08-file #'test-kaitai-struct-read-two-attributes-helper))

(defun test-kaitai-struct-read-two-attributes-helper (ks)
  (progn
    (register-attribute ks 'image-width	#'(lambda () (kaitai-stream-read ks 'read-u2le)))
    (register-attribute ks 'image-height #'(lambda () (kaitai-stream-read ks 'read-u2le)))
    (let ((reader (gethash 'image-width (slot-value ks 'kaitai-struct::slot-readers))))
      (testing "test-kaitai-struct-read-attribute should register image-width attribute reader"
	(ok reader)))
    (let ((reader (gethash 'image-height (slot-value ks 'kaitai-struct::slot-readers))))
      (testing "test-kaitai-struct-read-attribute should register image-height attribute reader"
	(ok reader)))
    (let ((short-val (read-attribute ks 'image-width)))
      (testing "test-kaitai-struct-read-attribute should read data"
	(ok (= short-val #X0201)))
      (testing "test-kaitai-struct-read-attribute image-width should update slot"
	(ok (= (slot-value ks 'image-width) #X0201)))
      (testing "test-kaitai-struct-read-attribute should get position correctly"
	(ok (= (kaitai-stream:pos (slot-value ks 'kaitai-struct::ks)) 2))))
    (let ((short-val (read-attribute ks 'image-height)))
      (testing "test-kaitai-struct-read-attribute image-height should read data"
	(ok (= short-val #X0403)))
      (testing "test-kaitai-struct-read-attribute image-height should get position correctly"
	(ok (= (kaitai-stream:pos (slot-value ks 'kaitai-struct::ks)) 4))))))

;; Tests using peek-read in an attribute reader
;; Builds the attribute reader with peek-reader-builder
(deftest test-kaitai-struct-peek-reader-builder
  (test-small-file #'test-kaitai-struct-peek-reader-builder-helper))

(defun test-kaitai-struct-peek-reader-builder-helper (ks)
  (progn
    (register-attribute ks 'image-width (peek-reader-builder ks 'read-u2le))
    (let ((reader (gethash 'image-width (slot-value ks 'kaitai-struct::slot-readers))))
      (testing "test-kaitai-struct-peek-reader-builder should register attribute reader"
	(ok reader)))
    ;; read-attribute is setting the image-width slot to the reader, not the value
    ;; need to execute the reader
    (let ((short-val (read-attribute ks 'image-width)))
      (testing "test-kaitai-struct-peek-reader-builder should read data"
	(ok (= short-val #X0201)))
      (testing "test-kaitai-struct-peek-reader-builder should set slot"
	(ok (= (slot-value ks 'image-width) #X0201)))
      (testing "test-kaitai-struct-peek-reader-builder should not update image-height slot"
	(ok (= (slot-value ks 'image-height) #X0000)))
      (testing "test-kaitai-struct-peek-reader-builder should get position correctly"
	(ok (= (kaitai-stream:pos (slot-value ks 'kaitai-struct::ks)) 0))))))

;; Test using peek-read to do two reads.
;; We don't create a custom attribute reader.
;; This just tests the peek-read macro to make sure it works.
(deftest test-kaitai-struct-peek-read-two-reads
  (test-bytes-01-08-file #'test-kaitai-struct-peek-read-two-reads-helper))

(defun test-kaitai-struct-peek-read-two-reads-helper (ks)
  (progn
    (peek-read
	#'(lambda () (kaitai-stream::pos (slot-value ks 'kaitai-struct::ks)))
	#'(lambda (p) (kaitai-stream:seek (slot-value ks 'kaitai-struct::ks) p))
      (progn
	(testing "test-kaitai-struct-peek-read-two-reads should do first read"
	  (ok (= (read-u2le (slot-value ks 'kaitai-struct::ks)) #X0201)))
	(testing "test-kaitai-struct-peek-read-two-reads should do second read"
	  (ok (= (read-u2le (slot-value ks 'kaitai-struct::ks)) #X0403)))
	(testing "test-kaitai-struct-peek-read-two-reads should have moved the stream pointer inside the peek-read"
	  (ok (= (kaitai-stream::pos (slot-value ks 'kaitai-struct::ks)) 4)))))
    (testing "test-kaitai-struct-peek-read-two-reads should pop the stream position"
      (ok (= (kaitai-stream::pos (slot-value ks 'kaitai-struct::ks)) 0)))))

;; Test using peek-read to do a read nested in another read.
;; This just tests the peek-read macro to make sure it works.
(deftest test-kaitai-struct-peek-read-nested-reads
  (test-bytes-01-08-file #'test-kaitai-struct-peek-read-nested-reads-helper))

(defun test-kaitai-struct-peek-read-nested-reads-helper (ks)
  (progn
    ;; The outer peek-read
    (peek-read
	#'(lambda () (kaitai-stream::pos (slot-value ks 'kaitai-struct::ks)))
	#'(lambda (p) (kaitai-stream:seek (slot-value ks 'kaitai-struct::ks) p))
      (progn
	(testing "test-kaitai-struct-peek-read-two-reads should do first outer read"
	  (ok (= (read-u2le (slot-value ks 'kaitai-struct::ks)) #X0201)))
	;; The inner peek-read
	;; The current stream position should be 2
	(peek-read
	    #'(lambda () (kaitai-stream::pos (slot-value ks 'kaitai-struct::ks)))
	    #'(lambda (p) (kaitai-stream:seek (slot-value ks 'kaitai-struct::ks) p))
	  (progn
	    (testing "test-kaitai-struct-peek-read-two-reads should do inner read"
	      (ok (= (read-u2le (slot-value ks 'kaitai-struct::ks)) #X0403)))
	    (testing "test-kaitai-struct-peek-read-two-reads should have moved the stream pointer inside the inner peek-read"
	      (ok (= (kaitai-stream::pos (slot-value ks 'kaitai-struct::ks)) 4)))
	    (testing "test-kaitai-struct-peek-read-two-reads should do inner read"
	      (ok (= (read-u2le (slot-value ks 'kaitai-struct::ks)) #X0605)))
	    (testing "test-kaitai-struct-peek-read-two-reads should have moved the stream pointer inside the inner peek-read"
	      (ok (= (kaitai-stream::pos (slot-value ks 'kaitai-struct::ks)) 6)))))
	;; Back in the outer read, the nested read should have popped
	;; the stream position back to 2
	(testing "test-kaitai-struct-peek-read-two-reads should have popped the stream pointer after the inner peek-read"
	  (ok (= (kaitai-stream::pos (slot-value ks 'kaitai-struct::ks)) 2)))
	(testing "test-kaitai-struct-peek-read-two-reads should do second outer read"
	  (ok (= (read-u2le (slot-value ks 'kaitai-struct::ks)) #X0403)))
	(testing "test-kaitai-struct-peek-read-two-reads should have moved the stream pointer inside the peek-read"
	  (ok (= (kaitai-stream::pos (slot-value ks 'kaitai-struct::ks)) 4)))))
    (testing "test-kaitai-struct-peek-read-two-reads should pop the stream position"
      (ok (= (kaitai-stream::pos (slot-value ks 'kaitai-struct::ks)) 0)))))



(deftest test-kaitai-struct-dirty-slot-p
  (test-bytes-01-08-file #'test-kaitai-struct-dirty-slot-p-helper))

(defun test-kaitai-struct-dirty-slot-p-helper (ks)
  (progn
    (register-attribute ks 'image-width (peek-reader-builder ks 'read-u2le))
    (testing "test-kaitai-struct-dirty-slot-p should return nil by default"
      (ng (dirty-slot-p ks 'image-width)))
    (read-attribute ks 'image-width)
    (setf (gethash 'image-width (slot-value ks 'kaitai-struct::slot-cache)) t)
    (testing "test-kaitai-struct-dirty-slot-p should return t after getting set"
      (ok (equal (dirty-slot-p ks 'image-width) t)))
    (setf (gethash 'image-width (slot-value ks 'kaitai-struct::slot-cache)) nil)
    (testing "test-kaitai-struct-dirty-slot-p should return nil after setting to nil"
      (ng (dirty-slot-p ks 'image-width)))))

(deftest test-kaitai-with-cached-slot
  (test-bytes-01-08-file #'test-kaitai-struct-with-cached-slot-helper))

(defun test-kaitai-struct-with-cached-slot-helper (ks)
  (progn
    (register-attribute ks 'image-width
			#'(lambda () (read-u2le (slot-value ks 'kaitai-struct::ks))))
    (testing "test-kaitai-struct-with-cached-slot should calculate and return the slot value"
      (ok (= (with-cached-slot ks 'image-width
	       (read-attribute ks 'image-width))
	     #X0201)))
    (testing "test-kaitai-struct-with-cached-slot after with-cached-slot should have set dirty flag"
      (ok (equal (dirty-slot-p ks 'image-width) t)))
    (testing "test-kaitai-struct-with-cached-slot after with-cached-slot should have set slot"
      (ok (= (slot-value ks 'image-width) #X0201)))))
