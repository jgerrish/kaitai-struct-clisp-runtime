;; kaitai-struct runtime implementation
;;
;; This mostly follows the kaitai project's recommendations.
;; There are some custom methods and functions here to help make a
;; more functional parser.
(defpackage kaitai-struct
  (:use :cl :kaitai-struct-clisp-runtime :kaitai-stream)
  (:export :kaitai-struct :kaitai-stream-read :dirty-slot-p :with-cached-slot
   :register-attribute :read-attribute :peek-read :peek-reader-builder))

(in-package :kaitai-struct)

;; The base kaitai-struct class.  Code generated from the compiler to
;; parse binaries should subclass this.
(defclass kaitai-struct ()
  ;; ks is the kaitai-stream object associated with this kaitai-struct
  ((ks :initarg :stream
       :initform (error "Must supply a kaitai-stream"))

   ;; Slot / attribute cache flags
   ;; A map from attribute names to boolean flags.
   ;; If the flag is true, the attribute / slot has already been read
   ;; or calculated and can be returned.
   (slot-cache :initarg :slot-cache :initform (make-hash-table))

   ;; slot-readers maps slots to reader functions like read-u2 that
   ;; parse the raw data
   ;; It's a trade-off between making every attribute a class or not
   (slot-readers :initarg :slot-readers :initform (make-hash-table))))

(defmethod print-object ((object kaitai-struct) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (ks) object
      (format stream ":ks ~a" ks))))

(defmethod kaitai-stream-read ((ks kaitai-struct) read-fun)
  "read function to chain reads to the kaitai-stream slot

   Example:

   (kaitai-struct:kaitai-stream-read ks 'read-u2le)"
  (funcall read-fun (slot-value ks 'kaitai-struct::ks)))


(defmethod dirty-slot-p ((ks kaitai-struct) slot)
  "Check if a slot has been written to.
   Return true if the slot is dirty, false if it isn't"
  (multiple-value-bind (dirty present) (gethash slot (slot-value ks 'slot-cache))
    ;; There are three possible conditions:
    ;; 1. Isn't present (never been updated), return false
    ;; 2. Is present and false, return false
    ;; 3. Is present and true, return true
    (if present	dirty nil)))

(defmacro with-cached-slot (ks slot-name &body body)
  "This provides a functional wrapper around the Kaitai standard
   practice of checking to see if a composite attribute has been
   calculated, returning it or calculating and returning it.  It
   doesn't perform locking or any other concurrency, but it sets up a
   good pattern."
  (with-gensyms (return-value)
    `(if (dirty-slot-p ,ks ,slot-name)
	 (slot-value ,ks ,slot-name)
	 (let ((,return-value (progn ,@body)))
	   (progn
	     (setf (gethash ,slot-name (slot-value ,ks 'slot-cache)) t)
	     ,return-value)))))

(defmethod register-attribute ((ks kaitai-struct) attribute reader)
  "register-attribute adds an attribute to the object.
   It links the attribute's slot to a reader function like 'read-u2

   Example: (register-attribute my-ks 'image-width
                                #'(lambda () (read-u2le (slot-value my-ks 'ks))))

   With the peek-read function:

   (register-attribute my-ks 'image-width
     (peek-read #'(lambda () (pos (slot-value my-ks 'ks)))
                #'(lambda (p) (seek (slot-value my-ks 'ks) p))
                (read-u2le (slot-value my-ks 'ks))))"
   (setf (gethash attribute (slot-value ks 'slot-readers)) reader))

(defmethod read-attribute ((ks kaitai-struct) attribute)
  "read an attribute, setting the appropriate slot value with the
   correct read function.

   NOTE: This also sets the slot value

   Example use:

   (register-attribute my-ks 'image-width (reader-builder my-ks #'read-u2le))
   (read-attribute my-ks 'image-width)
   (pos (slot-value my-ks 'ks))

   read is still an expression: (1+ (read-attribute my-ks 'image-width))"
  (multiple-value-bind (reader present) (gethash attribute (slot-value ks 'slot-readers))
    (if present
	(setf (slot-value ks attribute) (funcall reader)))))

(defmacro peek-read (pos-fun seek-fun &body body)
  "Save the current stream position, execute the body, then restore the stream position
   Takes three parameters:
   pos-fun: the function to get the stream position
   seek-fun: the function to set the stream position
   body: the code to execute

   Example use (stubs kaitai-stream seek and pos methods):
   (defvar my-test-pos 3)
   (defun set-pos (p) (setf my-test-pos p))
   (defun get-pos () my-test-pos)
   (print (peek-read #'get-pos #'set-pos (progn (print \"start\") (print my-test-pos) (setf my-test-pos (1+ my-test-pos)) (print \"end\") (print my-test-pos) 3)))

   Example with a kaitai-struct:

   (defclass fake-image (kaitai-struct)
     ((image-width :type (integer * *) :initarg :image-width :initform 0)))

   (defvar my-ks (make-instance 'fake-image
                                :stream (make-instance 'kaitai-byte-stream :data #(#X01 #X02 #X03))))

   (defun get-pos () (pos (slot-value my-ks 'ks)))
   (defun set-pos (p) (seek (slot-value my-ks 'ks) p))
   (register-attribute my-ks 'iamge-width
                  (peek-read #'get-pos #'set-pos
                    (read-u2le (slot-value my-ks 'ks))))
   (read-attribute my-ks 'image-width)
   (pos (slot-value my-ks 'ks))"
  (with-gensyms (saved-pos return-value)
    `(let ((,saved-pos (funcall ,pos-fun)))
      (let ((,return-value (progn ,@body)))
        (funcall ,seek-fun ,saved-pos)
	,return-value))))

(defmethod peek-reader-builder ((ks kaitai-struct) read-fun)
  "Build a reader function to read in data.
   This uses peek-read to save and restore stream position.

   Example:
   (register-attribute ks 'image-width
     (setf (slot-value ks 'image-width) (peek-reader-builder ks 'read-u2le)))"
  (lambda ()
    (peek-read
	#'(lambda () (kaitai-stream:pos (slot-value ks 'ks)))
	#'(lambda (p) (kaitai-stream:seek (slot-value ks 'ks) p))
      (funcall read-fun (slot-value ks 'ks)))))
