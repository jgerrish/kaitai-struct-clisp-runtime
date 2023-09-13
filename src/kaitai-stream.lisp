(defpackage kaitai-stream
  (:use :cl)
  (:export
   :kaitai-stream :kaitai-stream-eof-error :kaitai-stream-not-implemented-error
   :kaitai-stream-eof-error-text :kaitai-stream-not-implemented-error-text
   :kaitai-stream-validation-not-equal-error :kaitai-stream-validation-not-equal-error-text
   :kaitai-file-stream :kaitai-byte-stream
   :seek :pos :eof-p :size
   :sign-bit :read-signed
   :read-s1 :read-s2be :read-s4be :read-s8be :read-s2le :read-s4le :read-s8le
   :read-u1 :read-u2be :read-u4be :read-u8be :read-u2le :read-u4le :read-u8le
   :read-f4be :read-f8be :read-f4le :read-f8le
   :align-to-byte :read-bits-int :read-bits-array
   :read-bytes :read-bytes-full :read-bytes-term :ensure-fixed-contents :bytes-strip-right
   :bytes-terminate :bytes-to-str
   :process-xor :process-xor-one :process-xor-many :process-rotate-left :process-zlib
   :kaitai-stream-mod))

(in-package :kaitai-stream)

;; kaitai-stream base class
(defclass kaitai-stream ()
  ())

(defmethod print-object ((object kaitai-stream) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "pos: ~d" (pos object))))

;; A basic error indicating that a read was attempted beyond the end
;; of a stream
(define-condition kaitai-stream-eof-error (error)
  ((text :initarg :text :reader kaitai-stream-eof-error-text)))

;; An error indicating that ensure-fixed-contents match failed
(define-condition kaitai-stream-validation-not-equal-error (error)
  ((text :initarg :text :reader kaitai-stream-validation-not-equal-error-text)))

;; A basic error indicating that a function isn't implemented
(define-condition kaitai-stream-not-implemented-error (error)
  ((text :initarg :text :reader kaitai-stream-not-implemented-error-text)))

;; A subclass using files as a backing store
(defclass kaitai-file-stream (kaitai-stream)
  ((io :initarg :io :initform (error "Must supply an IO object"))))

(defmethod print-object ((object kaitai-file-stream) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (io) object
      (format stream ":io ~a" io)))
  (format stream ", ")
  (call-next-method))

;; A subclass using vectors as a backing store
(defclass kaitai-byte-stream (kaitai-stream)
  ((data :initarg :data :initform (error "Must supply a data vector object"))
   ;; The index into the current item in the byte array
   (index :initarg :index :initform 0)))

(defmethod print-object ((object kaitai-byte-stream) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (data) object
      (format stream ":data ~a" data)))
  (format stream ", ")
  (call-next-method))

(defmethod read-kaitai-stream-byte ((ks kaitai-file-stream))
  (read-byte (slot-value ks 'io)))

(defmethod read-kaitai-stream-byte ((ks kaitai-byte-stream))
  (if (eof-p ks)
      (error 'kaitai-stream-eof-error :text "eof")
      (let ((index (slot-value ks 'index)))
	(let ((value (elt (slot-value ks 'data) index)))
	  (setf (slot-value ks 'index) (1+ index))
	  value))))
    ;; (sb-kernel:bounding-indices-bad-error (e) (error 'kaitai-stream-eof-error :text "eof"))
    ;; (simple-error (e) (error 'kaitai-stream-eof-error :text "eof"))))

;; These are the main functions and methods that satisfy the Kaitai
;; runtime requirements.
;; See runtime/README.md - Developers' memo for runtimes

;; Stream positioning

(defmethod seek ((ks kaitai-file-stream) position)
  (file-position (slot-value ks 'io) position))

(defmethod seek ((ks kaitai-byte-stream) position)
  (setf (slot-value ks 'index) position))

(defmethod pos ((ks kaitai-file-stream))
  (file-position (slot-value ks 'io)))

(defmethod pos ((ks kaitai-byte-stream))
  (slot-value ks 'index))

;; EOF may have a different meaning besides no bytes left.
;; For example, if the stream reader threw an eof-error.
;;
;; Using fread:
;;
;; For POSIX systems (POSIX.1-2001, POSIX.1-2008), reading in the
;; entire file stream will not set the EOF flag.  Reading one byte
;; beyond the end of the stream will set the EOF flag.
;;
;; Using C++ and ifstream:
;;
;; Behavior is different than POSIX, reading in the entire file stream
;; will set the EOF flag, so the eof() member function will return
;; true.
;; If it's an empty file, no read is needed, eof() is always true.
;;
;; In a redesign to a more parser-combinator model, eof-p may not need
;; to be a separate function, and each individual parser can return a
;; richer set of error results or exceptions.
;;
;; For this version, we'll match kaitai's C++ runtime behavior, which
;; matches the C++ ifstream behavior.
(defmethod eof-p ((ks kaitai-stream))
  (= (- (size ks) (pos ks)) 0))

(defmethod size ((ks kaitai-file-stream))
  (file-length (slot-value ks 'io)))

(defmethod size ((ks kaitai-byte-stream))
  (length (slot-value ks 'data)))

;; Integer numbers

;; General format is: read-$S$L$E
;;   $S is unsigned(u) or signed(s)
;;   $L is length of integer (1, 2, 4, or 8)
;;   $E is endianness (le or be)

;; Signed

;; For little-endian, the sign bit is in the last byte.
;; In other words, after the bytes are arranged from the byte stream
;; into the number, then the sign-bit (highest order bit) is checked
;; in the formed number.
;;
;; For big-endian, the sign bit is in the first byte.
;; In other words, after the bytes are arranged from the byte stream
;; into the number, then the sign-bit (highest-order bit) is checked
;; in the formed number.

;; Return T if the sign bit is set, otherwise return NIL
;; Takes two arguments, the number to check and the number of bits in
;; the number.
;;
;; Interesting behavior using eq instead of = or equal for 64-bit
;; numbers.
;; Maybe the underlying representation changes at the 64-bit boundary?
;; Goes to arbitrary precision after an arithmetic operation and
;; comparing that to a normal 64-bit value fails because the
;; underlying representation isn't equal.
(defun sign-bit (number num-bits)
  (let ((mask (expt 2 (1- num-bits))))
    (= (logand number mask) mask)))


(defun number-mask (num-bits)
  "The mask for the signed number"
  (1- (expt 2 (1- num-bits))))

(defun read-signed (number num-bits)
  "Read a signed number with a given reader and number of bits"
  (let ((masked-number (logand (number-mask num-bits) number)))
    (if (sign-bit number num-bits)
	(- masked-number)
	masked-number)))

(defmethod read-s1 ((ks kaitai-stream))
  (read-signed (read-u1 ks) 8))

;; Big-endian

(defmethod read-s2be ((ks kaitai-stream))
  (read-signed (read-u2be ks) 16))

(defmethod read-s4be ((ks kaitai-stream))
  (read-signed (read-u4be ks) 32))

(defmethod read-s8be ((ks kaitai-stream))
  (read-signed (read-u8be ks) 64))

;; Little-endian

(defmethod read-s2le ((ks kaitai-stream))
  (read-signed (read-u2le ks) 16))

(defmethod read-s4le ((ks kaitai-stream))
  (read-signed (read-u4le ks) 32))

(defmethod read-s8le ((ks kaitai-stream))
  (read-signed (read-u8le ks) 64))

;; Unsigned

(defmethod read-u1 ((ks kaitai-stream))
  (read-kaitai-stream-byte ks))

;; Big-endian

(defmethod read-u2be ((ks kaitai-stream))
  (logior (ash (read-kaitai-stream-byte ks) 8) (read-kaitai-stream-byte ks)))

(defmethod read-u4be ((ks kaitai-stream))
  (logior
   (ash (read-kaitai-stream-byte ks) 24)
   (ash (read-kaitai-stream-byte ks) 16)
   (ash (read-kaitai-stream-byte ks) 8)
   (read-kaitai-stream-byte ks)))

(defmethod read-u8be ((ks kaitai-stream))
  (logior
   (ash (read-kaitai-stream-byte ks) 56)
   (ash (read-kaitai-stream-byte ks) 48)
   (ash (read-kaitai-stream-byte ks) 40)
   (ash (read-kaitai-stream-byte ks) 32)
   (ash (read-kaitai-stream-byte ks) 24)
   (ash (read-kaitai-stream-byte ks) 16)
   (ash (read-kaitai-stream-byte ks) 8)
   (read-kaitai-stream-byte ks)))

;; Little-endian

(defmethod read-u2le ((ks kaitai-stream))
  (logior (read-kaitai-stream-byte ks) (ash (read-kaitai-stream-byte ks) 8)))

(defmethod read-u4le ((ks kaitai-stream))
  (logior (read-kaitai-stream-byte ks)
	  (ash (read-kaitai-stream-byte ks) 8)
	  (ash (read-kaitai-stream-byte ks) 16)
	  (ash (read-kaitai-stream-byte ks) 24)))

(defmethod read-u8le ((ks kaitai-stream))
  (logior (read-kaitai-stream-byte ks)
	  (ash (read-kaitai-stream-byte ks) 8)
	  (ash (read-kaitai-stream-byte ks) 16)
	  (ash (read-kaitai-stream-byte ks) 24)
	  (ash (read-kaitai-stream-byte ks) 32)
	  (ash (read-kaitai-stream-byte ks) 40)
	  (ash (read-kaitai-stream-byte ks) 48)
	  (ash (read-kaitai-stream-byte ks) 56)))

;; Floating point numbers

;; These use SBCL specific functions to parse floating point numbers.
;; We'll get the basic tests working, using Python as a reference
;; comparison.  Then move over to custom LISP.
;;
;; The quicklisp package ieee-floats could also be used, but there are
;; currently no other dependencies for the runtime.

;; Big-endian

;;   (sb-kernel:make-single-float #b0111011010111010010110100001000)

(defmethod read-f4be ((ks kaitai-stream))
  #+(and floating-point sbcl) (sb-kernel:make-single-float (read-u4be ks))
  #-(or floating-point) (error
			 'kaitai-stream-not-implemented-error
			 :text "Floating point numbers are not implemented"))

(defmethod read-f8be ((ks kaitai-stream))
  #+(and floating-point sbcl) (let ((hi-bits (read-u4be ks))
				    (lo-bits (read-u4be ks)))
				(sb-kernel:make-double-float hi-bits lo-bits))
  #-(or floating-point) (error
			 'kaitai-stream-not-implemented-error
			 :text "Floating point numbers are not implemented"))

;; Little-endian

(defmethod read-f4le ((ks kaitai-stream))
  #+(and floating-point sbcl) (sb-kernel:make-single-float (read-u4le ks))
  #-(or floating-point) (error
			 'kaitai-stream-not-implemented-error
			 :text "Floating point numbers are not implemented"))

(defmethod read-f8le ((ks kaitai-stream))
  #+(and floating-point sbcl) (let ((lo-bits (read-u4le ks))
				    (hi-bits (read-u4le ks)))
				(sb-kernel:make-double-float hi-bits lo-bits))
  #-(or floating-point) (error
			 'kaitai-stream-not-implemented-error
			 :text "Floating point numbers are not implemented"))

;; Unaligned bit values

(defmethod align-to-byte ((ks kaitai-stream))
  (error 'kaitai-stream-not-implemented-error
	 :text "not implemented"))

(defmethod read-bits-int ((ks kaitai-stream) n)
  (error 'kaitai-stream-not-implemented-error
	 :text "not implemented"))

(defmethod read-bits-array ((ks kaitai-stream) n)
  (error 'kaitai-stream-not-implemented-error
	 :text "not implemented"))


;; Byte arrays

(defmethod read-bytes ((ks kaitai-file-stream) n)
  "Read n bytes and return as a vector"
  ;; Doing bounds checks on every read-bytes is slow, but it helps
  ;; provide a consistent API
  (if (> n (- (size ks) (pos ks)))
      (error 'kaitai-stream-eof-error :text "eof")
      (let ((buf (make-array n :element-type '(unsigned-byte 8))))
	(read-sequence buf (slot-value ks 'io))
	buf)))

(defmethod read-bytes ((ks kaitai-byte-stream) n)
  (if (> n (- (size ks) (pos ks)))
      (error 'kaitai-stream-eof-error :text "eof")
      (let ((index (slot-value ks 'index)))
	(let ((buf (subseq (slot-value ks 'data) index (+ index n))))
	  (setf (slot-value ks 'index) (+ index n))
	  buf))))
    ;; (sb-kernel:bounding-indices-bad-error () (error 'kaitai-stream-eof-error :text "eof"))
    ;; (simple-error () (error 'kaitai-stream-eof-error :text "eof"))))

(defmethod read-bytes-full ((ks kaitai-file-stream))
  (let ((n (- (size ks) (pos ks))))
    (let ((buf (make-array n :element-type '(unsigned-byte 8))))
      (read-sequence buf (slot-value ks 'io))
      buf)))

(defmethod read-bytes-full ((ks kaitai-byte-stream))
  (let ((index (slot-value ks 'index)))
    (let ((buf (subseq (slot-value ks 'data) index)))
      (setf (slot-value ks 'index) (+ index (length buf)))
      buf)))

;; ((ks kaitai-stream) (String encoding, int term, boolean include-term, boolean consumeTerm, boolean eosError))
(defmethod read-bytes-term ((ks kaitai-stream)
			    encoding term include-term consume-term eos-error)
  "Read bytes until a terminating character is reached

   - term is the terminating character.
   - include-term is a boolean indicating whether the terminating
     character should be included in the returned buffer.
   - consume-term is a boolean indicating whether the terminating character should be
     consumed or left in the stream
   - eos-error is a boolean indicating whether an exception should be raised if no
     terminating character is found."
  (error 'kaitai-stream-not-implemented-error
	 :text "not implemented"))

(defmethod ensure-fixed-contents ((ks kaitai-stream) expected)
  "Compare the expected n bytes with the next n bytes from the stream

  - If the next n bytes matches the expected n bytes, return the
    buffer.
  - If the next n bytes don't match the expected n bytes,
    return a kaitai-stream-validation-not-equal-error error.

  What happens if the expected stream is longer than the remaining bytes?
  We could either raise a kaitai-stream-eof-error or a kaitai-stream-validation-not-equal-error
  We'll raise a kaitai-stream-validation-not-equal-error."
  ;; Check for the condition where expected is longer than remaining
  (if (> (length expected) (- (size ks) (pos ks)))
	(error 'kaitai-stream-validation-not-equal-error
	       :text "Actual does not equal expected")
	(let ((buf (read-bytes ks (length expected))))
	  (if (mismatch buf expected)
	      (error 'kaitai-stream-validation-not-equal-error
		     :text "Actual does not equal expected")
	      buf))))

(defun bytes-strip-right (bytes pad-byte)
  (error 'kaitai-stream-not-implemented-error
	 :text "not implemented"))

(defun bytes-terminate (bytes term include-term)
  (error 'kaitai-stream-not-implemented-error
	 :text "not implemented"))

(defun bytes-to-str (bytes encoding)
  (error 'kaitai-stream-not-implemented-error
	 :text "not implemented"))

;; Byte array processing

(defmethod process-xor ((ks kaitai-stream) data key)
  (error 'kaitai-stream-not-implemented-error
	 :text "not implemented"))

(defmethod process-xor-one ((ks kaitai-stream) data key)
  (error 'kaitai-stream-not-implemented-error
	 :text "not implemented"))

(defmethod process-xor-many ((ks kaitai-stream) data key)
  (error 'kaitai-stream-not-implemented-error
	 :text "not implemented"))

(defmethod process-rotate-left ((ks kaitai-stream) data amount group-size)
  (error 'kaitai-stream-not-implemented-error
	 :text "not implemented"))

(defmethod process-zlib ((ks kaitai-stream) data)
  (error 'kaitai-stream-not-implemented-error
  	 :text "not implemented"))

;; Misc runtime operations

;; Already defined in Common LISP

;; (defun mod (a b))


;; Behavior of mod isn't implemented in all runtimes
;; There are implementations in CPP, Java and Javascript
;; Those runtimes throw an exception if b <= 0
;; This needs to be implemented and tested
(defun kaitai-stream-mod (a b)
  (error 'kaitai-stream-not-implemented-error :text "not implemented"))
