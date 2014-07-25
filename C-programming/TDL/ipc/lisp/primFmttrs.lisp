;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PROJECT: New Millennium, DS1
;;;          IPC (Interprocess Communication) Package
;;; 
;;; FILE: primFmttrs.lisp
;;;
;;; ABSTRACT: Primitive Data Formatters
;;;           Same as the TCA primFmttrs.lisp file, except in the IPC package.
;;;
;;; $Revision: 2.4 $
;;; $Date: 2014/02/28 02:27:15 $
;;; $Author: reids $
;;;
;;; Copyright (c) 2008, Carnegie Mellon University
;;;     This software is distributed under the terms of the 
;;;    Simplified BSD License (see ipc/LICENSE.TXT)

;;; REVISION HISTORY
;;;
;;;  $Log: primFmttrs.lisp,v $
;;;  Revision 2.4  2014/02/28 02:27:15  reids
;;;  Added more types that could be counted as primitives
;;;
;;;  Revision 2.3  2013/07/23 21:11:49  reids
;;;  Updated for using SWIG
;;;
;;;  Revision 2.2  2009/01/12 15:54:55  reids
;;;  Added BSD Open Source license info
;;;
;;;  Revision 2.1.1.1  1999/11/23 19:07:37  reids
;;;  Putting IPC Version 2.9.0 under local (CMU) CVS control.
;;;
;;;  Revision 1.2.4.4  1997/01/25 22:44:02  udo
;;;  ipc_2_6 to r3_dev merge
;;;
;;;  Revision 1.2.4.3.6.1  1996/12/27 19:30:18  reids
;;;  Fixed the way Lisp is passed integer values of various sizes,
;;;  and the way it handles arrays of integers (or various sizes).
;;;
;;;  Revision 1.2.4.3  1996/10/29 04:42:45  reids
;;;  Fixed input argument type for formatPutInt.
;;;
;;;  Revision 1.2.4.2  1996/10/24 17:00:30  reids
;;;  Add additional primitive formats (byte, ubyte, short).
;;;
;;;  Revision 1.2.4.1  1996/10/08 14:27:56  reids
;;;  Changes to enable IPC to run under Lispworks on the PPC.
;;;  Main changes due to the fact that Lispworks currently does not support
;;;  "foreign-callable" and that it corrupts the stack when a double is sent
;;;  as the first argument to a foreign (C) function.
;;;
;;;  Revision 1.2  1996/03/12 03:11:47  reids
;;;  Support for the "enum" format in LISP, including automatic conversion
;;;    between integer (C) and keyword (LISP) forms.
;;;
;;;  Revision 1.1  1996/03/03 04:38:58  reids
;;;  First release of IPC files.  Corresponds to LISP version of
;;;  IPC Specifiction 2.2, except that IPC_printData and IPC_readData are
;;;  not yet implemented.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DR 2/98 Revised all the windows *features*

;; NOTE: All WINDOWS options are added by TNgo, 5/19/97, for PC Allegro

(in-package :IPC)

#+MCL
(defun blockCopyToArray (int1 array int2)
  (declare (ignore int1 array int2))
  (format T "Calling blockCopyToArray~%")
  (abort))

#+MCL 
(defun blockCopyFromArray (int1 array int2)
  (declare (ignore int1 array int2))
  (format t "Calling blockCopyFromArray~%")
  (abort))

(defun formatChoosePtrFormat (format parentFormat)
  (if (null parentFormat) 
      (formatChoosePtrFormatC format 0) 
    (formatChoosePtrFormatC format parentFormat) ))

;;; must follow c - enum
(defconstant Encode		0)
(defconstant Decode		1)
(defconstant ELength		2)
(defconstant ALength		3)
(defconstant RLength		4)
(defconstant SimpleType		5)
(defconstant DPrint		6)
(defconstant DFree		7)
(defconstant SimpleTypeSize	8)

;;;*********************************

;; SimpleTypeSize is the amount of storage to byte copy in C per element
;; if a byte copy of some array is possible, otherwise if byte copy is
;; not possible for this element type NIL is returned.

;;;*********************************

;;; IMPORTANT!! Needs to match C translation functions
(defconstant INT_FMT		1)
(defconstant BOOLEAN_FMT	2)
(defconstant FLOAT_FMT		3)
(defconstant DOUBLE_FMT		4)
(defconstant BYTE_FMT		5)
;;(defconstant TWOBYTE_FMT	6)
(defconstant STR_FMT		7)
;;(defconstant FORMAT_FMT	8)
(defconstant UBYTE_FMT		9)
;;(defconstant CMAT_FMT		10)
;;(defconstant SMAT_FMT		11)
;;(defconstant IMAT_FMT		12)
;;(defconstant LMAT_FMT		13)
;;(defconstant FMAT_FMT		14)
;;(defconstant DMAT_FMT		15)
(defconstant CHAR_FMT		16)
(defconstant SHORT_FMT		17)
(defconstant LONG_FMT		18)
;;(defconstant UCMAT_FMT	19)

(defconstant USHORT_FMT		28)
(defconstant UINT_FMT		29)
(defconstant ULONG_FMT		30)
(defconstant ENUM_FMT		31)
(defconstant MAXFORMATTERS	32)

;;; IMPORTANT!! Must follow C-Enum
(defconstant PrimitiveFMT	0)
(defconstant LengthFMT		1)
(defconstant StructFMT		2)
(defconstant PointerFMT		3)
(defconstant FixedArrayFMT	4)
(defconstant VarArrayFMT	5)
(defconstant BadFormatFMT	6)
(defconstant NamedFMT		7)
(defconstant EnumFMT		8)

(defconstant CHAR_SIZE		1)
(defconstant BYTE_SIZE		1)
(defconstant SHORT_SIZE		2)
(defconstant INT_SIZE		4)
(defconstant FLOAT_SIZE		4) 
(defconstant DOUBLE_SIZE	8)
(defconstant PTR_SIZE		4)
(defconstant DATAPTR_SIZE	1)
(defconstant ENUM_SIZE   INT_SIZE)

(defconstant BYTE_TYPE	`(signed-byte ,(* 8 BYTE_SIZE)))
(defconstant SHORT_TYPE	`(signed-byte ,(* 8 SHORT_SIZE)))
(defconstant INT_TYPE	`(signed-byte ,(* 8 INT_SIZE)))
;;(defconstant INT_TYPE	`integer)
(defconstant UBYTE_TYPE	`(unsigned-byte ,(* 8 BYTE_SIZE)))
(defconstant USHORT_TYPE	`(unsigned-byte ,(* 8 SHORT_SIZE)))
(defconstant UINT_TYPE	`(unsigned-byte ,(* 8 INT_SIZE)))

(defvar TransTable (make-array (1+ MAXFORMATTERS) :initial-element nil))

;; DR 2/98 added the eval-when to force def in compilation envmnt
(eval-when (compile load eval)
(defmacro doStructFormat ((index formatArray) &body body)
  `(do ((,index 1 (1+ ,index))
	(formatArraySize (formatFormatArrayMax ,formatArray)))
       ((>= ,index formatArraySize))
     ,@body))
)

;; DR 2/98 added the eval-when to force def in compilation envmnt
(eval-when (compile load eval)
(defmacro doArrayFormat ((index formatArray) &body body)
  `(do ((,index 2 (1+ ,index))
	(formatArraySize (formatFormatArrayMax ,formatArray)))
       ((>= ,index formatArraySize))
     ,@body))
)

(defun field-error (dfield afield)
  (error "ERROR: Data structure does not match format -- Should be ~a; is ~a"
	 dfield afield))

(defun check-field (object n types)
  (let ((field (aref object n)))
    (when field
      (let ((field-type (type-of field)))
	(unless (cond ((listp types) (member field-type types))
		      (t (equal field-type types)))
	  (field-error (if (listp types) (first types) types) field-type))))))

(defun check-field-string (object n)
  (let ((field (aref object n)))
    (when field
      (let ((field-type (type-of field)))
	(unless (and (listp field-type)
		     (equal (first field-type) 'simple-array)
		     (equal (second field-type) 'character))
	  (field-error 'string field-type))))))

;;;
;;; STR_Trans (Op DataStruct DStart Buffer BStart)
;;;
;;; "String" is an array of characters.
;;; Decodes to: length of string, Character-List
;;;

(defun STR_Trans (op &optional dataStruct dStart buffer)
  (cond ((eql op Decode)
	 (check-field-string dataStruct dStart)
         (let* ((length (formatGetInt buffer))
		(string (make-string length)))
	   (cond ((zerop length) (formatGetChar buffer))
		 (t (dotimes (i length) 
		      (setf (aref string i) (formatGetChar buffer)))
		    (setf (aref dataStruct dStart) string)))))
	
	((eql op Encode)
	 (check-field-string dataStruct dStart)
	 (let ((length (length (aref dataStruct dStart)))
	       (string (aref dataStruct dStart))
	       (nullString #\Z))
	   (formatPutInt buffer length)
	   (cond ((zerop length) (formatPutChar buffer nullString))
		 (t (dotimes (i length)
		      (formatPutChar buffer (aref string i)))))))

        ((eql op ELength)
	 (let ((str_length (length (aref dataStruct dStart))))
	   ( + (if (zerop str_length) CHAR_SIZE str_length) INT_SIZE)))

        ((eql op ALength) 1)

        ((eql op SimpleType) nil)
	((eql op SimpleTypeSize) nil)

        (t (error "STR_Trans: undefined op: ~d" op))))

;;;
;;; BYTE_Trans (Op DataStruct DStart Buffer BStart)
;;;

(defun BYTE_Trans (op &optional dataStruct dStart buffer)
  (cond ((eql op Decode) 
	 (check-field dataStruct dStart '(:byte integer fixnum))
	 (setf (aref dataStruct dStart) (formatGetByte buffer)))
	((eql op Encode)
	 (check-field dataStruct dStart '(:byte integer fixnum))
	 (formatPutByte buffer (aref dataStruct dStart)))
	((eql op ELength) BYTE_SIZE)
	((eql op ALength) 1)

	((eql op SimpleType) BYTE_TYPE)
	((eql op SimpleTypeSize) BYTE_SIZE)
	
	(t (error "BYTE_Trans ERROR ~%"))))

;;;
;;; UBYTE_Trans (Op DataStruct DStart Buffer BStart)
;;;

(defun UBYTE_Trans (op &optional dataStruct dStart buffer)
  (cond ((eql op Decode) 
	 (check-field dataStruct dStart '(:ubyte integer fixnum))
	 (setf (aref dataStruct dStart) (formatGetUByte buffer)))
	((eql op Encode)
	 (check-field dataStruct dStart '(:ubyte integer fixnum))
	 (formatPutUByte buffer (aref dataStruct dStart)))
	((eql op ELength) BYTE_SIZE)
	((eql op ALength) 1)

	((eql op SimpleType) UBYTE_TYPE)
	((eql op SimpleTypeSize) BYTE_SIZE)
	
	(t (error "BYTE_Trans ERROR ~%"))))

;;;
;;; SHORT_Trans (Op DataStruct DStart Buffer BStart)
;;;

(defun SHORT_Trans (op &optional dataStruct dStart buffer)
  (cond ((eql op Decode) 
	 (check-field dataStruct dStart '(:short integer fixnum))
	 (setf (aref dataStruct dStart) (formatGetShort buffer)))
	((eql op Encode)
	 (check-field dataStruct dStart '(:short integer fixnum))
	 (formatPutShort buffer (aref dataStruct dStart)))
	((eql op ELength) SHORT_SIZE)
	((eql op ALength) 1)

	((eql op SimpleType) SHORT_TYPE)
	((eql op SimpleTypeSize) SHORT_SIZE)
	
	(t (error "SHORT_Trans ERROR ~%"))))

;;;
;;; INT_Trans (Op DataStruct DStart Buffer BStart)
;;;

(defun INT_Trans (op &optional dataStruct dStart buffer)
  (cond ((eql op Decode) 
	 (check-field dataStruct dStart '(integer fixnum bignum))
	 (setf (aref dataStruct dStart) (formatGetInt buffer)))
	((eql op Encode)
	 (check-field dataStruct dStart '(integer fixnum bignum))
	 (formatPutInt buffer (aref dataStruct dStart)))
	((eql op ELength) INT_SIZE)
	((eql op ALength) 1)

	((eql op SimpleType) INT_TYPE)
	((eql op SimpleTypeSize) INT_SIZE)
	
	(t (error "INT_Trans ERROR ~%"))))

;;;
;;; CHAR_Trans (Op DataStruct DStart Buffer BStart)
;;;

(defun CHAR_Trans (op &optional dataStruct dStart buffer)
  (cond ((eql op Decode) 
	 (check-field dataStruct dStart 'character)
	 (setf (aref dataStruct dStart) (formatGetChar buffer)))
	((eql op Encode)
	 (check-field dataStruct dStart 'character)
	 (formatPutChar buffer (aref dataStruct dStart)))
	((eql op ELength) CHAR_SIZE)
	((eql op ALength) 1)

        ((eql op SimpleType) 'character)
	((eql op SimpleTypeSize) CHAR_SIZE)
	
	(t (error "CHAR_Trans ERROR ~%"))))

;;;
;;; FLOAT_Trans (Op DataStruct DStart Buffer BStart)
;;;
(defun FLOAT_Trans (op &optional dataStruct dStart buffer)
  (cond ((eql op Decode)
	 (check-field dataStruct dStart
			  '(:float single-float double-float integer fixnum bignum))
	 (setf (aref dataStruct dStart)
	       (coerce (formatGetFloat buffer) 'single-float)))
	((eql op Encode)
	 (check-field dataStruct dStart
			  '(:float single-float double-float integer fixnum bignum))
	 (formatPutFloat buffer
			 (coerce (aref dataStruct dStart) 'double-float)))
	((eql op ELength) FLOAT_SIZE)
	((eql op ALength) 1)

        ((eql op SimpleType) 'single-float)
	((eql op SimpleTypeSize) FLOAT_SIZE)
	
	(t (error "FLOAT_Trans ERROR ~%"))))

;;;
;;; DOUBLE_Trans (Op DataStruct DStart Buffer BStart)
;;;
(defun DOUBLE_Trans (op &optional dataStruct dStart buffer)
  (cond ((eql op Decode)
	 (check-field dataStruct dStart
			  '(:double double-float single-float integer fixnum bignum))
	 (setf (aref dataStruct dStart) (formatGetDouble buffer)))
	((eql op Encode)
	 (check-field dataStruct dStart
			  '(:double double-float single-float integer fixnum bignum))
	 (formatPutDouble buffer
			  (coerce (aref dataStruct dStart) 'double-float)))
	((eql op ELength) DOUBLE_SIZE)
	((eql op ALength) 1)

        ((eql op SimpleType) 'float)
	((eql op SimpleTypeSize) DOUBLE_SIZE)
	
	(t (error "DOUBLE_Trans ERROR ~%"))))

;;;
;;; BOOLEAN_Trans (Op DataStruct DStart Buffer BStart)
;;;
(defun BOOLEAN_Trans (op &optional dataStruct dStart buffer)
  (cond ((eql op Decode)
	 (check-field dataStruct dStart '(:boolean null symbol))
	 (let ((item (formatGetInt buffer)))
	   (cond ((eql item 0) (setf (aref dataStruct dStart) nil))
		 ((eql item 1) (setf (aref dataStruct dStart) t))
		 (t (error "BOOLEAN_Trans: not TRUE or FALSE")))))
	((eql op Encode)
	 (check-field dataStruct dStart '(:boolean null symbol))
	 (cond ((null (aref dataStruct dStart))	(formatPutInt buffer 0))
	       (t (formatPutInt buffer 1))))
	((eql op ELength) INT_SIZE)
	((eql op ALength) 1)

        ((eql op SimpleType) nil)
	((eql op SimpleTypeSize) nil)
	
	(t (error "BOOLEAN_Trans ERROR ~%"))))

;;;
;;; USHORT_Trans (Op DataStruct DStart Buffer BStart)
;;;

(defun USHORT_Trans (op &optional dataStruct dStart buffer)
  (cond ((eql op Decode) 
	 (check-field dataStruct dStart '(:ushort integer fixnum))
	 (setf (aref dataStruct dStart) (formatGetUShort buffer)))
	((eql op Encode)
	 (check-field dataStruct dStart '(:ushort integer fixnum))
	 (formatPutUShort buffer (aref dataStruct dStart)))
	((eql op ELength) SHORT_SIZE)
	((eql op ALength) 1)

	((eql op SimpleType) USHORT_TYPE)
	((eql op SimpleTypeSize) SHORT_SIZE)

	(t (error "USHORT_Trans ERROR ~%"))))

;;;
;;; UINT_Trans (Op DataStruct DStart Buffer BStart)
;;;

(defun UINT_Trans (op &optional dataStruct dStart buffer)
  (cond ((eql op Decode) 
	 (check-field dataStruct dStart '(integer fixnum bignum))
	 ;; Allegro does not do the right thing wrt
	 ;; large unsigned integers
	 (let ((intval (formatGetUInt buffer)))
	   (setf (aref dataStruct dStart)
		 (if (>= intval 0) intval (+ #x100000000 intval)))))
	((eql op Encode)
	 (check-field dataStruct dStart '(integer fixnum bignum))
	 (formatPutUInt buffer (aref dataStruct dStart)))
	((eql op ELength) INT_SIZE)
	((eql op ALength) 1)

	((eql op SimpleType) UINT_TYPE)
	((eql op SimpleTypeSize) INT_SIZE)

	(t (error "UINT_Trans ERROR ~%"))))

;;;
;;; ENUM_Trans (Op DataStruct DStart Buffer BStart)
;;;

(defun ENUM_Trans (op &optional dataStruct dStart buffer)
  (cond ((eql op Decode) 
	 (check-field dataStruct dStart '(:enum symbol integer fixnum))
	 (setf (aref dataStruct dStart) (formatGetInt buffer)))
	((eql op Encode)
	 (check-field dataStruct dStart '(:enum symbol integer fixnum))
	 (formatPutInt buffer (aref dataStruct dStart)))
	((eql op ELength) ENUM_SIZE)
	((eql op ALength) 1)

	((eql op SimpleType) INT_TYPE)
	((eql op SimpleTypeSize) ENUM_SIZE)

	(t (error "ENUM_Trans ERROR ~%"))))
