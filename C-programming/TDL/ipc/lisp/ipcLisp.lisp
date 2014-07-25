;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROJECT: IPC (Interprocess Communication) Package
;;;
;;; (c) Copyright 2013 Reid Simmons.  All rights reserved.
;;;
;;; FILE: ipcLisp.lisp
;;;
;;; ABSTRACT: Lisp code for interfacing specifically with the C version of IPC
;;;           Used by SWIG (see IPC.i)
;;;
;;;       $Id: ipcLisp.lisp,v 2.1 2013/07/31 19:54:49 reids Exp $
;;; $Revision: 2.1 $
;;;     $Date: 2013/07/31 19:54:49 $
;;;   $Author: reids $
;;;    $State: Exp $
;;;   $Locker:  $
;;;
;;; Copyright (c) 2011, Carnegie Mellon University
;;;     This software is distributed under the terms of the 
;;;     Simplified BSD License (see ipc/LICENSE.TXT)
;;;
;;; REVISION HISTORY
;;; $Log: ipcLisp.lisp,v $
;;; Revision 2.1  2013/07/31 19:54:49  reids
;;; Updated for using SWIG
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :IPC)

(defstruct (handler-data)
  key
  name
  proc
  unmarshall
  client-data
  timer-ref)

;; Export this function
(cl::defmacro defun-external (name (&rest args) &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (cl::export ',name)
     (defun ,name ,args ,@body)))

;; Export this constant
(cl::defmacro defconstant-external (name val)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (cl::export ',name)
     (defconstant ,name ,val)))

(defconstant-external IPC_WAIT_FOREVER #xFFFFFFFF)
(defconstant-external TRIGGER_FOREVER #xFFFFFFFF)
(defconstant-external IPC_VARIABLE_LENGTH #xFFFFFFFF)
(defconstant-external IPC_FIXED_LENGTH (1- #xFFFFFFFF))

(defparameter *KEY_HASH_TABLE* (make-hash-table)
  "Hash table keyed on handler key, value is handler-data structure")

(defparameter *NAME_HASH_TABLE* (make-hash-table :test #'equal)
  "Hash table keyed on handler name, value is handler key")

(defparameter *CHANGE_HANDLERS_TABLE* (make-hash-table :test #'equal)
  "Hash table of change handlers (each entry a list of handlers)")

(defparameter *CONNECT_HANDLERS_LIST* nil
  "List of connection handlers")

(defparameter *DISCONNECT_HANDLERS_LIST* nil
  "List of disconnection handlers")

(defparameter *MSG_CLASS_TABLE* (make-hash-table :test #'equal)
  "Hash table of classes associated with messages")

(defparameter *HANDLER_NUMBER* 0
  "Handler key, counts up from one")

(defun handler-name (handler)
  (format nil "~a" (cond ((functionp handler) 
			  #+:franz-inc (excl::external-fn_symdef handler)
			  #-:franz-inc handler) ;; This is not right
			 (t handler))))

(defun msg-handler-name (msgName handler)
  (format nil "~a_~a" msgName (handler-name handler)))

(defun query-handler-name (msgName handler)
  (format nil "Q_~a_~a" msgName (handler-name handler)))

(defun fd-handler-name (fd handler)
  (format nil "~d_~a" fd (handler-name handler)))

(defun timer-handler-name (handler)
  (format nil "~a" (handler-name handler)))

(defun get-handler-data-by-key (key)
  (gethash key *KEY_HASH_TABLE*))

(defun get-handler-data-by-name (name)
  (let ((key (gethash name *NAME_HASH_TABLE*)))
    (when key (gethash key *KEY_HASH_TABLE*))))

(defun set-handler-data (key name proc client-data &optional (unmarshall nil))
  (setf (gethash name *NAME_HASH_TABLE*) key)
  (setf (gethash key *KEY_HASH_TABLE*) 
	(make-handler-data :name name :key key
			   :proc proc :unmarshall unmarshall
			   :client-data client-data)))

(defun remove-handler-data-by-key (key)
  (let ((handler-data (get-handler-data-by-key key)))
    (when handler-data
      (remhash (handler-data-name handler-data) *NAME_HASH_TABLE*)
      (remhash key *KEY_HASH_TABLE*))))

(defun remove-handler-data-by-name (name)
  (let ((handler-data (get-handler-data-by-name name)))
    (when handler-data
      (remhash name *NAME_HASH_TABLE*)
      (remhash (handler-data-key handler-data) *KEY_HASH_TABLE*))))
		    
(defun update-handler-data (handler-name handler client-data unmarshall)
  (let ((handler-data (get-handler-data-by-name handler-name)))
    (cond (handler-data
	   (unless (equal (handler-data-client-data handler-data) client-data)
	     (format t "Resetting client data for handler ~a~%" handler-name)
	     (setf (handler-data-client-data handler-data) client-data))
	   (setf (handler-data-proc handler-data) handler)
	   (setf (handler-data-unmarshall handler-data) unmarshall))
	  (t (setq handler-data
	       (set-handler-data (incf *HANDLER_NUMBER*) handler-name
				 handler client-data unmarshall))))
    handler-data))

(defun handler-test (handler hnd-data)
  (equal handler (handler-data-name hnd-data)))

(defun find-handler (handler handler-list)
  (car (member (handler-name handler) handler-list :test  #'handler-test)))
  
(defmacro add-handler (handler handler-list &optional (client-data nil))
  `(setq ,handler-list (cons (make-handler-data :name (handler-name ,handler)
						:proc ,handler
						:client-data ,client-data)
			     ,handler-list)))

(defmacro rem-handler (handler handler-list)
  `(setq ,handler-list (remove (handler-name ,handler) ,handler-list
			       :test #'handler-test)))

(defun freeByteArray (varcontent)
  (unless (zerop (IPC_VARCONTENT_TYPE_length varcontent))
    (IPC_freeByteArray (IPC_VARCONTENT_TYPE_content varcontent))))

(defun boolVal (val) (if (= val 0) nil t))

(defun return-error (errno)
  (ipcSetError errno) 
  IPC_Error)

(defun-external IPC_isConnected ()
  (boolVal (IPC::_IPC_isConnected)))

(defun-external IPC_isModuleConnected (module)
  (boolVal (IPC::_IPC_isModuleConnected module)))

(defun-external IPC_isMsgDefined (msg)
  (boolVal (IPC::_IPC_isMsgDefined msg)))

(defun-external IPC_defineMsg (msgName length formatString)
  (_IPC_defineMsg msgName length (or formatString 0)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl::export 'IPC_defstruct))

(defun ipc-marshall (formatter lisp-data)
  (cond ((eql (checkMarshallStatus formatter) IPC_Error)
	 (values IPC_Error 0 0))
	(t (let ((length (BufferSize formatter lisp-data))
		 (byte-array nil))
	     (when (> length 0)
	       (setf byte-array (createByteArray length))
	       (let ((buffer (createBuffer byte-array)))
		 (EncodeData formatter lisp-data buffer)
		 (freeBuffer buffer)))
	     (values IPC_OK length byte-array)))))

(defun-external IPC_marshall (formatter lisp-data varcontent)
  (cond ((null varcontent) (return-error IPC_Null_Argument))
	(t (multiple-value-bind (retval length byte-array)
	       (ipc-marshall formatter lisp-data)
	     (when (eql retval IPC_OK)
	       (setf (IPC_VARCONTENT_TYPE_length varcontent) length)
	       (setf (IPC_VARCONTENT_TYPE_content varcontent) byte-array))
	     retval))))

(defun-external _IPC_unmarshall (formatter byte-array object)
  (cond ((zerop (validFormatter formatter)) nil)
	(t (let ((buffer (createBuffer byte-array)))
	     (prog1 (DecodeData formatter buffer object)
	       (freeBuffer buffer))))))

(defmacro IPC_unmarshall (formatter byte-array data)
  `(cond ((eql (checkMarshallStatus ,formatter) IPC_Error) IPC_Error)
	 (t (setf ,data (_IPC_unmarshall ,formatter ,byte-array ,data))
	    IPC_OK)))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl::export 'IPC_unmarshall))

(defun-external IPC_unmarshallData (formatter byte-array
					      &optional (classSymbol nil))
  (_IPC_unmarshall formatter byte-array (make-data-struct classSymbol)))

(defun foreign-callable-index (func)
  (second (multiple-value-list (ff:register-foreign-callable func :reuse t))))

(defun ipc-lisp-init()

  (clrhash *KEY_HASH_TABLE*)
  (clrhash *NAME_HASH_TABLE*)
  (clrhash *CHANGE_HANDLERS_TABLE*)

  (setq *CONNECT_HANDLERS_LIST* nil)
  (setq *DISCONNECT_HANDLERS_LIST* nil)

  (setf (aref TransTable CHAR_FMT) 'CHAR_Trans)
  (setf (aref TransTable BYTE_FMT) 'BYTE_Trans)
  (setf (aref TransTable UBYTE_FMT) 'UBYTE_Trans)
  (setf (aref TransTable SHORT_FMT) 'SHORT_Trans)
  (setf (aref TransTable INT_FMT) 'INT_Trans)
  (setf (aref TransTable LONG_FMT) 'INT_Trans)
  (setf (aref TransTable FLOAT_FMT) 'FLOAT_Trans)
  (setf (aref TransTable DOUBLE_FMT) 'DOUBLE_Trans)
  (setf (aref TransTable BOOLEAN_FMT) 'BOOLEAN_Trans)
  (setf (aref TransTable STR_FMT) 'STR_Trans)

  (setf (aref TransTable USHORT_FMT) 'USHORT_Trans)
  (setf (aref TransTable UINT_FMT) 'UINT_Trans)
  (setf (aref TransTable ULONG_FMT) 'UINT_Trans)

  (registerIndices (foreign-callable-index #'msgCallbackHandler)
		   (foreign-callable-index #'queryCallbackHandler)
		   (foreign-callable-index #'fdCallbackHandler)		   
		   (foreign-callable-index #'connectCallbackHandler)
		   (foreign-callable-index #'disconnectCallbackHandler)
		   (foreign-callable-index #'changeCallbackHandler)
		   (foreign-callable-index #'timerCallbackHandler))

  (setExitProc)
  )

(defun-external IPC_initialize ()
  (ipc-lisp-init)
  (IPC::_IPC_initialize))

(defun-external IPC_connect (module)
  (ipc-lisp-init)
  (IPC::_IPC_connect module))

(defun-external IPC_connectModule (module server)
  (ipc-lisp-init)
  (IPC::_IPC_connectModule module server))

(defun-external IPC_connectNoListen (module)
  (ipc-lisp-init)
  (IPC::_IPC_connectNoListen module))

(defun-external IPC_connectModuleNoListen (module server)
  (ipc-lisp-init)
  (IPC::_IPC_connectModuleNoListen module server))

(defun-external IPC_parseFormat (format)
  (cond ((null format) (_IPC_parseFormat ""))
	((not (stringp format))
	 (return-error IPC_Illegal_Formatter))
	(t (_IPC_parseFormat format))))

(defun-external IPC_publishData (msgName data)
  (let* ((vc (new_IPC_VARCONTENT_TYPE))
	 (retVal
	  (cond ((null data)
		 (IPC_publish msgName 0 (IPC_VARCONTENT_TYPE_content vc)))
		((not (eql (IPC_marshall (IPC_msgFormatter msgName) data vc)
			   IPC_Error))
		 (IPC_publish msgName (IPC_VARCONTENT_TYPE_length vc)
			      (IPC_VARCONTENT_TYPE_content vc)))
		(t IPC_Error))))
    (freeByteArray vc)
    retVal))

(defun getMsgClass (msgName formatter)
  (cond ((or (null formatter) (= (validFormatter formatter) 0)) nil)
	(t (let ((oclass (gethash msgName *MSG_CLASS_TABLE*)))
	     (unless oclass 
	       (format t "WARNING: Missing class associated with message ~a~%"
		       msgName))
	     oclass))))

(defun-foreign-callable msgCallbackHandler ((msgInstancePtr (* void))
					    (byteArray (* void))
					    (key :int))
  (let ((msgInstance (make-instance 'foreign-pointer
		       :foreign-address msgInstancePtr))
	(handlerData (get-handler-data-by-key key)))
    (cond ((null handlerData)
	   (format t "Ooops -- no handler for ~a~%" 
		   (IPC_msgInstanceName msgInstance)))
	  ((handler-data-unmarshall handlerData)
	   (let* ((formatter (IPC_msgInstanceFormatter msgInstance))
		  (data (make-data-struct
			 (getMsgClass (IPC_msgInstanceName msgInstance) formatter))))
	     (cond ((eql (IPC_unmarshall formatter byteArray data) IPC_Error)
		    (return-error IPC_Mismatched_Formatter))
		   (t (funcall (handler-data-proc handlerData) msgInstance
			       data (handler-data-client-data handlerData))))
	     (IPC_freeByteArray byteArray)))
	  (t (funcall (handler-data-proc handlerData) msgInstance byteArray
		      (handler-data-client-data handlerData))))))

(defun-foreign-callable queryCallbackHandler ((msgInstancePtr (* void))
					      (byteArray (* void))
					      (key :int))
  (let ((msgInstance (make-instance 'foreign-pointer
		       :foreign-address msgInstancePtr))
	(handlerData (get-handler-data-by-key key)))
    (cond ((null handlerData)
	   (format t "Ooops -- no handler for ~a~%" 
		   (IPC_msgInstanceName msgInstance)))
	  ((handler-data-unmarshall handlerData)
	   (let* ((formatter (IPC_msgInstanceFormatter msgInstance))
		  (data (make-data-struct
			 (getMsgClass (IPC_msgInstanceName msgInstance) formatter))))
	     (cond ((eql (IPC_unmarshall formatter byteArray data) IPC_Error)
		    (return-error IPC_Mismatched_Formatter))
		   (t (funcall (handler-data-proc handlerData) msgInstance
			       data (handler-data-client-data handlerData))))
	     (IPC_freeByteArray byteArray)))
	  (t (funcall (handler-data-proc handlerData) msgInstance byteArray
		      (handler-data-client-data handlerData))))))

(defun-foreign-callable fdCallbackHandler ((fd :int) (key :int))
  (let ((handlerData (get-handler-data-by-key key)))
    ;;(format t "fdCallbackHandler: ~a~%" key)
    (cond ((null handlerData)
	   (format t "Ooops -- no handler for fd ~d~%" key))
	  (t (funcall (handler-data-proc handlerData) fd
		      (handler-data-client-data handlerData))))))

(defun-foreign-callable connectCallbackHandler ((moduleName (* :char)))
  (setq moduleName (ff::native-to-string moduleName))
  ;;(format t "connectCallbackHandler: ~a~%" moduleName)
  (dolist (connectHandler *CONNECT_HANDLERS_LIST*)
    (funcall (handler-data-proc connectHandler) moduleName
	     (handler-data-client-data connectHandler))))

(defun-foreign-callable disconnectCallbackHandler ((moduleName (* :char)))
  (setq moduleName (ff::native-to-string moduleName))
  ;;(format t "disconnectCallbackHandler: ~a~%" moduleName)
  (dolist (disconnectHandler *DISCONNECT_HANDLERS_LIST*)
    (funcall (handler-data-proc disconnectHandler) moduleName
	     (handler-data-client-data disconnectHandler))))

(defun-foreign-callable changeCallbackHandler ((msgName (* :char))
					       (numHandlers :int))
  (setq msgName (ff::native-to-string msgName))
  ;;(format t "changeCallbackHandler: ~a ~a~%" msgName numHandlers)
  (let ((change-handlers (gethash msgName *CHANGE_HANDLERS_TABLE*)))
    (when change-handlers
      (dolist (change-handler change-handlers)
	(funcall (handler-data-proc change-handler) msgName numHandlers
		 (handler-data-client-data change-handler))))))

(defun-external IPC_subscribe (msg-name handler &optional (client-data nil))
  (let* ((hnd-name (msg-handler-name msg-name handler))
	 (handler-data (update-handler-data hnd-name handler client-data nil)))
    (subscribe msg-name hnd-name (handler-data-key handler-data))))

(defun-external IPC_subscribeData (msg-name handler
					    &optional (client-data nil))
  (let* ((hnd-name (msg-handler-name msg-name handler))
	 (handler-data (update-handler-data hnd-name handler client-data t)))
    (subscribe msg-name hnd-name (handler-data-key handler-data))))

(defun-external IPC_unsubscribe (msg-name handler)
  (let ((hnd-name (msg-handler-name msg-name handler)))
    (remove-handler-data-by-name hnd-name)
    (_IPC_unsubscribe msg-name hnd-name)))

(defun-external IPC_subscribeFD (fd fdHandler &optional (client-data nil))
  (let* ((hnd-name (fd-handler-name fd fdHandler))
	 (handler-data (update-handler-data hnd-name fdHandler
					    client-data nil)))
    (subscribeFD fd (handler-data-key handler-data))))

(defun-external IPC_unsubscribeFD (fd fdHandler)
  (let* ((hnd-name (fd-handler-name fd fdHandler))
	 (handler-data (get-handler-data-by-name hnd-name)))
    (cond (handler-data
	   (remove-handler-data-by-key (handler-data-key handler-data))
	   (unsubscribeFD fd))
	  (t (format t "Ooops: ~a not subscribed for fd ~d~%" 
		     (handler-name fdHandler) fd)
	     IPC_Error))))

(defun-external IPC_subscribeConnect (connectHandler
				      &optional (client-data nil))
  ;; Do it this way so that multiple handlers can be subscribed
  (let ((oldLen (length *CONNECT_HANDLERS_LIST*))
	(hndData (find-handler connectHandler *CONNECT_HANDLERS_LIST*)))
    (cond ((null hndData)
	   (add-handler connectHandler *CONNECT_HANDLERS_LIST* client-data))
	  ((not (equal (handler-data-client-data hndData) client-data))
	   (format t "WARNING: Replacing connect handler client data for ~a~%"
		   (handler-name connectHandler))
	   (setf (handler-data-client-data hndData) client-data)))

    (cond ((and (zerop oldLen) (= (length *CONNECT_HANDLERS_LIST*) 1))
	   (subscribeConnect))
	  ((IPC_isConnected) IPC_OK)
	  (t IPC_Error))))

(defun-external IPC_subscribeDisconnect (disconnectHandler
					 &optional (client-data nil))
  ;; Do it this way so that multiple handlers can be subscribed
  (let ((oldLen (length *DISCONNECT_HANDLERS_LIST*))
	(hndData (find-handler disconnectHandler *DISCONNECT_HANDLERS_LIST*)))
    (cond ((null hndData)
	   (add-handler disconnectHandler *DISCONNECT_HANDLERS_LIST*
			client-data))
	  ((not (equal (handler-data-client-data hndData) client-data))
	   (format t "WARNING: Replacing disconnect handler client data for ~a~%"
		   (handler-name disconnectHandler))
	   (setf (handler-data-client-data hndData) client-data)))

    (cond ((and (zerop oldLen) (= (length *DISCONNECT_HANDLERS_LIST*)))
	   (subscribeDisconnect))
	  ((IPC_isConnected) IPC_OK)
	  (t IPC_Error))))

(defun-external IPC_unsubscribeConnect (connectHandler)
  (let ((oldLen (length *CONNECT_HANDLERS_LIST*)))
    (rem-handler connectHandler *CONNECT_HANDLERS_LIST*)
    (cond ((= oldLen (length *CONNECT_HANDLERS_LIST*))
	   (format t "IPC_unsubscribeConnect: Connect handler ~a not found~%"
		   (handler-name connectHandler))
	   IPC_Error)
	  ((zerop (length *CONNECT_HANDLERS_LIST*))
	   (unsubscribeConnect))
	  (t IPC_OK))))

(defun-external IPC_unsubscribeDisconnect (disconnectHandler)
  (let ((oldLen (length *DISCONNECT_HANDLERS_LIST*)))
    (rem-handler disconnectHandler *DISCONNECT_HANDLERS_LIST*)
    (cond ((= oldLen (length *DISCONNECT_HANDLERS_LIST*))
	   (format t "IPC_unsubscribeDisconnect: Disconnect handler ~a not found~%"
		   (handler-name disconnectHandler))
	   IPC_Error)
	  ((zerop (length *DISCONNECT_HANDLERS_LIST*))
	   (unsubscribeDisconnect))
	  (t IPC_OK))))

(defun-external IPC_subscribeHandlerChange (msgName changeHandler
						    &optional (clientData nil))
  ;; Do it this way so that multiple handlers can be subscribed for same message
  (let* ((change-handlers (gethash msgName *CHANGE_HANDLERS_TABLE*))
	 (oldlen (length change-handlers))
	 (hnd-data (find-handler changeHandler change-handlers)))
    (cond ((null change-handlers)
	   (add-handler changeHandler change-handlers clientData))
	  ((null hnd-data)
	   (add-handler changeHandler change-handlers clientData))
	  ((not (equal (handler-data-client-data hnd-data) clientData))
	   (format t "WARNING: Replacing change handler client data for ~a~%"
		   (handler-name changeHandler))
	   (setf (handler-data-client-data hnd-data) clientData)))

    (setf (gethash msgName *CHANGE_HANDLERS_TABLE*) change-handlers)
    (cond ((and (zerop oldlen) (= (length change-handlers) 1))
	   (subscribeHandlerChange msgName))
	  ((IPC_isConnected) IPC_OK)
	  (t IPC_Error))))

(defun-external IPC_unsubscribeHandlerChange (msgName changeHandler)
  (let* ((change-handlers (gethash msgName *CHANGE_HANDLERS_TABLE*))
	 (oldlen (length change-handlers))
	 (hnd-data (find-handler changeHandler change-handlers)))
    (cond ((zerop oldlen)
	   (format t "No change handler found for message ~a~%" msgName)
	   IPC_Error)
	  ((null hnd-data)
	   (format t "No change handler found for message ~a~%" msgName)
	   IPC_Error)
	  (t (setf (gethash msgName *CHANGE_HANDLERS_TABLE*)
		   (remove hnd-data change-handlers))
	     (cond ((= oldlen 1) (unsubscribeHandlerChange msgName))
		   (t IPC_OK))))))

;;; This is pretty kludgy and inefficient, but it works!
(defun-external IPC_printData (formatter ostream data)
  (let ((vc (new_IPC_VARCONTENT_TYPE))
	(retval IPC_OK))
    (when (and (eql (setf retval (IPC_marshall formatter data vc)) IPC_OK)
	       (eql (setf retval (printData formatter ".pd.tmp" vc)) IPC_OK))
      (with-open-file (istream (open ".pd.tmp" :direction :input))
        (loop with ch = nil
	  while (setf ch (read-char istream nil nil))
	  do (write-char ch ostream)))
      (delete-file ".pd.tmp")
      retval)))

(defun-external IPC_readData (formatter istream)
  (declare (ignore formatter istream))
  (format t "IPC_readData: Not yet implemented~%")
  IPC_Error)

(defun-external IPC_queryResponse (msgName length content timeout)
  (let* ((vc (new_IPC_VARCONTENT_TYPE))
	 (replyFormat (new_FORMAT_CONTAINER_TYPE))
	 (ret (queryResponse msgName length content vc replyFormat timeout)))
    (values (IPC_VARCONTENT_TYPE_content vc) ret)))

(defun-external IPC_queryResponseVC (msgName varcontent timeout)
  (IPC_queryResponse msgName (IPC_VARCONTENT_TYPE_length varcontent)
		     (IPC_VARCONTENT_TYPE_content varcontent) timeout))

(defun-external IPC_queryResponseData (msgName data timeoutMSecs)
  (let ((varcontent (new_IPC_VARCONTENT_TYPE))
	(responseObject nil)
	(ret IPC_OK))
    (setf ret (IPC_marshall (IPC_msgFormatter msgName) data varcontent))
    (when (eql ret IPC_OK)
      (let ((vc (new_IPC_VARCONTENT_TYPE))
	    (replyFormat (new_FORMAT_CONTAINER_TYPE)))
	(setf ret (queryResponse msgName (IPC_VARCONTENT_TYPE_length varcontent)
				 (IPC_VARCONTENT_TYPE_content varcontent)
				 vc replyFormat timeoutMSecs))
	(freeByteArray varcontent)
	(when (eql ret IPC_OK)
	  (let ((formatter (FORMAT_CONTAINER_TYPE_format replyFormat)))
	    (setf responseObject 
		  (make-data-struct 
		   (getMsgClass (FORMAT_CONTAINER_TYPE_msgName replyFormat)
				formatter)))
	    (setf ret (IPC_unmarshall formatter (IPC_VARCONTENT_TYPE_content vc)
				      responseObject))))))
    (values responseObject ret)))

(defun-external IPC_msgClass (msgName classSymbol)
  (setf (gethash msgName *MSG_CLASS_TABLE*) classSymbol))

(defun-external IPC_respondData (msgInstance msgName data)
  (let ((vc (new_IPC_VARCONTENT_TYPE)))
    (cond ((not (eql (IPC_marshall (IPC_msgFormatter msgName) data vc) IPC_OK))
	   IPC_Error)
	  (t (let ((retVal (IPC_respondVC msgInstance msgName vc)))
	       (freeByteArray vc)
	       retVal)))))

(defun-external IPC_queryNotify (msg-name length content handler
					  &optional (client-data nil))
  (let* ((hnd-name (query-handler-name msg-name handler))
	 (handler-data (update-handler-data hnd-name handler client-data nil)))
    (queryNotify msg-name length content (handler-data-key handler-data))))

(defun-external IPC_queryNotifyVC (msgName varcontent handler
					   &optional (clientData nil))
  (IPC_queryNotify msgName (IPC_VARCONTENT_TYPE_length varcontent)
		   (IPC_VARCONTENT_TYPE_content varcontent) handler clientData))

(defun-external IPC_queryNotifyData (msgName data handler
					     &optional (clientData nil))
  (let* ((hnd-name (query-handler-name msgName handler))
	 (handler-data (update-handler-data hnd-name handler clientData t)))
    (cond ((null data) (IPC_publish msgName 0 nil))
	  (t (let ((vc (new_IPC_VARCONTENT_TYPE))
		   (retVal IPC_OK))
	       (setq retVal (IPC_marshall (IPC_msgFormatter msgName) data vc))
	       (cond ((eql retVal IPC_OK)
		      (setq retVal (queryNotify msgName 
						(IPC_VARCONTENT_TYPE_length vc)
						(IPC_VARCONTENT_TYPE_content vc)
						(handler-data-key handler-data)))
		      (freeByteArray vc)))
	       retVal)))))

(defun addTimer (tdelay count handler client-data)
  (let* ((hnd-name (timer-handler-name handler))
	 (handler-data (update-handler-data hnd-name handler client-data nil))
	 (timerRef (new_TIMER_REF_CONTAINER_TYPE))
	 (retval (addTimerGetRef tdelay count (handler-data-key handler-data)
				 timerRef)))
    (cond ((eql retval IPC_OK) 
	   (setf (handler-data-timer-ref handler-data)
		 (TIMER_REF_CONTAINER_TYPE_timerRef timerRef))
	   (values (TIMER_REF_CONTAINER_TYPE_timerRef timerRef) IPC_OK))
	  (t (values nil retval)))))

(defun removeTimer (handlerIndex)
  (remove-handler-data-by-key handlerIndex))

(defun _IPC_addTimerGetRef (tdelay count handler clientData timerRefContainer)
  (let* ((hnd-name (timer-handler-name handler))
	 (handler-data (get-handler-data-by-name hnd-name)))
    (when handler-data
      (format t "WARNING: Replacing existing timer for handler ~a~%" handler)
      (removeTimer (handler-data-key handler-data)))
    (multiple-value-bind (timerRef retVal) (addTimer tdelay count handler clientData)
      (when timerRefContainer
	(setf (TIMER_REF_CONTAINER_TYPE_timerRef timerRefContainer) timerRef))
      retVal)))

(defun-external IPC_addTimerGetRef (tdelay count handler clientData 
					   timerRefContainer)
  (_IPC_addTimerGetRef tdelay count handler clientData timerRefContainer))

(defun-external IPC_addTimer (tdelay count handler &optional (clientData nil))
  (_IPC_addTimerGetRef tdelay count handler clientData nil))

(defun-external IPC_addOneShotTimer (tdelay handler &optional (clientData nil))
  (IPC_addTimer tdelay 1 handler clientData))

(defun-external IPC_addPeriodicTimer (tdelay handler
					     &optional (clientData nil))
  (IPC_addTimer tdelay TRIGGER_FOREVER handler clientData))

(defun _IPC_removeByTimerRef (timerRef)
  (let ((handler-data nil))
    (maphash #'(lambda (key hnd-data)
		 (declare (ignore key))
		 (when (eql (handler-data-timer-ref hnd-data) timerRef)
		   (setf handler-data hnd-data)))
	     *KEY_HASH_TABLE*)
    (cond (handler-data
	   (prog1 (_IPC_removeTimerByRef timerRef)
	     (removeTimer (handler-data-key handler-data))))
	  (t (format t "Timer with ref (~a) does not exist~%" timerRef)
	     IPC_OK))))

(defun-external IPC_removeTimerByRef (timerRefContainer)
  (_IPC_removeByTimerRef (TIMER_REF_CONTAINER_TYPE_timerRef timerRefContainer)))

(defun-external IPC_removeTimer (handler)
  (let ((handler-data (get-handler-data-by-name (timer-handler-name handler))))
    (cond (handler-data
	   (prog1 (_IPC_removeByTimerRef (handler-data-timer-ref handler-data))
	     (removeTimer (handler-data-key handler-data))))
	  (t (format t "Timer for handler (~a) does not exist~%"
		     (handler-name handler))
	     IPC_OK))))

(defun-foreign-callable timerCallbackHandler ((hndIndex :int)
					      (currentTime :int)
					      (scheduledTime :int))
  (let ((hnd-data (get-handler-data-by-key hndIndex)))
    (cond (hnd-data
	   (funcall (handler-data-proc hnd-data)
		    (handler-data-client-data hnd-data)
		    currentTime scheduledTime)
	   (when (zerop (maxTriggers (handler-data-timer-ref hnd-data)))
	     (format t "Deleting timer ~a~%" (handler-data-name hnd-data))
	     (removeTimer hndIndex)))
	  (t (format t "Ooops -- no handler for timer~%")))))
