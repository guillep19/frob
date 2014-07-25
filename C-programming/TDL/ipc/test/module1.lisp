;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROJECT: New Millennium, DS1
;;          IPC (Interprocess Communication) Package
;;
;; (c) Copyright 1996 Reid Simmons.  All rights reserved.
;;
;; FILE: module1.lisp
;;
;; ABSTRACT: Test program for IPC.
;;             Publishes: MSG1, QUERY1
;;             Subscribes to: MSG2
;;             Behavior: Sends MSG1 whenever an "m" is typed at the terminal;
;;                       Sends a QUERY1 whenever an "r" is typed;
;;                       Quits the program when a 'q' is typed.
;;                       Should be run in conjunction with module2.
;;
;; $Revision: 2.3 $
;; $Date: 2013/07/24 20:01:01 $
;; $Author: reids $
;;
;; Copyright (c) 2008, Carnegie Mellon University
;;     This software is distributed under the terms of the 
;;     Simplified BSD License (see ipc/LICENSE.TXT)
;; 
;; REVISION HISTORY
;;
;; $Log: module1.lisp,v $
;; Revision 2.3  2013/07/24 20:01:01  reids
;; Updating lisp, java, python test programs to adhere to updated API
;;
;; Revision 2.2  2009/01/12 15:54:58  reids
;; Added BSD Open Source license info
;;
;; Revision 2.1.1.1  1999/11/23 19:07:38  reids
;; Putting IPC Version 2.9.0 under local (CMU) CVS control.
;;
;; Revision 1.1.2.2  1997/01/25 23:18:18  udo
;; ipc_2_6 to r3 merge
;;
;; Revision 1.1.2.1.6.1  1996/12/24 15:25:38  reids
;; Change name of "main" function to enable multiple modules to be loaded
;;   simultaneously
;;
;; Revision 1.1.2.1  1996/10/02 20:38:46  reids
;; Explicitly label functions as "IPC:"
;; Changes to support LISPWORKS.
;;
;; Revision 1.1  1996/06/17 18:43:35  rouquett
;; ipc test files
;;
;; Revision 1.2  1996/03/12 03:06:08  reids
;; Test programs now illustrate use of "enum" format;
;; Handlers now free data.
;;
;; Revision 1.1  1996/03/06 20:19:26  reids
;; New test programs for passing data between C and LISP modules
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;/

;;; Load the common file with all the type and name definitions
(eval-when (load eval)
  (load (make-pathname :directory (pathname-directory *load-truename*)
		       :name "module.lisp")))

(defun msg2Handler (msgRef lispData clientData)
  (format t "msg2Handler: Receiving ~s (~s) [~s]~%" 
	  (IPC:IPC_msgInstanceName msgRef) lispData clientData))

(defun stdinHnd (fd clientData)
  (declare (ignore fd))
  (let ((inputLine (read-line)))
    (case (aref inputLine 0)
      ((#\q #\Q) 
       (IPC:IPC_disconnect)
       #+ALLEGRO (top-level:do-command "reset") #+LISPWORKS (abort)
       )
      ((#\m #\M) 
       (format t "~%  (IPC_publishData ~s ~d)~%" MSG1 42)
       (IPC:IPC_publishData MSG1 42))
      ((#\r #\R) 
       (let ((t1 (make-T1 :i1 666
			  ;; T1 does not support symbolic enums, so have to
			  ;;  use the corresponding integer value
			  :status (position :SendVal STATUS_ENUM)
			  :matrix (make-array '(2 3) 
					      :element-type 'double-float
					      :initial-contents
					      '((0.0d0 1.0d0 2.0d0)
						(1.0d0 2.0d0 3.0d0)))
			  :d1 pi)))
	 (format t "~%  (IPC_queryResponseData ~s ~a r1 IPC_WAIT_FOREVER)~%" 
		 QUERY1 t1)
	 (let ((r1 (IPC:IPC_queryResponseData QUERY1 t1 IPC:IPC_WAIT_FOREVER)))
	   (format t "~%  Received response ~a~%" r1)
	   ;; (IPC:IPC_printData (IPC:IPC_msgFormatter RESPONSE1) t r1Ptr)
	   )))
      (T (format t "stdinHnd [~s]: Received ~s" clientData inputLine)))))

(defun module1 ()

  ;; Connect to the central server
  (format t "~%(IPC_connect ~s)~%" MODULE1_NAME)
  (IPC:IPC_connect MODULE1_NAME)

  ;; Define the named formats that the modules need
  (format t "~%(IPC_defineFormat ~s ~s)~%" T1_NAME T1_FORMAT)
  (IPC:IPC_defineFormat T1_NAME T1_FORMAT)
  (format t "~%(IPC_defineFormat ~s ~s)~%" T2_NAME T2_FORMAT)
  (IPC:IPC_defineFormat T2_NAME T2_FORMAT)
  
  ;; Define the messages that this module publishes
  (format t "~%(IPC_defineMsg ~s IPC_VARIABLE_LENGTH ~s)~%" MSG1 MSG1_FORMAT)
  (IPC:IPC_defineMsg MSG1 IPC:IPC_VARIABLE_LENGTH MSG1_FORMAT)

  (format t "~%(IPC_defineMsg ~s IPC_VARIABLE_LENGTH ~s)~%"
	  QUERY1 QUERY1_FORMAT)
  (IPC:IPC_defineMsg QUERY1 IPC:IPC_VARIABLE_LENGTH QUERY1_FORMAT)

  ;; Subscribe to the messages that this module listens to.
  ;;  NOTE: No need to subscribe to the RESPONSE1 message since it is a
  ;;        response to a query not a regular subscription!
  (format t "~%(IPC_subscribe ~s 'msg2Handler ~s)~%" MSG2 MODULE1_NAME)
  (IPC:IPC_subscribe MSG2 'msg2Handler MODULE1_NAME)
  (IPC:IPC_msgClass RESPONSE1 'T2)

  ;; Subscribe a handler for tty input.
  ;;   Typing "q" will quit the program; Typing "m" will send MSG1;
  ;;   Typing "r" will send QUERY1 ("r" for response)
  ;; NOTE: 0 is the file descriptor number of stdin (the terminal)
  (format t "~%(IPC_subscribeFD ~d 'stdinHnd ~s)~%" 0 MODULE1_NAME)
  (IPC:IPC_subscribeFD 0 'stdinHnd MODULE1_NAME)

  (format t "~%Type 'm' to send ~s; Type 'r' to send ~s; Type 'q' to quit~%"
	  MSG1 QUERY1)

  (IPC:IPC_dispatch)
  )
