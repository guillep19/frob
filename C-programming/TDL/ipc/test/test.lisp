;;; To run:
;;; gmake install in $IPC_DIR/lisp
;;; Afterwards, IPC.lisp (near the end of the file) should include all
;;; the load commands needed, with the correct directory names embedded
;;; in the code.  If you have trouble loading the files, check to ensure
;;; the names are correct.
;;; Run $IPC_DIR/bin/$SYS/central in a separate terminal
;;;  where IPC_DIR is the location of IPC and SYS is the system type
;;;  (e.g., Linux-3.8)
;;; export CENTRALHOST=localhost
;;; Start lisp and do (progn (load "test") (main))
;;; Follow the instructions (you need to enter input and start/stop the
;;;  $IPC_DIR/test/module2 program (which needs to be built first)
;;; You can then compare the output against test.lisp.output.
;;; Except for differences in pointer values, and occasional swaps in which
;;; handler fires first, they should be identical

(eval-when (load eval)
  (load (make-pathname :directory (butlast (pathname-directory *load-truename*))
		     :name "lisp/IPC")))

(use-package :IPC)

(defvar printByteArrayP t)

(defun printVC (vc)
  (when printByteArrayP
    (printByteArray (IPC_VARCONTENT_TYPE_content vc)
		    (IPC_VARCONTENT_TYPE_length vc))))

(IPC_defstruct (sample0)
  (d1 0.0 :type double-float)
  (d2 0.0 :type double-float))

(IPC_defstruct (sample1)
  (c1 nil :type character)
  (c2 nil :type character))

(defun test0 ()
  (IPC_initialize)
  (let ((buf (createBuffer (createByteArray 10)))
	(ds (make-sample0 :d1 666.0)))
    (IPC::DOUBLE_Trans IPC::Encode ds 1 buf)
    (printBuffer buf)
    (rewindBuffer buf)
    (IPC::DOUBLE_Trans IPC::Decode ds 2 buf)
    (format t "~a~%" (sample0-d2 ds))
    (format t "~a~%" (IPC::DOUBLE_Trans IPC::ELength ds 1))
    (format t "~a~%" (IPC::DOUBLE_Trans IPC::SimpleType))
    (let ((ar (make-array '(3) :initial-contents '(1.0 2.0 3.0))))
      (rewindBuffer buf)
      (IPC::DOUBLE_Trans IPC::Encode ar 1 buf)
      (printBuffer buf)
      (rewindBuffer buf)
      (IPC::DOUBLE_Trans IPC::Decode ar 2 buf)
      (format t "~a~%" ar))))

(defun test1 ()
  (IPC_initialize)
  (let ((buf (createBuffer (createByteArray 10)))
	(ds (make-sample1 :c1 #\h)))
    (IPC::CHAR_Trans IPC::Encode ds 1 buf)
    (printBuffer buf)
    (rewindBuffer buf)
    (IPC::CHAR_Trans IPC::Decode ds 2 buf)
    (format t "~a~%" (sample1-c2 ds))
    (format t "~a~%" (IPC::CHAR_Trans IPC::ELength ds 1))
    (format t "~a~%" (IPC::CHAR_Trans IPC::SimpleType))
    (let ((ar (make-array '(3) :initial-contents '(#\e #\m #\c))))
      (rewindBuffer buf)
      (IPC::CHAR_Trans IPC::Encode ar 1 buf)
      (printBuffer buf)
      (rewindBuffer buf)
      (IPC::CHAR_Trans IPC::Decode ar 2 buf)
      (format t "~a~%" ar))))

;;; This tests all the primitives
(defun test2 ()
  (IPC_initialize)
  (let ((fmt1 (IPC_parseFormat "int"))
	(vc1 (new_IPC_VARCONTENT_TYPE)))
    (IPC_marshall fmt1 123 vc1)
    (printVC vc1)
    (format t "~a~%" (IPC_unmarshallData 
		      fmt1 (IPC_VARCONTENT_TYPE_content vc1))))
  
  (let ((fmt3 (IPC_parseFormat "boolean"))
	(vc3 (new_IPC_VARCONTENT_TYPE)))
    (IPC_marshall fmt3 t #|true|# vc3)
    (printVC vc3)
    (format t "~a~%" (IPC_unmarshallData
		      fmt3 (IPC_VARCONTENT_TYPE_content vc3))))

  (let ((fmt4 (IPC_parseFormat "float"))
	(vc4 (new_IPC_VARCONTENT_TYPE)))
    (IPC_marshall fmt4 55.0 vc4)
    (printVC vc4)
    (format t "~a~%" (IPC_unmarshallData
		      fmt4 (IPC_VARCONTENT_TYPE_content vc4))))

  (let ((fmt2 (IPC_parseFormat "double"))
	(vc2 (new_IPC_VARCONTENT_TYPE)))
    (IPC_marshall fmt2 666.0 vc2)
    (printVC vc2)
    (format t "~a~%" (IPC_unmarshallData
		      fmt2 (IPC_VARCONTENT_TYPE_content vc2))))

  (let ((fmt5 (IPC_parseFormat "byte"))
	(vc5 (new_IPC_VARCONTENT_TYPE)))
    (IPC_marshall fmt5 #xA vc5)
    (printVC vc5)
    (format t "~a~%" (IPC_unmarshallData
		      fmt5 (IPC_VARCONTENT_TYPE_content vc5))))

  (let ((fmt5 (IPC_parseFormat "ubyte"))
	(vc5 (new_IPC_VARCONTENT_TYPE)))
    (IPC_marshall fmt5 #xFA vc5)
    (printVC vc5)
    (format t "~a~%" (IPC_unmarshallData
		      fmt5 (IPC_VARCONTENT_TYPE_content vc5))))

  (let ((fmt6 (IPC_parseFormat "string"))
	(vc6 (new_IPC_VARCONTENT_TYPE)))
    (IPC_marshall fmt6 "hello" vc6)
    (printVC vc6)
    (format t "~a~%" (IPC_unmarshallData
		      fmt6 (IPC_VARCONTENT_TYPE_content vc6))))

  (let ((fmt6 (IPC_parseFormat "string"))
	(vc6 (new_IPC_VARCONTENT_TYPE)))
    (IPC_marshall fmt6 "" vc6)
    (printVC vc6)
    (format t "~a~%" (IPC_unmarshallData
		      fmt6 (IPC_VARCONTENT_TYPE_content vc6))))
#|
  (let ((fmt6 (IPC_parseFormat "string"))
	(vc6 (new_IPC_VARCONTENT_TYPE)))
    (IPC_marshall fmt6 nil vc6)
    (printVC vc6)
    (format t "~a~%" (IPC_unmarshallData
		      fmt6 (IPC_VARCONTENT_TYPE_content vc6))))
|#
  (let ((fmt7 (IPC_parseFormat "char"))
	(vc7 (new_IPC_VARCONTENT_TYPE)))
    (IPC_marshall fmt7 #\c vc7)
    (printVC vc7)
    (format t "~a~%" (IPC_unmarshallData
		      fmt7 (IPC_VARCONTENT_TYPE_content vc7))))

  (let ((fmt8 (IPC_parseFormat "short"))
	(vc8 (new_IPC_VARCONTENT_TYPE)))
    (IPC_marshall fmt8 666 vc8)
    (printVC vc8)
    (format t "~a~%" (IPC_unmarshallData
		      fmt8 (IPC_VARCONTENT_TYPE_content vc8))))

  (let ((fmt9 (IPC_parseFormat "long"))
	(vc9 (new_IPC_VARCONTENT_TYPE)))
    (IPC_marshall fmt9 #x7FFFFFFF vc9)
    (printVC vc9)
    (format t "~a~%" (IPC_unmarshallData
		      fmt9 (IPC_VARCONTENT_TYPE_content vc9))))
  )

(IPC_defstruct (struct1)
  (i 0 :type integer)
  (a1 nil :type struct2))

(IPC_defstruct (struct2)
  (str nil :type string)
  (d 0 :type double-float))

;;; test structures
(defun test3 ()
  (IPC_initialize)
  (let ((fmt1 (IPC_parseFormat "{int, {string, double}}"))
	(vc1 (new_IPC_VARCONTENT_TYPE))
	(ds (make-struct1 :i 666 :a1 (make-struct2 :str "hello" :d 3.14159d0))))
    (IPC_marshall fmt1 ds vc1)
    (printVC vc1)
    (format t "~a~%" (IPC_unmarshallData fmt1 (IPC_VARCONTENT_TYPE_content vc1)
					 'struct1))
					 
    (let* ((ds2 (make-struct2 :str "eat more spam" :d 9.87654321d0))
	   (ds1 (make-struct1 :i 1234 :a1 ds2)))
      (IPC_marshall fmt1 ds1 vc1)
      (printVC vc1)
      (let* ((ds1a (make-struct1))
	     (ret (IPC_unmarshall fmt1 (IPC_VARCONTENT_TYPE_content vc1) ds1a)))
	(format t "~a ~a ~a ~a~%" ds1a ret 
		(equalp ds1 ds1a) (equalp ds2 (struct1-a1 ds1a))))

      ;; Should raise an error
      (handler-case (IPC_unmarshall fmt1 (IPC_VARCONTENT_TYPE_content vc1) ds2)
		    (error (condition) (format t "~a~%" condition)))
      
      )))

;;; test fixed arrays
(defun test4 ()
  (IPC_initialize)
  (let ((fmt1 (IPC_parseFormat "[int : 5]"))
	(vc1 (new_IPC_VARCONTENT_TYPE))
	(ds1 (make-array 5 :initial-contents '(10 11 12 13 14)))
	ds1a)
    (IPC_marshall fmt1 ds1 vc1)
    (printVC vc1)
    (IPC_unmarshall fmt1 (IPC_VARCONTENT_TYPE_content vc1) ds1a)
    (format t "~a~%" ds1a))

  (let ((fmt2 (IPC_parseFormat "[{string, double} : 5]"))
	(vc2 (new_IPC_VARCONTENT_TYPE))
	(ds2 (make-array 5 :initial-contents 
			 `(,(make-struct2 :str "eat" :d (expt 0 3))
			   ,(make-struct2 :str "more" :d (expt 1 3))
			   ,(make-struct2 :str "spam" :d (expt 2 3))
			   ,(make-struct2 :str "for"  :d (expt 3 3))
			   ,(make-struct2 :str "life" :d (expt 4 3))))))
    (IPC_marshall fmt2 ds2 vc2)
    (printVC vc2)
    (format t "~a~%" (IPC_unmarshallData
		      fmt2 (IPC_VARCONTENT_TYPE_content vc2))))

  (let ((fmt3 (IPC_parseFormat "[int : 3, 4]"))
	(vc3 (new_IPC_VARCONTENT_TYPE))
	(ds3 (make-array '(3 4)))
	(ds3a (make-array '(3 4))))
    (dotimes (i 3)
      (setf (aref ds3 i 0) (expt (+ i 1) 2))
      (setf (aref ds3 i 1) (+ (expt (+ i 1) 2) 1))
      (setf (aref ds3 i 2) (+ (expt (+ i 1) 2) 2))
      (setf (aref ds3 i 3) (+ (expt (+ i 1) 2) 3)))
    (IPC_marshall fmt3 ds3 vc3)
    (printVC vc3)
    (IPC_unmarshall fmt3 (IPC_VARCONTENT_TYPE_content vc3) ds3a)
    (format t "~a~%" ds3a))

  (let ((fmt4 (IPC_parseFormat "[double : 3, 4]"))
	(vc4 (new_IPC_VARCONTENT_TYPE))
	(ds4 (make-array '(3 4))))
    (dotimes (i 3)
      (dotimes (j 4)
	(setf (aref ds4 i j) (+ (expt (1+ i) 2) (expt (1+ j) 2) 4))))
    (IPC_marshall fmt4 ds4 vc4)
    (printVC vc4)
    (format t "~a~%" (IPC_unmarshallData
		      fmt4 (IPC_VARCONTENT_TYPE_content vc4))))
  )

(IPC_defstruct (struct4)
  (num 0 :type integer)
  (ar nil :type array))

(IPC_defstruct (struct5)
  (ar nil :type struct2)
  (num 0 :type integer))

(IPC_defstruct (struct6)
  (dim1 0 :type integer)
  (dim2 0 :type integer)
  (ar nil :type array))

;;; test variable arrays
(defun test5 ()
  (IPC_initialize)
  (let ((fmt1 (IPC_parseFormat "{int, <int : 1>}"))
	(vc1 (new_IPC_VARCONTENT_TYPE))
	(ds1 (make-struct4 :num 5 :ar (make-array 5 :initial-contents
						  '(111 112 113 114 115))))
	(ds1a (make-struct4)))
    (IPC_marshall fmt1 ds1 vc1)
    (printVC vc1)
    (IPC_unmarshall fmt1 (IPC_VARCONTENT_TYPE_content vc1) ds1a)
    (format t "~a~%" ds1a))

  (let ((fmt2 (IPC_parseFormat "{<{string, double} : 2>, int}"))
	(vc2 (new_IPC_VARCONTENT_TYPE))
	(ds2 (make-struct5
	      :num 5
	      :ar (make-array 5 :initial-contents
			      `(,(make-struct2 :str "eat" :d (expt 0 3))
				,(make-struct2 :str "more" :d (expt 1 3))
				,(make-struct2 :str "spam" :d (expt 2 3))
				,(make-struct2 :str "for"  :d (expt 3 3))
				,(make-struct2 :str "life" :d (expt 4 3)))))))
    (IPC_marshall fmt2 ds2 vc2)
    (printVC vc2)
    (format t "~a~%"
	    (IPC_unmarshallData fmt2 (IPC_VARCONTENT_TYPE_content vc2))))

  (let* ((fmt3 (IPC_parseFormat "{int, int, <int : 1, 2>}"))
	 (vc3 (new_IPC_VARCONTENT_TYPE))
	 (ar (make-array '(3 4)))
	 (ds3 (make-struct6 :dim1 3 :dim2 4 :ar ar))
	 (ds3a (make-struct6)))
    (dotimes (i 3)
      (setf (aref ar i 0) (+ (expt (1+ i) 2) 0))
      (setf (aref ar i 1) (+ (expt (1+ i) 2) 1))
      (setf (aref ar i 2) (+ (expt (1+ i) 2) 2))
      (setf (aref ar i 3) (+ (expt (1+ i) 2) 3)))
    (IPC_marshall fmt3 ds3 vc3)
    (printVC vc3)
    (IPC_unmarshall fmt3 (IPC_VARCONTENT_TYPE_content vc3) ds3a)
    (format t "~a~%" ds3a)
    (format t "~a~%" (IPC_unmarshallData
		      fmt3 (IPC_VARCONTENT_TYPE_content vc3) 'struct6)))
  )

(deftype ComVal () '(member :WaitVal :SendVal :ReceiveVal :ListenVal))
(IPC_defstruct (struct7)
  (i1 0 :type integer)
  (status 0 :type 'ComVal))

;;; test enums
(defun test6 ()
  (IPC_initialize)
  (let ((fmt1 (IPC_parseFormat "{int, {enum WaitVal, SendVal, ReceiveVal, ListenVal}}"))
	(vc1 (new_IPC_VARCONTENT_TYPE))
	(ds1 (make-struct7 :i1 42 :status 2))
	(ds1a (make-struct7)))
    (IPC_marshall fmt1 ds1 vc1)
    (printVC vc1)
    (IPC_unmarshall fmt1 (IPC_VARCONTENT_TYPE_content vc1) ds1a)
    (format t "~a~%" ds1a))

  (let ((fmt2 (IPC_parseFormat "[{enum WaitVal, SendVal, ReceiveVal, ListenVal} : 3]"))
	(vc2 (new_IPC_VARCONTENT_TYPE))
	(ds2 (make-array 3 :initial-contents '(:SendVal :ListenVal :WaitVal))))
    (IPC_marshall fmt2 ds2 vc2)
    (printVC vc2)
    (format t "~a~%" (IPC_unmarshallData
		      fmt2 (IPC_VARCONTENT_TYPE_content vc2) 'struct7)))

  (let ((fmt3 (IPC_parseFormat "{int, {enum : 4}}"))
	(vc3 (new_IPC_VARCONTENT_TYPE))
	(ds3 (make-struct7 :i1 42 :status 2)))
    (IPC_marshall fmt3 ds3 vc3)
    (printVC vc3)
    (format t "~a~%" (IPC_unmarshallData
		      fmt3 (IPC_VARCONTENT_TYPE_content vc3) 'struct7)))
  )

;;; test pointers
(defun test7 ()
  (IPC_initialize)
  (let ((fmt1 (IPC_parseFormat "{int, {string, double}}"))
	(vc1 (new_IPC_VARCONTENT_TYPE))
	(ds1 (make-struct1 :i 666 :a1 (make-struct2 :str "hello"
						    :d 3.14159d0)))
	(ds1a (make-struct1)))
    (IPC_marshall fmt1 ds1 vc1)
    (printVC vc1)
    (IPC_unmarshall fmt1 (IPC_VARCONTENT_TYPE_content vc1) ds1a)
    (format t "~a~%" ds1a))

  (let ((fmt2 (IPC_parseFormat "{*int, *{string, double}}"))
	(vc2 (new_IPC_VARCONTENT_TYPE))
	(ds2 (make-struct1 :i 666 :a1 (make-struct2 :str "hello"
						    :d 3.14159d0)))
	ds2a)
    (IPC_marshall fmt2 ds2 vc2)
    (printVC vc2)
    (IPC_unmarshall fmt2 (IPC_VARCONTENT_TYPE_content vc2) ds2a)
    (format t "~a~%" ds2a))

  (let ((fmt3 (IPC_parseFormat "{*int, *{string, double}}"))
	(vc3 (new_IPC_VARCONTENT_TYPE))	
	(ds3 (make-struct1 :i 666 :a1 nil)))
    (IPC_marshall fmt3 ds3 vc3)
    (printVC vc3)
    (format t "~a~%" (IPC_unmarshallData
		      fmt3 (IPC_VARCONTENT_TYPE_content vc3) 'struct1)))
  )

(defun test8 ()
  (IPC_initialize)
  (format t "~a~%" (IPC_parseFormat ""))
  (format t "~a~%" (IPC_parseFormat nil)))

(defun msgHandler1 (msgInstance  data clientData)
  (format t "msgHandler1: ~a ~a ~a~%"
	  (IPC_msgInstanceName msgInstance) data clientData))

(defun msgHandler2 (msgInstance data clientData)
  (format t "msgHandler2: ~a ~a ~a ~a~%"
	  (IPC_msgInstanceName msgInstance) data clientData
	  (IPC_dataLength msgInstance)))

(defun test9 ()
  (IPC_connect "test")
  (IPC_defineMsg "f" IPC_VARIABLE_LENGTH "int")
  (IPC_msgClass "f" 'integer)
  (IPC_subscribeData "f" #'msgHandler1 1)
  (IPC_listenWait 100) ;; Seems to be a timing issue here, sometimes
  (IPC_publishData "f" 42)
  (IPC_listenWait 500)
  (format t "~%")

  (IPC_subscribeData "f" #'msgHandler2 3)
  (IPC_publishData "f" 666)
  (IPC_listenWait 500)
  (format t "~%")

  (IPC_subscribeData "f" #'msgHandler1 2)
  (format t "Num handlers: ~a~%" (IPC_numHandlers "f"))
  (IPC_publishData "f" 1234)
  (IPC_listenWait 500)
  (format t "~%")

  (IPC_unsubscribe "f" #'msgHandler2)
  (format t "Num handlers: ~a~%" (IPC_numHandlers "f"))
  (let ((vc (new_IPC_VARCONTENT_TYPE)))
    (IPC_marshall (IPC_msgFormatter "f") 4321 vc)
    (IPC_publishVC "f" vc)
    (IPC_listenWait 500))
  (IPC_disconnect))

(defvar exit nil)

(defun stdinHnd (fd clientData)
  (declare (ignore clientData))
  (let ((msg (read-line)))
    (let ((char (char msg 0)))
      (cond ((member char '(#\? #\h #\H))
	     (format t "h: help~%")
	     (format t "q: quit~%")
	     (format t "u: stop listening~%"))
	    ((member char '(#\q #\Q))
	     (format t "quit~%")
	     (setq exit t))
	    ((member char '(#\u #\U))
	     (format t "silent~%")
	     (IPC_unsubscribeFD fd #'stdinHnd))
	    (t (format t "Unhandled input: ~a~%" msg))))))

(defun test10 ()
  (IPC_connect "test")
  (IPC_subscribeFD 0 #'stdinHnd)
  (setq exit nil)
  (format t "Please type -- either 'h' or 'q', end with a 'q'~%")
  (while (not exit) (IPC_listen 1000))
  (IPC_disconnect))

(defun test11 ()
  (IPC_connect "test")
  (IPC_perror "Test")
  (IPC_setCapacity 3)
  (IPC_defineMsg "f" IPC_VARIABLE_LENGTH"int")
  (IPC_setMsgQueueLength "f" 1)
  (IPC_setMsgPriority "f" 2)
  (IPC_setVerbosity IPC_Print_Errors)
  (IPC_disconnect)
  (IPC_defineMsg "h" IPC_VARIABLE_LENGTH"int")
  (format t "HERE~%")
  (IPC_disconnect))

(defun test12 ()
  (IPC_connect "test")
  (IPC_defineMsg "f" IPC_VARIABLE_LENGTH "int")
  (IPC_defineMsg "h" IPC_VARIABLE_LENGTH "int")
  (format t "Here1~%")
  (IPC_subscribeData "f" #'msgHandler1 1)
  (format t "Here2~%")
  (IPC_subscribeData "f" #'msgHandler1 2)
  (format t "Here3~%")
  (IPC_subscribeData "g" #'msgHandler1 1)
  (format t "Here4~%")
  (IPC_disconnect))

(defun msgHandler3 (msgInstance byteArray clientData)
  (let ((data (IPC_unmarshallData (IPC_msgFormatter "g") byteArray)))
    (format t "msgHandler3: ~a ~a ~a ~a~%" (IPC_msgInstanceName msgInstance)
	    data clientData (IPC_dataLength msgInstance))
    (IPC_freeByteArray byteArray)))

(defun test13 ()
  (IPC_connect "test")
  (IPC_defineMsg "f" 4 nil)
  (IPC_subscribe "f" #'msgHandler3) ;; optional client data
  (IPC_defineMsg "g" 4 "int")
  (IPC_listenWait 100) ;; Seems to be a timing issue here, sometimes
  (let ((vc (new_IPC_VARCONTENT_TYPE)))
    (IPC_marshall (IPC_parseFormat "int") 1234 vc)
    (IPC_publish "f" (IPC_VARCONTENT_TYPE_length vc)
		 (IPC_VARCONTENT_TYPE_content vc))
    (IPC_listenWait 500)
    (IPC_publishVC "f" vc)
    (IPC_listenWait 500)
    (IPC_publishFixed "f" (IPC_VARCONTENT_TYPE_content vc))
    (IPC_listenWait 500)
    (IPC_disconnect)))

(defvar connected nil)
(defvar disconnected nil)

(defun connectHandler1 (moduleName clientData)
  (setq connected t)
  (format t "connectHandler1: ~a ~a~%" moduleName clientData))

(defun connectHandler2 (moduleName clientData)
  (format t "connectHandler2: ~a ~a~%" moduleName clientData))

(defun disconnectHandler1 (moduleName clientData)
  (setq disconnected t)
  (format t "disconnectHandler1: ~a ~a~%" moduleName clientData))

(defun disconnectHandler2 (moduleName clientData)
  (format t "disconnectHandler2: ~a ~a~%" moduleName clientData))

(defun test14 ()
  (IPC_connect "test")
  (IPC_subscribeConnect #'connectHandler1)
  (IPC_subscribeDisconnect #'disconnectHandler1)
  (setq connected nil disconnected nil)
  (format t "Please start, then quit from, test/module2~%")
  (while (or (not connected) (not disconnected)) (IPC_listen 1000))
  (format t "HERE1~%")

  (IPC_subscribeConnect #'connectHandler1 1)
  (IPC_subscribeConnect #'connectHandler2)
  (IPC_subscribeDisconnect #'disconnectHandler1 1)
  (IPC_subscribeDisconnect #'disconnectHandler2)  
  (setq connected nil disconnected nil)
  (format t "Please start, then quit from, test/module2~%")
  (while (or (not connected) (not disconnected)) (IPC_listen 1000))
  (format t "HERE2~%")

  (IPC_unsubscribeConnect #'connectHandler2)
  (IPC_unsubscribeDisconnect #'disconnectHandler2)  
  (setq connected nil disconnected nil)
  (format t "Please start, then quit from, test/module2~%")
  (while (or (not connected) (not disconnected)) (IPC_listen 1000))
  (format t "HERE3~%")

  (IPC_unsubscribeConnect #'connectHandler1)
  (setq connected t disconnected nil)
  (format t "Please start, then quit from, test/module2~%")
  (format t "  (shouldn't do anything on connection, this time)~%")
  (while (or (not connected) (not disconnected)) (IPC_listen 1000))
  (format t "HERE4~%")
  (IPC_disconnect))

(defvar changed nil)

(defun changeHandler1 (msgName numHandlers clientData)
  (setq changed t)
  (format t "changeHandler1: ~a ~a ~a~%"  msgName numHandlers clientData))

(defun changeHandler2 (msgName numHandlers clientData)
  (format t "changeHandler2: ~a ~a ~a~%"  msgName numHandlers clientData))

(defun test15 ()
  (IPC_connect "test")
  (IPC_defineMsg "message1" IPC_VARIABLE_LENGTH "int")
  (IPC_subscribeHandlerChange "message1" #'changeHandler1)
  (setq changed nil)
  (format t "Please start test/module2~%")
  (while (not changed) (IPC_listen 1000))
  (format t "HERE1~%")

  (IPC_subscribeHandlerChange "message1" #'changeHandler1 1)
  (IPC_subscribeHandlerChange "message1" #'changeHandler2)
  (setq changed nil)
  (format t "Please quit from test/module2~%")
  (while (not changed) (IPC_listen 1000))
  (format t "HERE2~%")

  (IPC_unsubscribeHandlerChange "message1" #'changeHandler2)
  (setq changed nil)
  (format t "Please start test/module2~%")
  (while (not changed) (IPC_listen 1000))
  (format t "HERE3~%")

  (IPC_subscribeDisconnect #'disconnectHandler1 123)
  (IPC_unsubscribeHandlerChange "message1" #'changeHandler1)
  (setq disconnected nil)
  (format t "Please quit from test/module2~%")
  (format t "  (shouldn't call the change handler, this time)~%")
  (while (not disconnected) (IPC_listen 1000))
  (format t "HERE4~%")

  (IPC_disconnect))

;;; Test named formatters...
;;; Need to be connected to IPC central to test this...
(defun test16 ()
  (IPC_connect "test")
  (IPC_defineMsg "h" IPC_VARIABLE_LENGTH "{int, fooType}")
  (IPC_defineFormat "fooType" "{string, double}")
  (IPC_subscribeData "h" #'msgHandler2)
  (let ((ds1 (make-struct1 :i 666 :a1 (make-struct2 :str "hello" :d 3.14159d0))))
    (IPC_setVerbosity IPC_Print_Errors)
    (format t "Do formats match? {int, {string, double}}: ~a~%" 
	    (IPC_checkMsgFormats "h" "{int, {string, double}}"))
    (format t "Do formats match? {int, fooType}: ~a~%"
	    (IPC_checkMsgFormats "h" "{int, fooType}"))
    (format t "Do formats match? {int, double}: ~a~%"
	    (IPC_checkMsgFormats "h" "{int, double}"))
    (IPC_listenWait 100) ;; Seems to be a timing issue here, sometimes
    (IPC_publishData "h" ds1)
    (IPC_listenWait 500)
    (IPC_msgClass "h" 'struct1)
    (IPC_publishData "h" ds1)
    (IPC_listenWait 500)
    (format t "printData: ")
    (IPC_printData (IPC_msgFormatter "h") *standard-output* ds1)
    (IPC_disconnect)))

(defun msgHandler4 (msgInstance data clientData)
  (format t "msgHandler4: ~a ~a ~a~%" (IPC_msgInstanceName msgInstance)
	  data clientData)
  (let ((vc (new_IPC_VARCONTENT_TYPE)))
    (IPC_marshall (IPC_msgFormatter "g") 9876 vc)
    (IPC_respondVC msgInstance "g" vc)))

(defun msgHandler5 (msgInstance data clientData)
  (format t "msgHandler5: ~a ~a ~a~%" (IPC_msgInstanceName msgInstance)
	  data clientData)
  (cond ((not (= (struct1-i data) 0)) (IPC_respondData msgInstance "g" 9876))
	(t (IPC_respondData msgInstance "k" data))))

(defun msgHandler6 (msgInstance data clientData)
  (format t "msgHandler6: ~a ~a ~a~%" (IPC_msgInstanceName msgInstance)
	  data clientData))

(defun test17 ()
  (IPC_connect "test")
  (IPC_defineMsg "f" IPC_VARIABLE_LENGTH "{int, {string, double}}")
  (IPC_defineMsg "g" IPC_VARIABLE_LENGTH "int")
  (IPC_defineMsg "h" IPC_VARIABLE_LENGTH nil)
  (IPC_defineMsg "k" IPC_VARIABLE_LENGTH "{int, {string, double}}")
  (IPC_msgClass "f" 'struct1)
  (IPC_msgClass "g" 'integer)
  (IPC_msgClass "k" 'struct1)
  (IPC_subscribeData "f" #'msgHandler4 nil)
  (IPC_subscribeData "g" #'msgHandler2 nil)
  (let ((vc (new_IPC_VARCONTENT_TYPE))
	(ds (make-struct1 :i 666 :a1 (make-struct2 :str "hello" :d 3.14159d0))))
    (IPC_marshall (IPC_msgFormatter "f") ds vc)
    (let* ((byteArray (IPC_queryResponseVC "f" vc IPC_WAIT_FOREVER))
	   (obj (IPC_unmarshallData (IPC_msgFormatter "g") byteArray 'integer)))
      (IPC_freeByteArray byteArray)
      (format t "Reply from query: ~a~%" obj))

    (IPC_publishData "g" 4321)
    (IPC_listenWait 500)

    (IPC_unsubscribe "f" #'msgHandler4)
    (IPC_subscribeData "f" #'msgHandler5 nil)
    (let ((obj (IPC_queryResponseData "f" ds 5000)))
      (format t "IPC_queryResponseData: ~a~%" obj))

    (IPC_subscribeData "h" #'msgHandler6 999)
    (IPC_listenWait 100) ;; Seems to be a timing issue here, sometimes
    (IPC_publishData "h" nil)
    (IPC_listenWait 500)

    (setf (struct1-i ds) 0)
    (let ((obj (IPC_queryResponseData "f" ds 5000)))
      (format t "IPC_queryResponseData: ~a~%" obj))
    (IPC_disconnect)))

(defun queryHandler1 (msgInstance byteArray clientData)
  (declare (ignore msgInstance))
  (let ((obj (IPC_unmarshallData (IPC_msgFormatter "g") byteArray 'integer)))
    (IPC_freeByteArray byteArray)
    (format t "queryHandler1: Reply from query: ~a ~a~%" obj clientData)))

(defun queryHandler2 (msgInstance data clientData)
  (declare (ignore msgInstance))
  (format t "queryHandler2: Reply from query: ~a ~a~%" data clientData))

(defun test18 ()
  (IPC_connect "test")
  (IPC_defineMsg "f" IPC_VARIABLE_LENGTH "{int, {string, double}}")
  (IPC_msgClass "f" 'struct1)
  (IPC_defineMsg "g" IPC_VARIABLE_LENGTH "int")
  (IPC_msgClass "g" 'integer)
  (IPC_subscribeData "f" #'msgHandler4 nil)
  (let ((vc (new_IPC_VARCONTENT_TYPE))
	(ds (make-struct1 :i 666 :a1 (make-struct2 :str "hello" :d 3.14159d0))))
    (IPC_marshall (IPC_msgFormatter "f") ds vc)
    (IPC_queryNotifyVC "f" vc #'queryHandler1 1234)
    (IPC_listenWait 500)

    (IPC_queryNotifyData "f" ds #'queryHandler2 1234)
    (IPC_listenWait 500)
    (IPC_disconnect)))

(defstruct doneCount (done nil) (count 0))

(defun timerHnd1 (doneCount currentTime scheduledTime)
  (declare (ignore currentTime scheduledTime))
  (incf (doneCount-count doneCount))
  (setf (doneCount-done doneCount) (= (doneCount-count doneCount) 3))
  (format t "timerHnd1: ~a ~a~%"
	  (doneCount-count doneCount) (doneCount-done doneCount)))

(defun timerHnd2 (clientData currentTime scheduledTime)
  (declare (ignore currentTime scheduledTime))
  (format t "timerHnd2: ~a~%" clientData))

(defun timerHnd3 (clientData currentTime scheduledTime)
  (declare (ignore currentTime scheduledTime))
  (format t "timerHnd3: ~a~%" clientData))

(defun timerHnd4 (clientData currentTime scheduledTime)
  (declare (ignore currentTime scheduledTime))
  (format t "timerHnd4: ~a~%" clientData))

(defun test19 ()
  (let ((doneCount (make-doneCount)))
    (IPC_connect "test")
    (IPC_addTimer 1000 3 #'timerHnd1 doneCount)
    (let ((ref (new_TIMER_REF_CONTAINER_TYPE)))
      (IPC_addTimerGetRef 500 20 #'timerHnd2 doneCount ref)
      (IPC_addPeriodicTimer 1250 #'timerHnd3 1)
      (while (not (doneCount-done doneCount)) (IPC_listen 1000))
      (IPC_removeTimer #'timerHnd4)
      (IPC_removeTimerByRef ref)
      (IPC_removeTimerByRef ref)
      (IPC_removeTimer #'timerHnd3)
      (IPC_addOneShotTimer 500 #'timerHnd4 666)
      (IPC_listen 1000)
      (IPC_disconnect))))

(defun main ()
  (format t "~%test0~%") (test0)
  (format t "~%test1~%") (test1)
  (format t "~%test2~%") (test2)
  (format t "~%test3~%") (test3)
  (format t "~%test4~%") (test4)
  (format t "~%test5~%") (test5)
  (format t "~%test6~%") (test6)
  (format t "~%test7~%") (test7)
  (format t "~%test8~%") (test8)
  (format t "~%test9~%") (test9)
  (format t "~%test10~%") (test10)
  (format t "~%test11~%") (test11)
  (format t "~%test12~%") (test12)
  (format t "~%test13~%") (test13)
  (format t "~%test14~%") (test14)
  (format t "~%test15~%") (test15)
  (format t "~%test16~%") (test16)
  (format t "~%test17~%") (test17)
  (format t "~%test18~%") (test18)
  (format t "~%test19~%") (test19))
