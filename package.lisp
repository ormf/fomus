;; -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**************************************************************************************************
;; FOMUS
;; package.lisp
;;**************************************************************************************************

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE

;; (eval-when (:compile-toplevel)
;;   (let ((p (find-package :fomus)))
;;     (when (and p (find-symbol "CM-STUB" p))
;;       (format t "~%~%;;; COMPILING FOMUS WITH COMMON MUSIC LOADED~%")
;;       (format t ";;; IF THIS DOESN'T WORK:~%")
;;       (format t ";;;   1. TRY IT AGAIN--IT MIGHT WORK THE SECOND TIME (IN CMUCL AND SBCL)~%")
;;       (format t ";;;   2. COMPILE IT FIRST WITHOUT CM~%~%"))))
;; (eval-when (:compile-toplevel :load-toplevel)
;;   (let ((p (find-package :fomus)))
;;     (when p
;;       (let ((s (find-symbol "CM-STUB" p))) 
;; 	(when s
;; 	  (unintern s p)
;; 	  (map nil (lambda (x) (fmakunbound (find-symbol x p)))
;; 	       '("OBJ-PARTID" "FOMUS" "MAKE-PART" "MAKE-NOTE" "GET-INSTR-SYMS" "FOMUS-FILE")))))))

(defpackage "FOMUS"
  (:nicknames "FM" "FMS")
  (:use "COMMON-LISP")
  (:documentation "FOMUS is an application for automatic formatting of music notation")
  (:export "FOMUS" "LOAD-INITFILE"	; interface functions
	   "FOMUS-INIT" "FOMUS-NEWTIMESIG" "FOMUS-NEWPART" "FOMUS-NEWMARK" "FOMUS-NEWNOTE" "FOMUS-NEWREST" "FOMUS-EXEC" "FOMUS-PART" "FOMUS-FILE"
	   "LIST-FOMUS-SETTINGS" "LIST-FOMUS-INSTRUMENTS" "LIST-FOMUS-INSTRGROUPS" "LIST-FOMUS-PERCUSSION" "LIST-FOMUS-CLEFS"
	   "LIST-FOMUS-MEAS-DIVS" "LIST-FOMUS-TUPLET-DIVS" "GET-MIDI-INSTR" "IS-INSTR" "IS-CLEF" "GET-INSTR-SYMS" "GET-PERC-SYMS"
	   "FOMUSCHUNK" "DEFFOMUSMODULE" "LOAD-FOMUS-MODULE" "LIST-FOMUS-MODULES" "REGISTER-FOMUS-MODULE" "REGISTER-FOMUS-MODULES"
					; make/copy functions
	   "MAKE-TIMESIG" "MAKE-TIMESIG-REPL" "MAKE-PART" "MAKE-MARK" "MAKE-NOTE" "MAKE-REST" "MAKE-INSTR" "MAKE-PERC" "COPY-INSTR" "COPY-PERC" "MAKE-MEAS"
	   "COPY-TIMESIG" "COPY-TIMESIG-REPL" "COPY-EVENT" "COPY-PART" "COPY-MEAS"
					; type predicates
	   "FOMUSOBJP" "EVENTP" "MARKP" "DURP" "TIMESIGP" "PARTP" "NOTEP" "RESTP" "INSTRP" "PERCP" "MEASP"
					; objects (meas is internal)
	   "FOMUSOBJ-BASE" "EVENT-BASE" "MARK" "DUR-BASE" "TIMESIG-REPL" "TIMESIG" "NOTE" "REST" "PART" "INSTR" "PERC" "MEAS" ;;"KEYSIG"
	   "OBJ-ID" "OBJ-PARTID"
	   "EVENT-OFF" "EVENT-PARTID" "EVENT-PARTIDS"
	   "TIMESIG-TIME" "TIMESIG-DIV" "TIMESIG-COMP" "TIMESIG-BEAT" "TIMESIG-PROPS" "TIMESIG-PARTIDS" "TIMESIG-REPL"
	   "EVENT-DUR" "EVENT-MARKS" "EVENT-VOICE" "EVENT-NOTE"
	   "PART-NAME" "PART-ABBREV" "PART-OPTS" "PART-EVENTS" "PART-INSTR" "PART-PARTID"
					; instruments
	   "INSTR-SYM" "INSTR-CLEFS" "INSTR-STAVES" "INSTR-MINP" "INSTR-MAXP" "INSTR-VOICELIM" "INSTR-TPOSE" "INSTR-LEGLS" "INSTR-8UPLEGLS" "INSTR-8DNLEGLS"
	   "PERC-SYM" "PERC-STAFF" "PERC-NOTE"
					; object extensions
	   "EX-BASE" "NOTEEX" "RESTEX" "PARTEX"
	   "MAKE-NOTEEX" "MAKE-RESTEX" "MAKE-PARTEX"
	   "PART-MEAS"
	   "EVENT-TUP" "EVENT-TIELT" "EVENT-TIERT" "EVENT-BEAMLT" "EVENT-BEAMRT" "EVENT-INV" "PART-PROPS" "PART-USERORD"
	   "MEAS-TIMESIG" "MEAS-OFF" "MEAS-ENDOFF" "MEAS-EVENTS" "MEAS-PROPS" "MEAS-DIV"
					; auxiliary accessors/functions
					; caution: some only apply to OUTPUT .fms file extension objects, others only apply BEFORE calling fomus!
	   "TIMESIG-OFF" "MEAS-VOICES"
	   "ADDPROP" "GETPROP" "GETPROPS" "RMPROP" "COMBPROPS" "ADDMARK" "GETMARK" "GETMARKS" "RMMARK" "COMBMARKS"

	   "+TITLE+" "+VERSION+"))

(in-package :fomus)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GLOBAL

(defmacro compile-settings ()
  '(eval-when (:compile-toplevel)
    #+debug (declaim (optimize (safety 3) (debug 3)))
    #-debug (declaim (optimize (speed 3) (debug 0) (safety 0) #+(and cmu (not warn)) (ext:inhibit-warnings 3))
	     #+(and sbcl (not warn)) (sb-ext:muffle-conditions sb-ext:compiler-note))))
(compile-settings)

(declaim (type (integer 0 2) *verbose*))
(defparameter *verbose* 1)

;; CLISP COMPATIBILITY

#-clisp
(defmacro mloop (&body forms) `(loop ,@forms))
#+clisp
(defmacro mloop (&body forms) `(or (loop ,@forms) 0))

;; PREVENT SILLY CM LOADING ERROR
(intern "KEYSIG")
;; PREVENT WEIRD OPENMCL LOADING ERROR
#+openmcl (eval-when (:load-toplevel :execute) (let ((s (find-symbol "INSTR" :common-lisp-user))) (when s (shadow s :common-lisp-user))))
