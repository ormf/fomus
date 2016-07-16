;; -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**************************************************************************************************
;; FOMUS
;; classes.lisp
;;**************************************************************************************************

(in-package :fomus)
(compile-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLASSES

(defclass fomusobj-base ()
  ((id :accessor obj-id :initform nil :initarg :id))) ; fomus doesn't use this!
(defclass event-base (fomusobj-base)
  ((off :type (or (real 0) null) :accessor event-off :initform nil :initarg :off)
   (partid :type (or symbol real null) :accessor event-partid :initform nil :initarg :partid)))

(defclass timesig-repl (fomusobj-base)
  ((time :type cons :accessor timesig-time :initform '(4 4) :initarg :time)
   (div :type list :accessor timesig-div :initform nil :initarg :div) ; list of divisions to force, ex: '((3 3 2) (3 2 3)) or '((3/2 1) (1 3/2))
   (comp :type (or boolean symbol) :accessor timesig-comp :initform 'default :initarg :comp) ; t or nil
   (beat :type (or (rational 0) null) :accessor timesig-beat :initform nil :initarg :beat) ; what actually gets the beat (ex: 1/4 = quarter note, 1/4 + 1/8 = dotted quarter), compound is deterined from signature
   (props :type list :accessor timesig-props :initform nil :initarg :props))) 
(defclass timesig (timesig-repl event-base)
  ((off :type (rational 0))
   (partid :type (or symbol real list) :accessor timesig-partids :initform nil :initarg :partids) ; list of part ids, nil = default (all parts)
   (repl :type (or timesig-repl list) :accessor timesig-repl :initform nil :initarg :repl))) ; replacement time signatures for before meter change (nil = generate automatically)

(defclass mark (event-base) ; these just get dumped into notes after voices are assigned!--voice is like rest voice, list indicates all voices get mark
  ((off :type (or (real 0) cons))
   (marks :type list :accessor event-marks :initform nil :initarg :marks)
   (voice :type (or (integer 1) cons) :accessor event-voice :initform 1 :initarg :voice))) ; 

(defclass dur-base (mark)
  ((dur :type (or real symbol cons) :accessor event-dur :initform 1 :initarg :dur))) ; rational number or (num . grace), grace = integer <0 if w/ slash (in some situations, effects how algorithm sees vertical alignment)
(defclass note (dur-base)
  ((note :type (or real symbol cons) :accessor event-note :initform nil :initarg :note))) ; number, symbol, or cons of note num/sym and accidental: -1, 0 or 1 (or -2 or 2), or list of possibilities
#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:with-unlocked-packages ("COMMON-LISP")
    (defclass rest (dur-base) ()))) ; only w/ xml in special cases--must not overlap a note-event!!!
#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (excl:without-package-locks
    (defclass rest (dur-base) ())))
#+lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((lispworks:*handle-warn-on-redefinition* nil))
    (defclass rest (dur-base) ())))
#+clisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ext:without-package-lock ("COMMON-LISP") (defclass rest (dur-base) ())))
#-(or sbcl allegro lispworks clisp)
(defclass rest (dur-base) ())

(defclass part (fomusobj-base)
  ((name :type (or string null) :accessor part-name :initform nil :initarg :name) ; string
   (abbrev :type (or string null) :accessor part-abbrev :initform nil :initarg :abbrev) ; abbreviated name
   (opts :type list :accessor part-opts :initform nil :initarg :opts) ; arguments in form of keyword-pair lambda list--passed to backends
   (events :type list :accessor part-events :initform nil :initarg :events) ; list of note-event objects (or rest-events)
   (instr :type (or symbol (integer 0 127) instr cons) :accessor part-instr :initform nil :initarg :instr) ; symbol name for instrument lookup (like :flute)
   (props :type list :accessor part-props :initform nil :initarg :props)
   (partid :type (or symbol real) :accessor part-partid :initform nil :initarg :partid))) ; for matching parts with timesigs and events

(defclass meas (fomusobj-base)
  ((timesig :type timesig-repl :accessor meas-timesig :initform nil :initarg :timesig)
   (off :type (rational 0) :accessor meas-off :initform nil :initarg :off)
   (endoff :type (rational 0) :accessor meas-endoff :initform nil :initarg :endoff)
   (events :type list :accessor meas-events :initform nil :initarg :events)
   (props :type list :accessor meas-props :initform nil :initarg :props)
   (div :type list :accessor meas-div :initform nil :initarg :div)))

(defprint-class timesig-repl id time comp beat div props)
(defprint-class timesig id (partid :partids) off time comp beat div repl props)
(defprint-class mark id partid off voice marks)
(defprint-class note id partid voice off dur note marks)
#+allegro (eval-when (:compile-toplevel :load-toplevel :execute)
	    (excl:without-package-locks
	      (defprint-class rest id partid voice off dur marks)))
#+lispworks (eval-when (:compile-toplevel :load-toplevel :execute)
	      (let ((lispworks:*handle-warn-on-redefinition* nil))
		(defprint-class rest id partid voice off dur marks)))
#-(or allegro lispworks) (defprint-class rest id partid voice off dur marks)
(defprint-class part id partid name abbrev instr events props opts)
(defprint-class meas id off endoff timesig div events props)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UGLIFICATION

;; from text file to internal
(defun uglify (in)
  (if (listp in)
      (let ((x (first in)))
	(if (and (symbolp x) (let ((y (symbol-name x))) (string= y "MAKE-" :end1 (min 5 (length y)))))
	    (apply x (mapcar #'uglify (rest in)))
	    (mapcar #'uglify in)))
      in))
;; from internal to text file--outputs a string
(defun deuglify (out)
  (if (and (symbolp (type-of out)) (eq (symbol-package (type-of out)) (find-package :fomus))) ; a fomus structure
      (format nil "(MAKE-~A)" (out-format out))
      (if (listp out) (princ-to-string (mapcar #'deuglify out)) (prin1-to-string out))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FORMATTING

(defgeneric out-format (ob))
(defmethod out-format ((ob part))
  (format nil "PART ~S~A :name ~S~A :instr ~A~A~A"
	  (part-partid ob) (if (obj-id ob) (format nil " :id ~A" (deuglify (obj-id ob))) "") (part-name ob)
	  (if (part-abbrev ob) (format nil " :abbrev ~S" (part-abbrev ob)) "")
	  (deuglify (part-instr ob)) (if (part-props ob) (format nil " :props ~S" (part-props ob)) "")
	  (if (part-opts ob) (format nil " :opts ~S" (part-opts ob)) "")))
(defmethod out-format ((ob timesig))
  (format nil "TIMESIG~A~A :off ~S :time ~S~A~A~A~A~A"
	  (if (obj-id ob) (format nil " :id ~A" (deuglify (obj-id ob))) "") (if (timesig-partids ob) (format nil " :partids ~S" (timesig-partids ob)) "")
	  (timesig-off ob) (timesig-time ob) (if (timesig-comp ob) (format nil " :comp ~S" (timesig-comp ob)) "")
	  (if (timesig-beat ob) (format nil " :beat ~S" (timesig-beat ob)) "") (if (timesig-div ob) (format nil " :div ~S" (timesig-div ob)) "")
	  (if (timesig-repl ob) (format nil " :repl ~A" (deuglify (timesig-repl ob))) "")
	  (if (timesig-props ob) (format nil " :props ~S" (timesig-props ob)) "")))
(defmethod out-format ((ob timesig-repl))
  (format nil "TIMESIG-REPL~A :time ~S~A~A~A~A"
	  (if (obj-id ob) (format nil " :id ~A" (deuglify (obj-id ob))) "") 
	  (timesig-time ob) (if (timesig-comp ob) (format nil " :comp ~S" (timesig-comp ob)) "")
	  (if (timesig-beat ob) (format nil " :beat ~S" (timesig-beat ob)) "") (if (timesig-div ob) (format nil " :div ~S" (timesig-div ob)) "")
	  (if (timesig-props ob) (format nil " :props ~S" (timesig-props ob)) "")))
(defmethod out-format ((ob note))
  (format nil "NOTE ~S~A :voice ~S :off ~S :dur ~S :note ~S~A"
	  (event-partid ob) (if (obj-id ob) (format nil " :id ~A" (deuglify (obj-id ob))) "")
	  (event-voice ob) (event-off ob) (event-dur ob) (event-note ob) (if (event-marks ob) (format nil " :marks ~S" (event-marks ob)) "")))
(defmethod out-format ((ob rest))
  (format nil "REST ~S~A :voice ~S :off ~S :dur ~S~A"
	  (event-partid ob) (if (obj-id ob) (format nil " :id ~A" (deuglify (obj-id ob))) "") (event-voice ob) (event-off ob)
	  (event-dur ob) (if (event-marks ob) (format nil " :marks ~S" (event-marks ob)) "")))
(defmethod out-format ((ob mark))
  (format nil "MARK ~S~A :voice ~S :off ~S :marks ~S"
	  (event-partid ob) (if (obj-id ob) (format nil " :id ~A" (deuglify (obj-id ob))) "") (event-off ob) (event-voice ob) (event-marks ob)))
(defmethod out-format ((ob t)) (princ-to-string (deuglify ob)))
(defmethod out-format :around ((ob t)) (remove-newlines (call-next-method)))

(defmethod out-format ((ob perc))
  (format nil "PERC ~S :staff ~S :voice ~S :note ~S :autodur ~S~A :midinote-im ~S :midinote-ex ~S"
	  (perc-sym ob) (perc-staff ob) (perc-voice ob) (perc-note ob) (perc-autodur ob)
	  (if (perc-marks ob) (format nil " :marks ~S" (perc-marks ob)) "")
	  (perc-midinote-im ob) (perc-midinote-ex ob)))
(defmethod out-format ((ob instr))
  (format nil "INSTR ~S :clefs ~S :staves ~S :minp ~S :maxp ~S :simultlim ~S :tpose ~S :cleflegls ~S :8uplegls ~S :8dnlegls ~S :percs ~A :midiprgch-im ~S :midiprgch-ex ~S"
	  (instr-sym ob) (instr-clefs ob) (instr-staves ob) (instr-minp ob) (instr-maxp ob) (instr-simultlim ob) (instr-tpose ob)
	  (instr-cleflegls ob) (instr-8uplegls ob) (instr-8dnlegls ob) (deuglify (instr-percs ob)) (instr-midiprgch-im ob) (instr-midiprgch-ex ob)))

(declaim (inline make-timesig make-timesig-repl make-part make-mark make-note make-rest make-meas))
(defun make-timesig (&rest args)
  "Interface function:
Creates a TIMESIG object"
  (apply #'make-instance 'timesig args))
(defun make-timesig-repl (&rest args)
  "Interface function:
Creates a TIMESIG-REPL object (a TIMESIG object without an offset)"
  (apply #'make-instance 'timesig-repl args))
(defun make-part (&rest args)
  "Interface function:
Creates a PART object"
    (apply #'make-instance 'part args))
(defun make-mark (&rest args)
  "Interface function:
Creates a MARK object"
  (apply #'make-instance 'mark args))
(defun make-note (&rest args)
  "Interface function:
Creates a NOTE object"
  (apply #'make-instance 'note args))
(defun make-rest (&rest args)
  "Interface function:
Creates a REST object"
  (apply #'make-instance 'rest args))
(defun make-meas (&rest args)
  "Interface function (advanced/internal usage):
Creates a MEAS object"
  (apply #'make-instance 'meas args))

(declaim (inline notep restp timesigp partp markp durp eventp fomusobjp measp))
(defun notep (ev)
  "Utility function:
Returns T if argument is a NOTE object"
  (typep ev 'note))
(defun restp (ev)
  "Utility function:
Returns T if argument is a REST object"
  (typep ev 'rest))
(defun timesigp (ev)
  "Utility function:
Returns T if argument is a TIMESIG object"
  (typep ev 'timesig-repl))
(defun partp (ev)
  "Utility function:
Returns T if argument is a PART object"
  (typep ev 'part))
(defun markp (ev)
  "Utility function:
Returns T if argument is a MARK object"
  (typep ev 'mark))
(defun durp (ev)
  "Utility function:
Returns T if argument is an object containing a duration"
  (typep ev 'dur-base))
(defun eventp (ev)
  "Utility function:
Returns T if argument is a NOTE, REST or MARK object"
  (typep ev 'event-base)) ; events are objects with an offset
(defun fomusobjp (ev)
  "Utility function:
Returns T if argument is any kind of FOMUS object"
  (typep ev 'fomusobj-base))
(defun measp (ev)
  "Utility function:
Returns T if argument is a MEAS object"
  (typep ev 'meas))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SPECIAL ACCESSORS

;; (declaim (inline event-dur* event-endoff event-grace event-gracedur event-note* event-useracc event-acc event-addacc
;; 		 timesig-beat* timesig-nbeats timesig-num timesig-den event-voice* event-staff))
(defun event-dur* (ev)
  (declare (type dur-base ev))
  (if (consp (event-dur ev)) 0 (event-dur ev))) ; actual duration (if grace-note, = 0)
(defun event-grace (ev)
  (declare (type dur-base ev))
  (when (consp (event-dur ev)) (the (or integer list) (cdr (event-dur ev)))))
(defun event-gracedur (ev)
  (declare (type dur-base ev))
  (when (consp (event-dur ev)) (the (real (0)) (car (event-dur ev)))))
(defun event-endoff (ev)
  (declare (type dur-base ev))
  (if (event-grace ev) (event-off ev) (+ (event-off ev) (event-dur* ev)))) ; grace-notes effectively have 0 dur!
(defun event-note* (ev)
  (declare (type note ev))
  (if (consp (event-note ev)) (the rational (car (event-note ev))) (event-note ev)))
(defun event-notes* (ev)
  (declare (type note ev))
  (mapcar (lambda (e)
	    (declare (type (or cons rational) e))
	    (if (consp e) (the rational (car e)) e))
	  (event-note ev)))
(defun event-useracc (ev)
  (declare (type note ev))
  (when (consp (event-note ev)) (the (or cons rational) (cdr (event-note ev)))))
(defun event-acc (ev)
  (declare (type note ev))
  (if (consp (event-note ev))
      (let ((x (cdr (event-note ev))))
	(declare (type (or cons rational) x))
	(if (consp x) (the rational (car x)) x))
      0))
(defun event-accs (ev)
  (declare (type note ev))
  (mapcar (lambda (e)
	    (declare (type (or cons rational) e))
	    (if (consp e)
		(let ((x (cdr e)))
		  (declare (type (or cons rational)))
		  (if (consp x) (the rational (car x)) x))
		0))
	  (event-note ev)))
(defun event-addacc (ev)
  (declare (type note ev))
  (if (consp (event-note ev))
      (let ((x (cdr (event-note ev))))
	(declare (type (or cons rational) x))
	(if (consp x) (the rational (cdr x)) 0))
      0))
(defun event-addaccs (ev)
  (declare (type note ev))
  (mapcar (lambda (e)
	    (declare (type (or cons rational) e))
	    (if (consp e)
		(let ((x (cdr e)))
		  (if (consp x) (the rational (cdr x)) 0))
		0))
	  (event-note ev)))
;;(declaim (inline event-foff timesig-foff))
(defun event-foff (ev) (declare (type event-base ev)) (when (event-off ev) (float (event-off ev))))
(defun timesig-foff (ev) (declare (type event-base ev)) (when (timesig-off ev) (float (timesig-off ev))))

(defun event-voice* (ev)
  (declare (type mark ev))
  (if (consp (event-voice ev)) (the (or (integer 1) list) (cdr (event-voice ev))) (event-voice ev)))
(defun event-staff (ev)
  (declare (type mark ev))
  (if (consp (event-voice ev)) (the (integer 1) (car (event-voice ev))) 1))

(declaim (inline timesig-num timesig-den))
(defun timesig-num (ts) (declare (type timesig-repl)) (the (integer 1) (car (timesig-time ts))))
(defun timesig-den (ts) (declare (type timesig-repl)) (the (integer 1) (cdr (timesig-time ts))))
(defun timesig-beat* (ts) (declare (type timesig-repl)) (if (timesig-comp ts) (/ 3 (timesig-den ts)) (or (timesig-beat ts) *default-beat* (/ (timesig-den ts)))))

(declaim (inline obj-partid))
(defgeneric obj-partid (x))
(defmethod obj-partid ((ev event-base)) (event-partid ev))
(defmethod obj-partid ((ti timesig)) (timesig-partids ti))
(defmethod obj-partid ((pa part)) (part-partid pa))

(declaim (inline (setf obj-partid)))
(defgeneric (setf obj-partid) (x ob))
(defmethod (setf obj-partid) (x (ev event-base)) (setf (event-partid ev) x))
(defmethod (setf obj-partid) (x (ti timesig)) (setf (timesig-partids ti) x))
(defmethod (setf obj-partid) (x (pa part)) (setf (part-partid pa) x))

;; setfs--non-destructive for conses
(defsetf event-note* (ev) (x)
  (let ((en (gensym)) (xx (gensym)) (v0 (gensym)))
    `(let ((,v0 ,ev))
      (let ((,en (event-note ,v0)) (,xx ,x))
	(if (consp ,en)
	    (setf (event-note ,v0) (cons ,xx (cdr ,en)))
	    (setf (event-note ,v0) ,xx))
	,xx))))
(defsetf event-voice* (ev) (x)
  (let ((en (gensym)) (xx (gensym)) (v0 (gensym)))
    `(let ((,v0 ,ev))
      (let ((,en (event-voice ,v0)) (,xx ,x))
	(if (consp ,en)
	    (setf (event-voice ,v0) (cons (car ,en) ,xx))
	    (setf (event-voice ,v0) ,xx))
	,xx))))
(defsetf event-staff* (ev) (x)
  (let ((en (gensym)) (xx (gensym)) (v0 (gensym)))
    `(let ((,v0 ,ev))
      (let ((,en (event-voice ,v0)) (,xx ,x))
	(if (consp ,en)
	    (setf (event-voice ,v0) (cons ,xx (cdr ,en)))
	    (setf (event-voice ,v0) (cons ,xx ,en)))
	,xx))))
(defsetf event-dur* (ev) (x)
  (let ((en (gensym)) (xx (gensym)) (v0 (gensym)))
    `(let ((,v0 ,ev))
      (let ((,en (event-dur ,v0)) (,xx ,x))
	(if (consp ,en)
	    (setf (event-dur ,v0) (cons ,xx (cdr ,en)))
	    (setf (event-dur ,v0) ,xx))
	,xx))))
(defsetf event-grace* (ev) (x)
  (let ((en (gensym)) (xx (gensym)) (v0 (gensym)))
    `(let ((,v0 ,ev))
      (let ((,en (event-dur ,v0)) (,xx ,x))
	(if (consp ,en)
	    (setf (event-dur ,v0) (cons (car ,en) ,xx))
	    (setf (event-dur ,v0) (cons ,en ,xx)))
	,xx))))

;; aliases
(declaim (inline timesig-off meas-voices))
(defun timesig-off (ev) (declare (type event-base ev)) (event-off ev))
(defun meas-voices (ev) (declare (type meas ev)) (meas-events ev))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERNAL EXTENSIONS

(defclass ex-base ()
  ((tup :type list :accessor event-tup :initform '(nil . nil) :initarg :tup))) ; fraction of tuplet (not actual tuplet number), is a list (inner tuplet to outer tuplet)
(defclass noteex (ex-base note)
  ((tielt :type (or boolean list) :accessor event-tielt :initform nil :initarg :tielt) ; is tied to previous note
   (tiert :type (or boolean list) :accessor event-tiert :initform nil :initarg :tiert)
   (beamlt :type (or (integer 0) symbol list) :accessor event-beamlt :initform nil :initarg :beamlt)
   (beamrt :type (or (integer 0) symbol) :accessor event-beamrt :initform nil :initarg :beamrt)))
(defclass restex (ex-base rest)
  ((inv :type (or boolean list) :accessor event-inv :initform nil :initarg :inv))) ; invisible?
(defclass partex (part)
  ((userord :type (or integer null) :accessor part-userord :initform nil :initarg :userord)))

(defprint-class noteex id partid voice off dur note marks tup tielt tiert beamlt beamrt)
(defprint-class restex id partid voice off dur marks tup inv)
(defprint-class partex id partid name abbrev instr events props opts userord)

;; beam slots aren't used until later--use them as temp storage
(declaim (inline event-userstaff event-userclef event-acctie event-textdir event-nomerge event-fakenote #|event-noddot|#))
(defun event-userstaff (ev) (declare (type noteex ev)) (event-beamlt ev))
(defsetf event-userstaff (ev) (x) `(setf (event-beamlt ,ev) ,x))
(defun event-userclef (ev) (declare (type noteex ev)) (event-beamrt ev))
(defsetf event-userclef (ev) (x) `(setf (event-beamrt ,ev) ,x))
(defun event-acctie (ev) (declare (type noteex ev)) (event-beamlt ev))
(defsetf event-acctie (ev) (x) `(setf (event-beamlt ,ev) ,x))
(defun event-textdir (ev) (declare (type noteex ev)) (event-beamlt ev))
(defsetf event-textdir (ev) (x) `(setf (event-beamlt ,ev) ,x))
(defun event-nomerge (ev) (declare (type restex ev)) (event-inv ev))
(defsetf event-nomerge (ev) (x) `(setf (event-inv ,ev) ,x))
(defun event-fakenote (ev) (declare (type noteex ev)) (eq (event-beamlt ev) 'f))
;; (defun event-noddot (ev) (declare (type noteex ev)) (event-beamlt ev))
;; (defsetf event-noddot (ev) (x) `(setf (event-beamlt ,ev) ,x))
(defun event-autodur (ev) (declare (type noteex ev)) (event-beamrt ev))
(defsetf event-autodur (ev) (x) `(setf (event-beamrt ,ev) ,x))
(defun reset-tempslots (parts val)
  (declare (type list parts) (type (or null (integer 0 0)) val))
  (loop for p #|of-type part|# in parts
	if (measp (first (part-events p))) do
	(loop for m #|of-type meas|# in (part-meas p) do
	      (loop for e #|of-type (or noteex restex)|# in (meas-events m)
		    when (notep e) do (setf (event-beamlt e) val (event-beamrt e) val)))
	else do
	(loop for e #|of-type (or noteex restex)|# in (part-events p) 
	      when (notep e) do (setf (event-beamlt e) val (event-beamrt e) val))))
(defun reset-resttempslots (parts)
  (declare (type list parts))
  (loop for p in parts do
	(loop for m in (part-meas p) do
	      (loop for e in (meas-events m) when (restp e) do
		    (setf (event-inv e) nil)))))

(declaim (inline part-meas))
(defun part-meas (ev) (declare (type partex ev)) (part-events ev))

(declaim (special *old-objects*))
(declaim (inline make-noteex make-restex make-partex))
(defun make-noteex (ob &rest args)
  (declare (type (or null note) ob))
  (let ((r (apply #'make-instance 'noteex args))) (when *old-objects* (setf (gethash r *old-objects*) ob)) r))
(defun make-restex (ob &rest args)
  (declare (type (or null rest) ob))
  (let ((r (apply #'make-instance 'restex args))) (when *old-objects* (setf (gethash r *old-objects*) ob)) r))
(defun make-partex (ob &rest args)
  (declare (type (or null part) ob))
  (let ((r (apply #'make-instance 'partex args))) (when *old-objects* (setf (gethash r *old-objects*) ob)) r))

;; copy functions
(declaim (inline copy-timesig copy-timesig-repl copy-event copy-part copy-meas))
(defgeneric copy-timesig (ts &key &allow-other-keys))
(defmethod copy-timesig ((ts timesig-repl) &key off (id (obj-id ts)) (time (timesig-time ts)) (div (timesig-div ts)) (comp (timesig-comp ts))
			 (props (timesig-props ts)) (beat (timesig-beat ts)) partids repl)
  "Utility function:
Copies a TIMESIG object"
  (declare (type (rational 0) off) (type cons time) (type list div) (type boolean comp) (type (or (rational 0) null) beat) (type list props)
	   (type (or symbol real list) partids) (type (or timesig-repl list) repl))
  (make-timesig :off off :id id :time time :div div :comp comp :beat beat :props props :partids partids :repl repl))
(defmethod copy-timesig ((ts timesig) &key (off (timesig-off ts)) (id (obj-id ts)) (time (timesig-time ts)) (div (timesig-div ts)) (comp (timesig-comp ts))
			 (props (timesig-props ts)) (beat (timesig-beat ts)) (partids (timesig-partids ts)) (repl (timesig-repl ts)))
  "Utility function:
Copies a TIMESIG object"
  (declare (type (rational 0) off) (type cons time) (type list div) (type boolean comp) (type (or (rational 0) null) beat) (type list props)
	   (type (or symbol real list) partids) (type (or timesig-repl list) repl))
  (make-timesig :off off :id id :time time :div div :comp comp :beat beat :props props :partids partids :repl repl)) 
(defgeneric copy-timesig-repl (ts &key &allow-other-keys))
(defmethod copy-timesig-repl ((ts timesig-repl) &key (id (obj-id ts)) (time (timesig-time ts)) (div (timesig-div ts)) (comp (timesig-comp ts))
			      (props (timesig-props ts)) (beat (timesig-beat ts)))
  "Utility function:
Copies a TIMESIG-REPL object"
  (declare (type cons time) (type list div) (type boolean comp) (type (or (rational 0) null) beat) (type list props))
  (make-timesig-repl :id id :time time :div div :comp comp :beat beat :props props))
(defgeneric copy-event (ev &key &allow-other-keys))
(defmethod copy-event ((ev note) &key (off (event-off ev)) (id (obj-id ev)) (partid (event-partid ev)) (dur (event-dur ev)) (marks (event-marks ev)) (voice (event-voice ev))
		       (note (event-note ev)))
  "Utility function:
Copies a NOTE, REST or MARK object"
  (declare (type (or (real 0)) off) (type (or symbol real null) partid) (type (or real symbol cons) dur) (type list marks) (type (or (integer 1) cons) voice)
	   (type (or real symbol cons) note))
  (make-noteex ev
   :id id :partid partid :off off
   :dur dur :marks marks :voice voice
   :note note))
(defmethod copy-event ((ev rest) &key (off (event-off ev)) (id (obj-id ev)) (partid (event-partid ev)) (dur (event-dur ev)) (marks (event-marks ev))
		       (voice (event-voice ev)))
  "Utility function:
Copies a NOTE, REST or MARK object"
  (declare (type (or (real 0)) off) (type (or symbol real null) partid) (type (or real symbol cons) dur) (type list marks) (type (or (integer 1) cons) voice))
  (make-restex ev
   :id id :partid partid :off off
   :dur dur :marks marks :voice voice))
(defmethod copy-event ((ev mark) &key (off (event-off ev)) (id (obj-id ev)) (partid (event-partid ev)) (marks (event-marks ev))
		       (voice (event-voice ev)))
  "Utility function:
Copies a NOTE, REST or MARK object"
  (declare (type (or (real 0) cons) off) (type (or symbol real null) partid) (type list marks) (type (or (integer 1) cons) voice))
  (make-restex ev
   :id id :partid partid :off off
   :marks marks :voice voice))
(defmethod copy-event ((ev noteex) &key (off (event-off ev)) (id (obj-id ev)) (partid (event-partid ev)) (dur (event-dur ev)) (marks (event-marks ev)) (voice (event-voice ev))
		       (note (event-note ev)) (tup (event-tup ev)) (tielt (event-tielt ev)) (tiert (event-tiert ev))
		       (beamlt (event-beamlt ev)) (beamrt (event-beamrt ev)))
  "Utility function:
Copies a NOTE, REST or MARK object"
  (declare (type (or (real 0)) off) (type (or symbol real null) partid) (type (or real symbol cons) dur) (type list marks) (type (or (integer 1) cons) voice)
	   (type (or real symbol cons) note) (type (or boolean list) tielt tiert) (type (or (integer 0) symbol list) beamlt) (type (or (integer 0) symbol) beamrt))
  (make-noteex ev
   :id id :partid partid :off off
   :dur dur :marks marks :voice voice
   :note note
   :tup tup
   :tielt tielt :tiert tiert :beamlt beamlt :beamrt beamrt))
(defmethod copy-event ((ev restex) &key (off (event-off ev)) (id (obj-id ev)) (partid (event-partid ev)) (dur (event-dur ev)) (marks (event-marks ev))
		       (voice (event-voice ev)) (tup (event-tup ev)) (inv (event-inv ev)))
  "Utility function:
Copies a NOTE, REST or MARK object"
  (declare (type (or (real 0)) off) (type (or symbol real null) partid) (type (or real symbol cons) dur) (type list marks) (type (or (integer 1) cons) voice)
	   (type (or boolean list) inv))
  (make-restex ev
   :id id :partid partid :off off
   :dur dur :marks marks :voice voice
   :tup tup
   :inv inv))
(defgeneric copy-part (pa &key &allow-other-keys))
(defmethod copy-part ((pa part) &key (id (obj-id pa)) (name (part-name pa)) (abbrev (part-abbrev pa)) (events (part-events pa)) (opts (part-opts pa))
		      (instr (part-instr pa)) (partid (part-partid pa)) (props (part-props pa)))
  "Utility function:
Copies a PART object"
  (declare (type (or string null) name) (type (or string null) abbrev) (type list opts) (type list events) (type (or symbol (integer 0 127) instr cons) instr)
	   (type (or symbol real) partid) (type list props))
  (make-part
   :id id :name name :abbrev abbrev :events events :opts opts :instr instr :partid partid :props props))
(defmethod copy-part ((pa partex) &key (id (obj-id pa)) (name (part-name pa)) (abbrev (part-abbrev pa)) (events (part-events pa)) (opts (part-opts pa))
		      (instr (part-instr pa)) (partid (part-partid pa)) (props (part-props pa)) (userord (part-userord pa)))
  "Utility function:
Copies a PART object"
  (declare (type (or string null) name) (type (or string null) abbrev) (type list opts) (type list events) (type (or symbol (integer 0 127) instr cons) instr)
	   (type (or symbol real) partid) (type list props) (type (or integer null) userord))
  (make-partex pa
   :id id :name name :abbrev abbrev :events events :opts opts :instr instr :partid partid :props props :userord userord))
(defun copy-meas (me &key (id (obj-id me)) (timesig (meas-timesig me)) (off (meas-off me)) (endoff (meas-endoff me)) (events (meas-events me))
		  (props (meas-props me)) (div (meas-div me)))
  "Utility function (advanced/internal usage):
Copies a MEAS object"
  (declare (type meas me) (type timesig-repl timesig) (type (rational 0) off) (type (rational 0) endoff) (type list events props div))
  (make-meas :id id :timesig timesig :off off :endoff endoff :events events :props props :div div))

;; MAKE-INSTR

(defun make-instrex* (instr part)
  (declare (type instr instr))
  (copy-instr instr
	      :8uplegls (if (consp (instr-8uplegls instr)) (cons (first (instr-8uplegls instr)) (second (instr-8uplegls instr))) (instr-8uplegls instr))
	      :8dnlegls (if (consp (instr-8dnlegls instr)) (cons (first (instr-8dnlegls instr)) (second (instr-8dnlegls instr))) (instr-8dnlegls instr))
	      :percs (loop for e in (instr-percs instr) collect
			   (flet ((er (s) (error "Invalid percussion instrument ~S in part ~S" s (part-name part))))
			     (flet ((gi (s)
				      (declare (type (or symbol (integer 0 127)) s))
				      (if (symbolp s)
					  (or (find s *percussion* :key #'perc-sym)
					      (find s +percussion+ :key #'perc-sym)
					      (er s))
					  (or (find s *percussion* :test (lambda (k i)
									   (declare (type (integer 0 127) k) (type perc i))
									   (find k (force-list (perc-midinote-im i)))))
					      (find s +percussion+ :test (lambda (k i)
									   (declare (type (integer 0 127) k) (type perc i))
									   (find k (force-list (perc-midinote-im i)))))
					      (er s)))))
			       (let ((z (typecase e
					  (perc (copy-perc e))
					  ((or symbol number) (copy-perc (gi e))) 
					  (list (let ((z (apply #'copy-perc (gi (first e)) (rest e))))
						  (check-type* z +perc-type+)
						  z))
					  (otherwise (er e)))))
				 (when (perc-note z) (setf (perc-note z) (note-to-num (perc-note z))))
				 z))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INPUT TYPE CHECKS

(declaim (type cons +event-base-type+ +timesig-repl-type+ +timesig-type+ +partid-type+ +mark-type+ +dur-base-type+
	       +note-type+ +rest-type+ +part-type+))
(defparameter +event-base-type+
  '(class* event-base (off (check* (real 0) "Found ~S, expected (REAL 0) in OFFSET slot" t))))

(defparameter +timesig-repl-type+
  '(class* timesig-repl
    (time (check* (list* (integer 1) (integer 1)) "Found ~S, expected list ((INTEGER 1) (INTEGER 1)) in TIME slot" t))
    (beat (check* (or null (rational (0))) "Found ~S, expected (RATIONAL (0)) in BEAT slot" t))
    (div (check* (or* null (list-of* (rational (0))) (list-of-unique* (list-of* (rational (0))))) "Found ~S, expected list of (RATIONAL (0)) or ((RATIONAL (0)) ...) in DIV slot" t))
    (comp (check* (or boolean (member default))) "Found ~S, expected BOOLEAN in COMP slot" t)
    (props (or* null (with-error* ("~~A in PROPS slot") (type* +timesig-props+))))))

(defparameter +timesig-type+
  '(with-error* (timesig "~~A of timesig at offset ~S" (function timesig-foff))
    (and*
     (type* +timesig-repl-type+)
     (class* timesig
      (off (check* (rational 0) "Found ~S, expected (RATIONAL 0) in OFFSET slot" t))
      (partid (check* (or* (or null symbol real)
			   (list-of* (or null symbol real)))
		      "Found ~S, expected SYMBOL, REAL or list of SYMBOL/REAL in PARTIDS slot" t))
      (repl (check* (or* null timesig-repl (list-of* timesig-repl)) "Found ~S, expected TIMESIG-REPL or list of TIMESIG-REPL in REPL slot" t))))))

(defparameter +partid-type+
  '(check* (or symbol real) "Found ~S, expected SYMBOL or REAL in PARTID slot" t))

(defparameter +mark-type+
  '(class* mark
    (off (check* (or* (real 0) (list* real)) "Found ~S, expected (REAL 0) or list (REAL) in OFFSET slot" t))
    (partid (type* +partid-type+)) 
    (voice (check* (or* (integer 1) (list-of-unique* (integer 1)) (cons (member :staff) (unique-list-of* (or (integer nil -1) (integer 1)))))
	    "Found ~S, expected (INTEGER 1), unique list of (INTEGER 1) or unique list of (:STAFF (OR (INTEGER NIL -1) (INTEGER 1)) ...) in VOICE slot" t))
    (marks (or* null (with-error* ("~~A in MARKS slot") (type* +markmarks-type+))))))

(defparameter +dur-base-type+
  '(and*
    (type* +event-base-type+)
    (type* +mark-type+)
    (class* dur-base
     (partid (type* +partid-type+))
     (voice (check* (or* (integer 1) (list-of-unique* (integer 1))) "Found ~S, expected (INTEGER 1) or unique list of (INTEGER 1) in VOICE slot" t))
     (dur (check* (or* (satisfies is-dur) (list* (satisfies is-dur) integer))
		  "Found ~S, expected REAL, valid rhythmic symbol or list (REAL/SYMBOL INTEGER) in DUR slot" t)))))

(defparameter +note-type+
  '(with-error* (note "~~A of note at offset ~S" (function event-foff))
    (and*
     (type* +dur-base-type+)
     (class* note
      (note (check* (type* +notesym-type+)
		    "Found ~S, expected REAL or valid note/accidental symbols in the form X, (X X ...) or (X (X X) ...) in NOTE slot" t))
      (marks (or* null (with-error* ("~~A in MARKS slot") (type* +notemarks-type+))))))))

(defparameter +rest-type+
  '(with-error* (rest "~~A of rest at offset ~S" (function event-off))
    (and*
     (type* +dur-base-type+)
     (class* rest
      (marks (or* null (with-error* ("~~A in MARKS slot") (type* +restmarks-type+))))))))

(defparameter +part-type+
  `(and*
    (with-error* (part "~~A of part ~S" (function part-name))
      (class* part
       (name (check* (or null string) "Found ~S, expected STRING in NAME slot" t))
       (abbrev (check* (or null string) "Found ~S, expected STRING in ABBREV slot" t))
       (opts (check* key-arg-pairs* "Found ~S, expected KEYWORD/ARGUMENT-PAIRS in OPTS slot" t))
       (events (check* (or* null (list-of* (check* (or note rest mark timesig) "Found ~S, expected NOTE, REST or TIMESIG in list in EVENTS slot" t)))
		       "Expected list of NOTE, REST or TIMESIG in EVENTS slot"))
       (instr (check* (or* symbol (integer 0 127) instr (cons* symbol (key-arg-pairs* ,@+instr-keys+)))
		      "Found ~S, expected NIL, SYMBOL, (INTEGER 0 127), INSTR or (SYMBOL/(INTEGER 0 127) KEYWORD/ARGUMENT-PAIRS...) in INSTR slot" t))
       (partid (check* (or symbol real) "Found ~S, expected SYMBOL or REAL in PARTID slot" t))
       (props (or* null (with-error* ("~~A in PROPS slot") (type* +part-props+))))))
    (with-error* (part "~~A, part ~S" (function part-name))
      (class* part
       (events (or* null (list-of* (or* (type* +note-type+) (type* +rest-type+) (type* +mark-type+) (type* +timesig-type+)))))))))
