;; -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**************************************************************************************************
;; FOMUS
;; split.lisp
;;**************************************************************************************************

(in-package :fomus)
(compile-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SPLIT RULES

(declaim (type list *default-tuplets*) (type symbol *tuplet-function*))
(defparameter *default-tuplets* nil)
(defparameter *tuplet-function* t)
(declaim (inline tuplet-mod))
(defun tuplet-mod () (if (truep *tuplet-function*) :pow2 *tuplet-function*))

;; tup in place of div
(defun split-tupdurmult (tup div)
  (declare (type (integer 2) tup) (type (rational 1) div))
  (/ tup (or (second (find-if (lambda (x) (and (= tup (first x)) (expof2 (/ div (second x))))) *default-tuplets*))
	     (case (tuplet-mod)
	       (:dur (loop-return-firstmin (diff d tup) for d = (loop for x1 = div then x2 for x2 = (/ x1 2) while (integerp x2) finally (return x1)) then (* d 2)))
	       (:pow2 (loop
		       for d0 = nil then d
		       and d = (loop for x1 = div then x2 for x2 = (/ x1 2) while (> x2 1) finally (return x1)) then (* d 2)
		       when (> d tup) do (return (or d0 d))))
	       (otherwise (error "Unknown tuplet function ~S" *tuplet-function*))))))

;; returns list of new rules for given rule: (number-or-list-of-divs newrule1 newrule2...)
(defclass baserule () nil)
(defclass basesplit ()
  ((alt :type boolean :accessor rule-alt :initform nil :initarg :alt)	; alt/art = attached/anchored left/right (at a div-2 boundary)
   (art :type boolean :accessor rule-art :initform nil :initarg :art)
   (init :type list :accessor rule-init :initform nil :initarg :init)
   (irr :type boolean :accessor rule-irr :initform nil :initarg :irr))) ; t if parent is irregular (not expof2)
(defclass basenodiv ()
  ((tlt :type boolean :accessor rule-tlt :initform nil :initarg :tlt)	; tlt/trt = t if tie allowed on that side, nil if not allowed
   (trt :type boolean :accessor rule-trt :initform nil :initarg :trt)))
(defclass basecomp ()
  ((comp :type boolean :accessor rule-comp :initform nil :initarg :comp)))
(defclass baseunit ()
  ((tup :type list :accessor rule-tup :initform nil :initarg :tup)	; tup members multiplied together gives the actual fraction
   (dmu :type list :accessor rule-dmu :initform nil :initarg :dmu)))
(defclass baseinit ()
  ((time :type (cons (integer 1) (integer 1)) :accessor rule-time :initform '(1 1) :initarg :time)
   (beat :type (rational (0)) :accessor rule-beat :initform 1 :initarg :beat)))

(defclass initdiv (baserule baseinit basecomp)
  ((list :type list :accessor rule-list :initform nil :initarg :list)
   (tsoff :type (rational 0) :accessor rule-tsoff :initform 0 :initarg :tsoff)))
(defclass sig (baserule basesplit baseinit basecomp)
  ((top :type boolean :accessor rule-top :initform nil :initarg :top)))
(defclass unit (baserule basesplit baseunit basecomp)
  ((div :type (integer 2) :accessor rule-div :initform 1 :initarg :div)))
(defclass sig-nodiv (baserule basenodiv basecomp) ())
(defclass unit-nodiv (baserule basenodiv baseunit basecomp)
  ((rst :type boolean :accessor rule-rst :initform nil :initarg :rst)))

(defprint-class initdiv time comp beat list tsoff)
(defprint-class sig time comp beat alt art irr init top)
(defprint-class unit div comp alt art irr init tup dmu)
(defprint-class sig-nodiv comp tlt trt)
(defprint-class unit-nodiv tup comp dmu tlt trt rst)

(defmacro basesplitp (o) `(typep ,o 'basesplit))
(defmacro basenodivp (o) `(typep ,o 'basenodiv))
(defmacro basecompp (o) `(typep ,o 'basecomp))
(defmacro baseunitp (o) `(typep ,o 'baseunit))
(defmacro baseinitp (o) `(typep ,o 'baseinit))
(defmacro initdivp (o) `(typep ,o 'initdiv))
(defmacro sigp (o) `(typep ,o 'sig))
(defmacro unitp (o) `(typep ,o 'unit))
(defmacro sig-nodiv-p (o) `(typep ,o 'sig-nodiv))
(defmacro unit-nodiv-p (o) `(typep ,o 'unit-nodiv))

(defmacro make-initdiv (&rest args) `(make-instance 'initdiv ,@args))
(defmacro make-sig (&rest args) `(make-instance 'sig ,@args))
(defmacro make-unit (&rest args) `(make-instance 'unit ,@args))
(defmacro make-sig-nodiv (&rest args) `(make-instance 'sig-nodiv ,@args))
(defmacro make-unit-nodiv (&rest args) `(make-instance 'unit-nodiv ,@args))

;;(declaim (inline rule-num rule-den))
(defun rule-num (r) (declare (type baseinit r)) (the (integer 1) (car (rule-time r))))
(defun rule-den (r) (declare (type baseinit r)) (the (integer 1) (cdr (rule-time r))))

(declaim (type (member t :all :top :sig) *dotted-note-level*)
	 (type (member t :all :top :sig) *shortlongshort-notes-level*)
	 (type boolean *syncopated-notes-level*))
(defparameter *dotted-note-level* t) ; can = (t or :all), :top or :sig for levels where dotted notes are allowed, nil = no dotted notes
(defparameter *shortlongshort-notes-level* t) ; = (same as above) if special rhythmic patterns allowed (tied syncopations)
(defparameter *syncopated-notes-level* t) ; b bah.. bah.. bah.. b

(declaim (type boolean *double-dotted-notes* *tuplet-dotted-rests*))
(defparameter *double-dotted-notes* t) ; = t if can use double dotted notes 
(defparameter *tuplet-dotted-rests* t)

(defun split-rules-bylevel (rule tups)	; tups = tuplets are allowed
  (declare (type baserule rule) (type boolean tups))
  (let ((mt (first (if (baseunitp rule)
		       (loop for e on *max-tuplet* for xxx in (rule-tup rule) finally (return e))
		       *max-tuplet*))))	; max tuplet for next nesting level
    (flet ((dv2 (n)
	     (declare (type (integer 1) n))
	     (loop for n2 = (/ n 2) while (integerp n2) do (setf n n2))
	     (max n 2)))
      (flet ((divs (tup div &optional ntup ndmu)
	       (declare (type (integer 2) tup) (type (rational 1) div) (type list ntup ndmu))
	       (let ((tu (force-list ntup))
		     (dmu (cons (split-tupdurmult tup div) ndmu))
		     (ir (when *tuplet-dotted-rests* (not (expof2 tup)))))
		 (loop
		  for i of-type (or cons (integer 1)) in (tuplet-division tup)
		  collect
		  (let ((x (if (listp i) (loop with x = 0 for y of-type (integer 1) in (butlast i) collect (/ (incf x y) tup)) (list (/ i tup)))))
		    (cons (if (list>1p x) x (first x))
			  (loop for (e1 e2) of-type ((rational 0 1) (or (rational 0 1) null)) on (cons 0 (append x '(1))) #-clisp while #-clisp e2
				for ii in #-clisp (if (listp i) i (list i (- tup i))) #+clisp (if e2 (if (listp i) i (list i (- tup i))) (loop-finish))
				#-clisp and #+clisp for tt = (- e2 e1) and a1 = t then a2
				for a2 = (or (= e2 1) (and (expof2 e2) (expof2 (- tup e2)))) collect
				(make-unit :div (dv2 ii) :tup (cons tt tu) :dmu dmu :alt a1 :art a2 :irr ir :comp (rule-comp rule)))))))))
	(sort (etypecase rule
		((or initdiv sig)
		 (let* ((num (/ (rule-num rule) (* (rule-den rule) (rule-beat rule)))) ; 3/8 is treated like 1/4, etc.
			(ex (expof2 num))) ; in compound meter, num = 1 for 3/8
		   (flet ((al (sy)
			    (declare (type (member t :all :top :sig) sy))
			    (or (find sy '(t :all :sig))
				(and (eq sy :top) (or (initdivp rule) (rule-top rule)))))
			  (in (n al ar in &optional ir)	; n = division ratio, ir = if rule is irregular & 2/3 duration is expof2
			    (declare (type (rational (0) (1)) n) (type boolean al ar) (type list in))
			    (if (if (rule-comp rule) (>= num (/ n)) (> num (/ n)))
				(make-sig :time (cons (* (rule-num rule) n) (rule-den rule)) :comp (rule-comp rule) :beat (rule-beat rule)
					  :alt al :art ar :init in :irr (not ex) :comp (rule-comp rule))
				(make-unit :div (if (or (rule-comp rule) ir) 3 2) ;; (if (rule-comp rule) 3 2)
					   :tup nil :alt t :art t :init in :irr (not ex) :comp (rule-comp rule)))) 
			  (snd (n tl tr)
			    (declare (type (rational (0) (1)) n) (type boolean tl tr))
			    (if (if (rule-comp rule) (>= num (/ n)) (> num (/ n)))
				(make-sig-nodiv :comp (rule-comp rule) :tlt tl :trt tr :comp (rule-comp rule))
				(make-unit-nodiv :tup nil :tlt tl :trt tr :comp (rule-comp rule)))))
		     (flet ((si (n wh al ar &optional ir) ; n = division ratio, >1/4 or >3/8 comp. is designated with :sig, smaller durations become units
			      (declare (type (rational (0) (1)) n) (type (member :l :r) wh) (type boolean al ar))
			      (etypecase rule
				(initdiv (in n al ar nil ir))
				(sig (if (if (rule-comp rule) (>= num (/ n)) (> num (/ n)))
					 (make-sig :time (cons (* (rule-num rule) n) (rule-den rule)) :comp (rule-comp rule) :beat (rule-beat rule)
						   :alt (if (eq wh :l) (and (rule-alt rule) al) (and (rule-alt rule) (rule-art rule) al))
						   :art (if (eq wh :r) (and (rule-art rule) ar) (and (rule-alt rule) (rule-art rule) ar))
						   :irr (not ex) :comp (rule-comp rule))
					 (make-unit :div (if ir 3 2) :tup nil :alt t :art t :irr (not ex) :comp (rule-comp rule)))))))
		       (nconc (etypecase rule
				(initdiv (loop
					  for ee0 of-type cons in (force-list2all (rule-list rule))
					  #+debug unless #+debug (= (apply #'+ ee0) num)
					  #+debug do #+debug (error "Error in SPLIT-RULES-BYLEVEL")
					  when (list>1p ee0)
					  collect (loop
						   for (e en) of-type ((rational (0)) (or (rational (0)) null)) on ee0
						   sum e into s
						   collect (/ e num) into ee ; split durs
						   when en collect (/ s num) into ll ; split points
						   finally (return (cons (if (list>1p ll) ll (car ll))
									 (loop
									  for (i n) of-type ((rational (0)) (or (rational (0)) null)) on ee
									  and ii of-type (rational (0)) in ee0
									  and x of-type (rational (0) 1) in (append ll '(1))
									  and la = t then aa
									  for aa = (let ((xx (* x num)))
										     (and (expof2 xx) (or (= num xx) (expof2 (- num xx)))))
									  collect (in i la (or (null n) aa) ee (expof2 (* ii 2/3)))))))))
				(sig (loop
				      for nn of-type (integer 2) in (or (lowmult (numerator num)) (if (rule-comp rule) '(3) '(2)))
				      nconc (loop
					     for j from 1 below nn
					     for x of-type (rational (0) (1)) = (/ j nn) ; x is the ratio
					     for xx = (* x num) and co = (and (rule-comp rule) (<= num 1))
					     when (or (and co (expof2 (* xx 3/2)))
						      (expof2 xx) (expof2 (- num xx)))
					     collect (let ((aa (or (and co (expof2 (* xx 3/2)) (expof2 (* (- num xx) 3/2)))
								   (and (expof2 xx) (expof2 (- num xx))))))
						       (list x
							     (si x :l t aa (and (rule-irr rule) (expof2 (* xx 2/3)))) 
							     (si (- 1 x) :r aa t (and (rule-irr rule) (expof2 (* x 2/3)))))))))) 
			      (when (and (al *dotted-note-level*) (or (initdivp rule) (rule-alt rule)) ex (not (rule-comp rule)))
				(nconc (list (list 3/4 (snd 3/4 t nil) (si 1/4 :r t t))) ; dotted notes
				       (when *double-dotted-notes*
					 (list (list 7/8 (snd 7/8 t nil) (si 1/8 :r t t))))))
			      (when (and (al *dotted-note-level*) (or (initdivp rule) (rule-art rule)) ex (not (rule-comp rule)))
				(nconc (list (list 1/4 (si 1/4 :l t t) (snd 3/4 nil t)) )
				       (when *double-dotted-notes*
					 (list (list 1/8 (si 1/8 :l t t) (snd 7/8 nil t))))))
			      (when (and (al *shortlongshort-notes-level*) (or (initdivp rule) (rule-alt rule)) (or (initdivp rule) (rule-art rule))
					 ex (or (not (rule-comp rule)) (>= num 4)))
				(list (list '(1/4 3/4) (si 1/4 :l t t) (snd 1/2 t t) (si 1/4 :r t t))))	; longer note in middle
			      (when (and *syncopated-notes-level* (al :top) (or (initdivp rule) (rule-alt rule)) (or (initdivp rule) (rule-art rule)) (>= num 3)
					 (not (rule-comp rule)))
				(cond ((integerp num)
				       (list (nconc (list (loop for i from 1/2 below num collect (/ i num)) ; regular off beat syncopation
							  (snd (/ 1/2 num) t nil))
						    (make-list (1- num) :initial-element (snd (/ num) nil nil))
						    (list (snd (/ 1/2 num) nil t)))))
				      ((= (denominator num) 2)
				       (nconc (list (nconc (list (loop for i from 1 below num collect (/ i num))) ; regular off beat syncopation
							   (make-list (- num 1/2) :initial-element (snd (/ num) nil nil))
							   (list (snd (/ 1/2 num) nil t))))
					      (list (nconc (list (loop for i from 1/2 below num collect (/ i num)) ; regular off beat syncopation
								 (snd (/ 1/2 num) t nil))
							   (make-list (- num 1/2) :initial-element (snd (/ num) nil nil))))))))
			      (when (and tups mt (or (initdivp rule) (and (sigp rule) (rule-top rule)) (and (rule-alt rule) (rule-art rule))))
				(loop
				 with nu = (if (rule-comp rule) (* num 3/2) num)
				 for j of-type (integer 2) in (notdivby2s mt) ; only primes--number isn't actual tuplet, just division
				 unless (expof2 (/ nu j))
				 nconc (divs j nu))))))))
		(unit			; unit is at divide-by-2 level
		 (let ((ex (expof2 (rule-div rule))))
		   (flet ((al (sy)
			    (declare (type (member t :all :top :sig) sy))
			    (find sy '(t :all)))
			  (tu (n)
			    (declare (type (rational (0) (1)) n))
			    (when (rule-tup rule)
			      (cons (* (the (rational (0)) (first (rule-tup rule))) n) (rest (rule-tup rule))))))
		     (flet ((un (n wh al ar &optional d) ; d is fraction of total number of divs
			      (declare (type (rational (0) (1)) n) (type (member :l :r) wh) (type boolean al ar) (type (or (integer 1) null) d))
			      (make-unit :div (if d (dv2 d) 2) :tup (tu n) :dmu (rule-dmu rule)
					 :alt (if (eq wh :l) (and (rule-alt rule) al) (and (rule-alt rule) (rule-art rule) al))
					 :art (if (eq wh :r) (and (rule-art rule) ar) (and (rule-alt rule) (rule-art rule) ar))
					 :irr (not ex) :comp (rule-comp rule)))
			    (und (n tl tr) (make-unit-nodiv :tup (tu n) :dmu (rule-dmu rule) :tlt tl :trt tr :comp (rule-comp rule))))
		       (nconc (loop for nn of-type (integer 2) in (or (lowmult (rule-div rule)) '(2))
				    nconc (loop for j from 1 below nn collect
						(let ((x (/ j nn))
						      (aa (and (expof2 j) (expof2 (- nn j)))))
						  (list x (un x :l t aa j) (un (- 1 x) :r aa t (- nn j))))))
			      (when (and (al *dotted-note-level*) (rule-alt rule) ex)
				(nconc (list (list 3/4 (und 3/4 t nil) (un 1/4 :r t t))) ; dotted notes
				       (when *double-dotted-notes*
					 (list (list 7/8 (und 7/8 t nil) (un 1/8 :r t t))))))
			      (when (and (al *dotted-note-level*) (rule-art rule) ex)
				(nconc (list (list 1/4 (un 1/4 :l t t) (und 3/4 nil t)))
				       (when *double-dotted-notes*
					 (list (list 1/8 (un 1/8 :l t t) (und 7/8 nil t))))))
			      (when (and (al *shortlongshort-notes-level*) (rule-alt rule) (rule-art rule) ex)
				(list (list '(1/4 3/4) (un 1/4 :l t t) (und 1/2 t t) (un 1/4 :r t t))))	; longer note in middle
			      (when (and tups mt (or (initdivp rule) (and (sigp rule) (rule-top rule)) (rule-tup rule)
						     (if (and (baseunitp rule) (rule-tup rule))
							 (or (rule-alt rule) (rule-art rule))
							 (and (rule-alt rule) (rule-art rule)))))
				(loop
				 for j of-type (integer 2) in (notdivby2s mt) ; only primes--number isn't actual tuplet, just division
				 unless (expof2 (/ (rule-div rule) j))
				 nconc (divs j (rule-div rule) (rule-tup rule) (rule-dmu rule))))))))))
	      (lambda (x0 y0)
		(declare (type (cons (or cons (rational (0) (1))) *) x0 y0))
		(let ((x (car x0)) (y (car y0)))
		  (declare (type (or cons (rational (0) (1))) x y))
		  (let ((xm (if (listp x) (the (rational (0) (1)) (ave-list x)) x))
			(ym (if (listp y) (the (rational (0) (1)) (ave-list y)) y)))
		    (let ((xd (diff xm 1/2))
			  (yd (diff ym 1/2)))
		      (if (= xd yd)
			  (if (= xm ym)
			      (cond ((listp x) t)
				    ((listp y) nil))
			      (> xm ym))
			  (< xd yd)))))))))))

(defun first-splitrule (ts)
  (declare (type timesig-repl ts))
  (if (timesig-div* ts)
      (make-initdiv :time (timesig-time ts) :comp (timesig-comp ts) :beat (timesig-beat* ts)
		    :list (timesig-div* ts) :tsoff (timesig-off ts) :comp (timesig-comp ts))
      (make-sig :time (timesig-time ts) :comp (timesig-comp ts) :beat (timesig-beat* ts)
		:alt t :art t :top t :comp (timesig-comp ts))))

