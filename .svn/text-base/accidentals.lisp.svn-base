;; -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**************************************************************************************************
;; FOMUS
;; accidentals.lisp
;;**************************************************************************************************

;; ***** UPDATED COMMENTS *****

(in-package :fomus)
(compile-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFAULT ACCIDENTALS ALGORITHM

(declaim (type symbol *auto-accs-plugin* *auto-accs-module*))
(defparameter *auto-accs-plugin* nil) ; deprecated setting
(defparameter *auto-accs-module* t) ; setting
(declaim (inline auto-accs-fun))
(defun auto-accs-fun () (if (truep *auto-accs-module*) :acc1 *auto-accs-module*))

(declaim (type boolean *auto-accidentals* *auto-cautionary-accs*))
(defparameter *auto-accidentals* t) ; setting
(defparameter *auto-cautionary-accs* nil) ; setting

;; score values
(declaim (type (real 1) *max-acc-beat-dist-mul*))
(defparameter *max-acc-beat-dist-mul* 2) ; maximum number of beats of rests allowed before not caring about spelling
(declaim (type #-(or openmcl allegro) (float 0 1) #+(or openmcl allegro) float *acc-dist-score*))
(defparameter *acc-dist-score* (float 1/3)) ; used w/ acc-beat-dist: determines how much distance influences spelling
(declaim (type #-(or openmcl allegro) (float (0)) #+(or openmcl allegro) float *acc-beat-dist* *acc-octave-dist*))
(defparameter *acc-beat-dist* (float 3/2)) ; used w/ acc-dist-score: at a distance of *acc-beat-dist* beats, a spelling decision has a "weight" of *acc-dist-score*
(defparameter *acc-octave-dist* (float 2)) ; used w/ acc-dist-score: at a distance of *acc-octave-dist* octaves, a spelling decision has a "weight" of *acc-dist-score*

;; calculate some form of cartesian distance between two notes
;; link = whether or not notes are "linked" (example: the two notes of an artificial harmonic which must have highest spelling priority)
(declaim (type #-(or openmcl allegro) (float 0 1) #+(or openmcl allegro) float *acc-beat-dist-sc* *acc-octave-dist-sc*))
(declaim (special *acc-beat-dist-sc* *acc-octave-dist-sc*))
(defun nokey-notedist (link note1 off1 eoff1 note2 off2 eoff2)
  (declare (type boolean link) (type rational note1 note2) (type (real 0) off1 eoff1 off2 eoff2))
  (if link 3.0
      (distance (expt *acc-beat-dist-sc* (max (- off2 eoff1) (- off1 eoff2) 0.0))
		(expt *acc-octave-dist-sc* (* (diff note1 note2) (float 1/12))))))

;; allowed qualities for ea. diatonic interval
(declaim (type (vector cons) +nokey-niceints1+ +nokey-niceints2+ +nokey-penalty+)
	 (type (vector integer) +nokey-harmints+))
(defparameter +nokey-niceints1+ (vector '(0) '(-1 1) '(-1 1) '(0 2) '(-2 0) '(-1 1) '(-1 1))) ; best spellings from unison to 7th (0 = perfect, -1/1 = min/maj, -2/2 = dim/aug)
(defparameter +nokey-niceints2+ (vector '(-2 2) '(-2 2) '(-2 2) '(-2) '(2) '(-2 2) '(-2 2))) ; 2nd best spellings
(defparameter +nokey-penalty+ (vector '(1) '(-1 1) '(-1) '(1) '(-1 1) '(-1 1) '(-1))) ; for ea. white key, location of neighboring black keys (1/2 step lower or higher)
(defparameter +nokey-harmints+ (vector 0 1 1 2 2 3 4 4 5 5 6 6)) ; when two notes are "linked", required interval given the chromatic distance

(declaim (type #-(or openmcl allegro) (float 0 1) #+(or openmcl allegro) float
	       *acc-diatonic-int-score* *acc-aug-dim-int-score* *acc-spelling-penalty* *acc-good-unison-score* *acc-bad-unison-score* *acc-similar-qtone-score*))
(defparameter *acc-diatonic-int-score* (float 7/8)) ; score for forming a diatonic interval
(defparameter *acc-aug-dim-int-score* (float 1/2)) ; score for almost forming a diatonic interval
(defparameter *acc-spelling-penalty* (float 1/4)) ; penalty for using double sharps or flats
(defparameter *acc-good-unison-score* (float 1)) ; score for spelling upwards motion w/ sharps (and downwards w/ flats)
(defparameter *acc-bad-unison-score* (float 3/8)) ; penalty for not doing the above
(defparameter *acc-similar-qtone-score* (float 1/3)) ; score for using similar quartertones in close proximity to each other

;; calculate spelling penalty for using double sharps or double flats (for no quartertones & quartertones)
;; it's a separate function so it may be passed as an argument
;; n = note, a = accidental
;; returns: score
(defun nokey-notepen (n a)
  (declare (type rational n) (type (or (integer -2 2) (integer -2 2)) a))
  (* (mloop ; mloop used for compatibility w/ CLISP
      for e of-type (integer -1 1) in (cons 0 (svref +nokey-penalty+ (notespelling n a)))
      minimize (diff a e)) *acc-spelling-penalty*))
(defun nokeyq-notepen (n a)
  (declare (type rational n) (type (or (integer -2 2) (cons (integer -2 2) (rational -1/2 1/2))) a))
  (* (mloop
      for e of-type (integer -1 1) in (cons 0 (svref +nokey-penalty+ (qnotespelling n a)))
      minimize (diff (car a) e)) *acc-spelling-penalty*))

;; calculate score given two notes
;; link = whether or not notes are "linked" (example: the two notes of an artificial harmonic which must have highest spelling priority)
;; qt = is function being called from nokeyq-intscore?
;; returns value from 0.0 to 1.0
(defun nokey-intscore (link note1 acc1 off1 eoff1 note2 acc2 off2 eoff2 &optional qt)
  (declare (type boolean link qt) (type (integer -2 2) acc1 acc2) (type rational note1 note2) (type (rational 0) off1 eoff1 off2 eoff2))
  (multiple-value-bind (n1 a1 o1 eo1 n2 a2 o2 eo2)
      (if (= off1 off2)
	  (if (<= (- note1 acc1) (- note2 acc2))
	      (values note1 acc1 off1 eoff1 note2 acc2 off2 eoff2)
	      (values note2 acc2 off2 eoff2 note1 acc1 off1 eoff1))
	  (if (< off1 off2)
	      (values note1 acc1 off1 eoff1 note2 acc2 off2 eoff2)
	      (values note2 acc2 off2 eoff2 note1 acc1 off1 eoff1)))
    (declare (ignorable o1 eo1 o2 eo2))
    (multiple-value-bind (i q) (interval n1 a1 n2 a2)
      (let ((v (- (cond ((and link (/= i (svref +nokey-harmints+ (mod (diff n1 n2) 12)))) 0.0)
			((find q (svref +nokey-niceints1+ i)) *acc-diatonic-int-score*)
			((and (= i 0)	; unisons special case
			      (or 
			       (and (>= a1 0) (= (- a2 a1) 1))
			       (and (<= a1 0) (= (- a2 a1) -1))))
			 (if (<= eo1 o2) *acc-good-unison-score* *acc-bad-unison-score*))
			((find q (svref +nokey-niceints2+ i)) *acc-aug-dim-int-score*)
			(t 0.0))
		  (nokey-notepen n1 a1)
		  (nokey-notepen n2 a2))))
	(if qt v (max v 0.0))))))
(defun nokeyq-intscore (link note1 acc1 off1 eoff1 note2 acc2 off2 eoff2)
  (declare (type boolean link) (type (cons (integer -2 2) (rational -1/2 1/2)) acc1 acc2) (type rational note1 note2) (type (rational 0) off1 eoff1 off2 eoff2))
  (let ((aa1 (car acc1)) (aa2 (car acc2))
	(qa1 (cdr acc1)) (qa2 (cdr acc2)))
    (let ((s (nokey-intscore link (- note1 qa1) aa1 off1 eoff1 (- note2 qa2) aa2 off2 eoff2 t)))
      (if (and (= qa1 0) (= qa2 0)) (max s 0.0)
	  (let ((a1 (if (= qa1 0) aa1 qa1))
		(a2 (if (= qa2 0) aa2 qa2)))
	    (min (max (if (or (and (> a1 0) (< a2 0)) (and (< a1 0) (> a2 0)))
			  (if link 0.0
			      (let ((m (if (and (/= qa1 0) (/= qa2 0)) *acc-similar-qtone-score* (* *acc-similar-qtone-score* 0.5))))
				(if (= (qnotespelling note1 acc1) (qnotespelling note2 acc2)) (+ s m) (- s m)))) ; penalize different accs on different written notes
			  s)
		      0.0) 1.0))))))

;; default heap size
(declaim (type (integer 1) *acc-engine-heap*))
(defparameter *acc-engine-heap* 30)

;; structure encapsulating node in search
(defstruct (nokeynode (:copier nil) (:predicate nokeynodep))
  (sc 0.0 :type #-(or allegro lispworks) (float 0) #+(or allegro lispworks) float) ; score so far
  (ret nil :type list) ; list of return events (solution so far)
  (re 0 :type (integer 0)) ; number of remaining decisions
  (evs nil :type list) ; remaining events
  (evc nil :type list) ; events effecting current decision
  (evd nil :type list) ; events to redo (that will need redoing)
  (o 0 :type (rational 0)) ; offset counter
  (co 0 :type (integer 0))) ; counter

;; "main" accidentals-spelling function
(defun acc-nokey (events choices spellfun penfun intscorefun name conv) ; events in one part
  (declare (type list events choices)
	   (type (function (rational (or (integer -2 2) (cons (integer -2 2) (rational -1/2 1/2)))) (values (or (integer 0 6) null) (or integer null))) spellfun)
	   (type (function (boolean rational (or (integer -2 2) (cons (integer -2 2) (rational -1/2 1/2))) (rational 0) (rational 0)
				    rational (or (integer -2 2) (cons (integer -2 2) (rational -1/2 1/2))) (rational 0) (rational 0))
			   #-(or openmcl allegro) (float 0 1) #+(or openmcl allegro) float) intscorefun)
	   (type (or string null) name) (type (function ((or (cons (integer -2 2) (rational -1/2 1/2)) (integer -2 2))) cons) conv))
  (let ((co 0)
	(mxd (* *acc-beat-dist* *max-acc-beat-dist-mul*))
	(cho (mapcar conv choices)))
    (declare (type (integer 0) co))
    (flet ((scorefun (no)
	     (declare (type nokeynode no))
	     (cons (+ (nokeynode-sc no)
		      (loop for e of-type (cons #-(or openmcl allegro) (float 0 1) #+(or openmcl allegro) float *) in (nokeynode-evd no) sum (car e))
		      (nokeynode-re no)) ; remaining accidentals all get scores of 1.0
		   (nokeynode-co no)))
	   (expandfun (no)
	     (declare (type nokeynode no))
	     (when (> (nokeynode-co no) co) ; show progress
	       (setf co (nokeynode-co no))
	       (print-dot))
	     (loop
	      with f of-type noteex = (first (nokeynode-evs no)) and lf = (rest (nokeynode-evs no))
	      with o = (event-note* f) and oo = (event-off f)
	      with nco = (if (or (null (nokeynode-o no)) (> oo (nokeynode-o no))) (1+ (nokeynode-co no)) (nokeynode-co no))
	      for e of-type (or (integer -2 2) (cons (integer -2 2) (rational -1/2 1/2)))
	      in (or (loop for a in (intersection
				     (mapcar conv
					     (let ((x (event-useracc f)))
					       (if (and (listp x) (listp (rest x))) x
						   (list x))))
				     cho :test #'equal) ; e = lists of accs.
			   when (funcall spellfun o a) collect a)
		     (loop for a in cho if (funcall spellfun o a) collect a) ; ignore user's suggestion
		     (error "No accidentals possible for note ~S at offset ~S, part ~S" (event-note f) (event-foff f) name))
	      collect (let ((w (copy-event f :note (cons (event-note* f) e)))
			    (s (nokeynode-sc no)))
			(let ((d (cons w 
				       (or (loop ; keep only relevant notes that will need rescoring
					    for e of-type (cons #-(or openmcl allegro) (float 0) #+(or openmcl allegro) float note) in (nokeynode-evd no) ; e is (score . event)
					    if (> (event-endoff (cdr e)) oo)
					    collect (cdr e)	; collect just the events
					    else do (incf s (car e)))
					   (let ((mx (mloop
						      for e of-type (cons #-(or openmcl allegro) (float 0) #+(or openmcl allegro) float note) in (nokeynode-evd no)
						      maximize (event-endoff (cdr e)))))
					     (setf s (nokeynode-sc no))
					     (loop for e of-type (cons #-(or openmcl allegro) (float 0) #+(or openmcl allegro) float note) in (nokeynode-evd no)
						   if (>= (event-endoff (cdr e)) mx)
						   collect (cdr e)
						   else do (incf s (car e)))))))
			      (c (cons w (let ((o (- oo mxd)))
					   (remove-if (lambda (e)
							(declare (type noteex e))
							(<= (event-endoff e) o))
						      (nokeynode-evc no))))))
			  (make-nokeynode
			   :sc s :evc c :o oo :co nco
			   :evd (loop
				 for e of-type noteex in d 
				 collect (cons
					  (let* ((eua (event-useracc e))
						 (ne (event-note* e))
						 (su (- 1.0 (funcall penfun ne eua))) (di 1.0))
					    (declare (type #-(or openmcl allegro) (float 0) #+(or openmcl allegro) float su di))
					    (loop ; plus scores of 1.0 for rest in range
					     for e0 of-type noteex in lf
					     while (<= (event-off e0) (event-off e))
					     do (incf su) (incf di))
					    (loop
					     with eoe = (event-endoff e)
					     and foe = (float (event-off e))
					     and feoe = (float (event-endoff e))
					     for e0 of-type noteex in c
					     unless (eq e e0)
					     do (let* ((ne0 (event-note* e0)) (eoe0 (event-endoff e0))
						       (ti (and (event-acctie e) (event-acctie e0) (eq (event-acctie e) (event-acctie e0))))
						       (x (nokey-notedist ti ne foe feoe ne0 (event-off e0) eoe0)))
						  (incf su (* (funcall intscorefun ti
								       ne eua (event-off e) eoe
								       ne0 (event-useracc e0) (event-off e0) eoe0)
							      x))
						  (incf di x)))
					    (/ su di))
					  e))
			   :re (1- (nokeynode-re no)) :ret (cons w (nokeynode-ret no))
			   :evs lf)))))
	   (scoregreaterfun (s1 s2) (declare (type (cons #-(or openmcl allegro) (float 0) #+(or openmcl allegro) float *) s1 s2)) (> (car s1) (car s2)))
	   (remscoregreaterfun (r1 r2)
	     (declare (type (cons #-(or openmcl allegro) (float 0) #+(or openmcl allegro) float (integer 0)) r1 r2))
	     (if (= (cdr r1) (cdr r2)) (< (car r1) (car r2)) (< (cdr r1) (cdr r2))))
	   (solutfun (no) (declare (type nokeynode no)) (null (nokeynode-evs no))))
      (nokeynode-ret
       (or (let ((*max-acc-beat-dist-mul* (1+ (* (- *max-acc-beat-dist-mul* 1) *quality*)))
		 (*acc-engine-heap* (max (roundint (* *acc-engine-heap* *quality*)) 1))
		 (*acc-beat-dist-sc* (expt *acc-dist-score* (/ *acc-beat-dist*)))
		 (*acc-octave-dist-sc* (expt *acc-dist-score* (/ *acc-octave-dist*))))
	     (bfs*-engine (list (make-nokeynode :re (length events) :evs events))	; should be sorted already
			  #'scorefun
			  #'expandfun
			  #'solutfun
			  :heaplim *acc-engine-heap*
			  :scoregreaterfun #'scoregreaterfun
			  :remscoregreaterfun #'remscoregreaterfun))
	   (error "Cannot find valid note spellings for part ~S" name))))))

(declaim (type boolean *use-double-accs*))
(defparameter *use-double-accs* nil) ; setting

(declaim (inline load-acc-modules))
(defun load-acc-modules ()
  (unless (eq (auto-accs-fun) :acc1) (load-fomus-module (auto-accs-fun))))

;; wrapper to bind variable returned by module (informs fomus of which caut. accidentals algorithm to use, probably choice between nokey and key)
(declaim (special *module-cautacc-fun* *module-postacc-fun*))
(defmacro set-acc-modulevar (&body forms)
  `(let ((*module-cautacc-fun* nil)
	 (*module-postacc-fun* nil))
    ,@forms))

;; dispatch function for accidentals processing--called before chords exist and before voices are separated
;; returns: modified parts, sorted according to sort-offdur
(defun accidentals (parts timesigs)
  (declare (type list parts timesigs))
  (loop
   initially (when (eq (auto-accs-fun) :acc1) (acc-fakekeysig parts timesigs))
   for e of-type partex in parts
   unless (is-percussion e)
   do (multiple-value-bind (evs rs) (split-list (part-events e) #'notep)
	(setf (part-events e)
	      (sort (nconc rs
			   (if (eq (auto-accs-fun) :acc1)
			       (if *quartertones*
				   (acc-nokey evs (if *use-double-accs* +acc-qtones-double+ +acc-qtones-single+)
					      #'qnotespelling #'nokeyq-notepen #'nokeyq-intscore (part-name e) #'convert-qtone)
				   (acc-nokey evs (if *use-double-accs* +acc-double+ +acc-single+)
					      #'notespelling #'nokey-notepen #'nokey-intscore (part-name e) #'identity)) 
			       (multiple-value-bind (r1 r2 r3) (call-module (auto-accs-fun) (list "Unknown accidental assignment module ~S" *auto-accs-module*) evs)
				 (setf *module-cautacc-fun* r2 *module-postacc-fun* r3)
				 r1)))
		    #'sort-offdur)))
   finally (when (eq (auto-accs-fun) :acc1) (acc-delfakes parts))))

;; wrapper to set semitones/quartones according to selected accidentals module
(defmacro set-note-precision (&body forms)
  `(let ((*note-precision* (if *quartertones* 1/2 1)))
    ,@forms))

;; called when no accidentals module is chosen (gives dumb assignments)
(defun accidentals-generic (parts)
  (declare (type list parts))
  (flet ((so (d)
	   (lambda (x y)
	     (let ((ax (if (consp x) (car x) x))
		   (ay (if (consp y) (car y) y)))
	       (if (= (abs ax) (abs ay)) 
		   (funcall d ax ay)
		   (< (abs ax) (abs ay)))))))
    (loop with cho = (if *quartertones*
			 (mapcar #'convert-qtone +acc-qtones-double+)
			 +acc-double+)
	  with chof = (stable-sort (copy-list cho) (so #'<))
	  and chos = (stable-sort (copy-list cho) (so #'>))
	  for p of-type partex in parts
	  unless (is-percussion p)
	  do (loop for e of-type (or noteex restex) in (part-events p)
		   do (let ((n (event-note* e)))
			(setf (event-note e)
			      (cons n (find-if (lambda (a) (if (consp a) (qnotespelling n a) (notespelling n a)))
					       (append (event-useracc e) (let ((m (mod n 12))) (if (and (>= m 9/2) (<= m 7)) chos chof)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CAUTIONARY ACCIDENTALS

(declaim (type (real (0)) *max-caut-acc-dist*)
	 (type boolean *caut-acc-ottavas*)
	 (type (or boolean (integer 1)) *caut-acc-octaves*)
	 (type (or boolean (integer 1 2)) *caut-acc-next-meas*))
(defparameter *max-caut-acc-dist* 2) ; setting--max. distance to care about caut. accidentals
(defparameter *caut-acc-ottavas* t) ; setting--pay attention to octave transpositions?
(defparameter *caut-acc-octaves* 1) ; setting--can be a number (for number of octaves above/below) or t for all
(defparameter *caut-acc-next-meas* t) ; setting--care about caut. accidentals across measures? (1 = across only one meaure boundary, 2/t = across up to two measure boundaries)

;; finds and adds cautionary accidentals (adds CAUTACC marks to note events)
;; rests are removed before calling, called before chords or ties are added
;; meas = list of measures
;; returns: nil
(defun acc-nokey-cautaccs (meas)
  (declare (type list meas))
  (loop
   with r of-type (integer -12 12) = 0
   and as = (make-array 128 :element-type '(or null (cons (or boolean (integer -12 12)) noteex)) :initial-element nil)
   and ad = (make-array 128 :element-type '(or null (cons (or boolean (integer -12 12)) noteex)) :initial-element nil)
   for mo = -1 then mo0
   for (mo0 . m) of-type ((real 0) . list) in meas do
   (loop
    for e of-type noteex in m
    for o = (- (event-off e) *max-caut-acc-dist*) and w = (event-writtennote e)
    when *caut-acc-ottavas* do
    (cond ((or (getmark e :start8up-) (getmark e :8up)) (setf r 12))
	  ((or (getmark e :start8down-) (getmark e :8down)) (setf r -12)))
    if (and (= (event-acc e) 0) (= (event-addacc e) 0)) ; event has a natural
    do (flet ((ok (wh x)
		(declare (type (array (or nil (cons (or boolean (integer -12 12)) noteex)) (128)) wh) (type integer x))
		(and (>= (event-endoff (cdr (svref wh x))) o)
		     (or (member *caut-acc-next-meas* '(t 2))
			 (>= (event-off (cdr (svref wh x))) (if *caut-acc-next-meas* mo mo0))))))
	 (loop
	  with or = (loop with x = (* (if (truep *caut-acc-octaves*) 7 (or *caut-acc-octaves* 0)) 12) and r
			  for ww from (- w x) to (+ w x) by 12
			  when (and (>= ww 0) (< ww 128) (svref ad ww) (ok ad ww)) do (setf (svref ad ww) nil) (setf r t)
			  finally (return r))
	  for i from -24 to 24 by 12 ; possible ottava differences (8up = 12)
	  do (let ((ww (- w i))) ; written note relative with respect to ottava difference
	       (when (and (>= ww 0) (< ww 128) (svref as ww) (ok as ww) (= (- r (car (svref as ww))) i))
		 (setf (svref as ww) nil or t))) 
	  finally (let ((ma (list :cautacc (event-note* e))))
		    (when (and or (not (getmark e ma))) (addmark e ma)))))
    else do (setf (svref as w) (cons r e) (svref ad w) (cons r e)) ; save register and event has an accidental
    when (and *caut-acc-ottavas* (or (getmark e :end8up-) (getmark e :8up) (getmark e :end8down-) (getmark e :8down)))
    do (setf r 0)))
  (print-dot))

;; dispatch function for cautionary accidentals
;; cautaccs is in difficult place in task list--consolidate parts and collect events by staves, not voices
(defun cautaccs (parts)
  (declare (type list parts))
  (loop for pa of-type cons in (split-into-groups (remove-if #'is-percussion parts) #'part-userord) do
	(let ((ms (apply #'mapcar (lambda (&rest ms) ; list of simultaneous measures for 1 part
				    (cons (meas-off (first ms))
					  (sort (loop for m of-type meas in ms nconc (delete-if-not #'notep (copy-list (meas-events m))))
						#'sort-offdur)))
			 (mapcar #'part-meas pa))))
	  (if (eq (auto-accs-fun) :acc1) (acc-nokey-cautaccs ms)
	      (funcall (or *module-cautacc-fun* #'acc-nokey-cautaccs) ms)))))

;; find user CAUTACC marks and insert note numbers
;; returns: nil
(defun preproc-cautaccs (parts)
  (declare (type list parts))
  (loop for p of-type partex in parts do
	(loop for e of-type (or noteex restex) in (part-events p)
	      when (popmark e :cautacc) do (addmark e (list :cautacc (event-note* e))))
	(print-dot)))

;; aliases for key signatures
(defparameter +keysig-eqs+
  '((:fsmin . :f+min) (:csmin . :c+min) (:gsmin . :g+min) (:cfmaj . :c-maj) (:afmin . :a-min) (:fsmaj . :f+maj)
    (:dsmin . :d+min) (:gfmaj . :g-maj) (:efmin . :e-min) (:csmaj . :c+maj) (:asmin . :a+min) (:dfmaj . :d-maj)
    (:bfmin . :b-min) (:afmaj . :a-maj) (:efmaj . :e-maj) (:bfmaj . :b-maj)
    (:cs . :c+) (:df . :d-) (:ds . :d+) (:ef . :e-) (:fs . :f+) (:gf . :g-) (:gs . :g+) (:af . :a-) (:as . :a+) (:bf . :b-)))

;; replace key signature aliases
(defun preproc-keysigs (timesigs)
  (declare (type list timesigs))
  (loop for ts of-type timesig-repl in timesigs
	for k = (popprop ts :keysig)
	when k do (addprop ts (cons :keysig (mapcar (lambda (x) (declare (type symbol x)) (or (lookup x +keysig-eqs+) x)) (rest k))))))

;; add grace notes with accidentals to fool FOMUS into thinking about key signatures
(defun acc-fakekeysig (parts timesigs)
  (declare (type list parts timesigs))
  (let ((h (get-timesigs timesigs parts))
	(lg (let ((x (mloop for p of-type partex in parts minimize
			    (mloop for e of-type (or noteex restex) in (part-events p) for g = (event-grace e) when g minimize g))))
	      (if x (min (1- x) -1) -1))))
    (loop for p of-type partex in parts 
	  for ts = (gethash p h)
	  and evs = (part-events p)
	  unless (is-percussion p) do
	  (loop with kk = nil
		for (s ns) of-type (timesig-repl (or timesig-repl null)) on ts
		for ks = (getprop s :keysig)
		when ks do (setf kk (keysig-accs (rest ks)))
		do (loop
		    #-clisp while #-clisp (if ns (when evs (< (event-off (first evs)) (timesig-off ns))) evs)
		    for e of-type (or noteex restex) = #-clisp (pop evs) #+clisp (if (if ns (when evs (< (event-off (first evs)) (timesig-off ns))) evs) (pop evs) (return))
		    for (n . a) of-type ((or (integer 0) null) . (or (integer -1 1) null)) = (when (notep e) (find (event-note* e) kk :key #'car))
		    when n do (push (make-instance 'noteex :beamlt 'f :note (list n a) :off (event-off e) :dur (cons 1 lg))
				    (part-events p))))
	  (setf (part-events p) (sort (part-events p) #'sort-offdur)))))
(defun acc-postfakekeysig (parts)
  (declare (type list parts))
  (let ((lg (let ((x (mloop for p of-type partex in parts minimize
			    (mloop for m of-type meas in (part-meas p) minimize
				   (mloop for e of-type (or noteex restex) in (meas-events m) for g = (event-grace e) when g minimize g)))))
	      (if x (min (1- x) -1) -1))))
    (loop for p of-type partex in parts
	  unless (is-percussion p) do 
	  (loop with kk = nil
		for m of-type meas in (part-meas p)
		for o = (meas-off m)
		and ks = (getprop (meas-timesig m) :keysig)
		when ks do (setf kk (keysig-accs (rest ks)))
		do (loop for (n . a) of-type ((integer 0) . (integer -1 1)) in kk
			 do (push (make-instance 'noteex :beamlt 'f :note (cons n a) :off o :dur (cons 1 lg))
				  (meas-events m)))
		(setf (meas-events m) (sort (meas-events m) #'sort-offdur))))))

;; delete the "fake" key signatures
(defun acc-delfakes (parts)
  (declare (type list parts))
  (loop for p of-type partex in parts
	do (setf (part-events p)
		 (sort (loop for e of-type (or noteex restex) in (part-events p) unless (and (typep e 'noteex) (event-fakenote e)) collect e) #'sort-offdur))))
(defun acc-postdelfakes (parts)
  (declare (type list parts))
  (loop for p of-type partex in parts
	do (loop for m of-type meas in (part-meas p) do
		 (setf (meas-events m)
		       (sort (loop for e of-type (or noteex restex) in (meas-events m) unless (and (typep e 'noteex) (event-fakenote e)) collect e) #'sort-offdur)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACCIDENTALS POST PROCESSING

;; final processing of all FORCEACC, CAUTACC, SHOWACC and HIDEACC marks (decides when to hide/show naturals and repeated accidentals in measures)
;; rests are removed before calling, called after chords and ties are added
;; events = note events in 1 measure
;; returns: nil
(defun acc-nokey-postaccs (events)
  (declare (type cons events))
  (let ((as (make-array 128 :element-type '(or null (cons (integer -2 2) (rational -1/2 1/2))) :initial-element nil)) ; accidentals so far
	(ac (make-array 128 :element-type 'boolean :initial-element nil))) ; next one cautionary accidental?
    (flet ((fixacc (e n a a2 tl)
	     (declare (type noteex e) (type rational n) (type (integer -2 2) a) (type (rational -1/2 1/2) a2) (type boolean tl))
	     (let ((w (- n a a2)))
	       (if tl
		   (setf (svref as w) (unless (and (= a 0) (= a2 0)) (cons a a2)) (svref ac w) t) ; right side of tie
		   (if (popmark e :forceacc) ; user :forceacc mark
		       (progn
			 (setf (svref as w) (cons a a2))
			 (rmmark e (list :cautacc n)) 
			 (addmark e (list :showacc n)))
		       (if (and (= a 0) (= a2 0))
			   (when (svref as w) ; show the natural 
			     (setf (svref as w) nil)
			     (rmmark e (list :cautacc n)) 
			     (addmark e (list (if (svref ac w) :cautacc :showacc) n))) 
			   (if (equal (svref as w) (cons a a2))
			       (unless (or (getmark e (list :cautacc n)) (getmark e (list :showacc n))) (addmark e (list :hideacc n))) 
			       (setf (svref as w) (cons a a2) (svref ac w) nil))))))))
      (loop
       for e of-type noteex in events
       if (chordp e)
       do (loop
	   for n of-type rational in (event-notes* e)
	   and a of-type (integer -2 2) in (event-accs e)
	   and a2 of-type (rational -1/2 1/2) in (event-addaccs e)
	   and tl of-type boolean in (event-tielt e)
	   do (fixacc e n a a2 tl))
       else do (fixacc e (event-note* e) (event-acc e) (event-addacc e) (event-tielt e)))))
  (print-dot))

;; dispatch function for accidentals post processing
(defun postaccs (parts)
  (declare (type list parts))
  (if *acc-throughout-meas*
      (loop
       initially (when (eq (auto-accs-fun) :acc1) (acc-postfakekeysig parts))
       for p of-type partex in parts unless (is-percussion p) do
       (loop for m of-type meas in (part-meas p) do
	     (multiple-value-bind (evs rs) (split-list (meas-events m) #'notep)
	       (loop for ev of-type cons in (split-into-groups evs #'event-staff) do
		     (if (eq (auto-accs-fun) :acc1)
			 (acc-nokey-postaccs (copy-list (sort ev #'sort-offdur))) 
			 (funcall (or *module-postacc-fun* #'acc-nokey-postaccs) (copy-list (sort ev #'sort-offdur)))))
	       (setf (meas-events m) (sort (nconc rs evs) #'sort-offdur))))
       finally (when (eq (auto-accs-fun) :acc1) (acc-postdelfakes parts)))
      (loop for p of-type partex in parts unless (is-percussion p) do
	    (loop for m of-type meas in (part-meas p) do
		  (loop for e of-type (or noteex restex) in (meas-events m)
			when (popmark e :forceacc) do (addmark e :showacc)))
	    (print-dot))))

