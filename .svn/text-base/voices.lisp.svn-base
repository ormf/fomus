;; -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**************************************************************************************************
;; FOMUS
;; voices.lisp
;;**************************************************************************************************

(in-package :fomus)
(compile-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VOICE DISTRIBUTION

;; user specifies a list of voices for voice parameter
;; algorithm decides which one to choose
(declaim (type #-(or openmcl allegro) (float 0 1) #+(or openmcl allegro) float
	       *voice-high/low-penalty* *voice-chord-score* *voice-leading-penalty* +voice-notsimult-mult+))
(defparameter *voice-high/low-penalty* (float 1)) ; 1/3--voice 1 is higher than voice 2
(defparameter *voice-chord-score* (float 1/4)) ; incentive to group notes of same offset/dur into same voice
(defparameter *voice-leading-penalty* (float 1)) ; 1/4--close-together notes are in same voice
(defparameter +voice-notsimult-mult+ (float 2/3))

(declaim (type #-(or openmcl allegro) (float 0 1) #+(or openmcl allegro) float *voice-dist-score*))
(defparameter *voice-dist-score* (float 1/4))
(declaim (type #-(or openmcl allegro) (float 0) #+(or openmcl allegro) float *voice-octave-dist*))
(defparameter *voice-octave-dist* (float 1))

(declaim (type #-(or openmcl allegro) (float 0) #+(or openmcl allegro) float *voice-high/low-beat-dist* *voice-leading-beat-dist*))
(defparameter *voice-high/low-beat-dist* (float 1))
(defparameter *voice-leading-beat-dist* (float 1))

(declaim (type (real 1) *max-voice-beat-dist-mul*))
(defparameter *max-voice-beat-dist-mul* 2) ; multiply by one of the dist values to get max beat dist

(declaim (type boolean *auto-voicing*))
(defparameter *auto-voicing* t)

(declaim (type symbol *auto-voices-plugin* *auto-voices-module*))
(defparameter *auto-voices-plugin* nil)
(defparameter *auto-voices-module* t)
(declaim (inline auto-voices-fun))
(defun auto-voices-fun () (if (truep *auto-voices-module*) :voices1 *auto-voices-module*))

(declaim (type #-(or openmcl allegro) (float 0 1) #+(or openmcl allegro) float
	       *voice-high/low-beat-dist-sc* *voice-leading-beat-dist-sc* *voice-octave-dist-sc* *voice-full-beat-dist-sc*)
	 (type #-(or openmcl allegro) (float 0) #+(or openmcl allegro) float *voice-full-beat-dist*)
	 (type #-(or openmcl allegro) (float (0)) #+(or openmcl allegro) float *voice-octave-dist-sc1*))
(declaim (special *voice-high/low-beat-dist-sc* *voice-leading-beat-dist-sc* *voice-octave-dist-sc1*
		  *voice-octave-dist-sc* *voice-full-beat-dist* *voice-full-beat-dist-sc*)) ; adj is 1 + lowest (farthest) value
(declaim (type #-(or openmcl allegro) (float 0 1) #+(or openmcl allegro) float +voices-notedist-aux-const+))
(defparameter +voices-notedist-aux-const+ (float 1/12))
(defun voices-notedist-aux1 (note1 note2) ; by octave, unison = 1
  (declare (type rational note1 note2))
  (expt *voice-octave-dist-sc* (* (diff note1 note2) +voices-notedist-aux-const+)))
(defun voices-notedist-aux0 (note1 note2) ; by octave, unison = 0, octave = 1
  (declare (type rational note1 note2))
  (expt (* (diff note1 note2) +voices-notedist-aux-const+) *voice-octave-dist-sc1*))
(defun voices-notedist-aux2 (off1 eoff1 off2 eoff2 beatdist sc) ; by offset
  (declare (type (rational 0) off1 eoff1 off2 eoff2) (type (real 0) beatdist) (type #-(or openmcl allegro) (float 0 1) #+(or openmcl allegro) float sc))
  (let ((d (max (- (float off2) (float eoff1)) (- (float off1) (float eoff2)) 0.0)))
    (if (>= d (* *max-voice-beat-dist-mul* beatdist)) 0.0
	(expt sc d))))
(defun voices-notedist (note1 off1 eoff1 note2 off2 eoff2)
  (declare (type (rational (0)) note1 note2) (type (rational 0) off1 eoff1 off2 eoff2))
  (let ((a1 (voices-notedist-aux1 note1 note2)) ; unison = 1
	(a0 (voices-notedist-aux0 note1 note2))) ; unison = 0
    (list (voices-notedist-aux2 off1 eoff1 off2 eoff2 *voice-high/low-beat-dist* *voice-high/low-beat-dist-sc*)	; high/low
	  (distance (voices-notedist-aux2 off1 eoff1 off2 eoff2 *voice-leading-beat-dist* *voice-leading-beat-dist-sc*) a0) ; simult voices (larger vert. dist., greater penalty)
	  a1		       ; chords (smaller dist., greater score)
	  (distance (voices-notedist-aux2 off1 eoff1 off2 eoff2 *voice-leading-beat-dist* *voice-leading-beat-dist-sc*) a1)))) ;vd ; voice leading (larger dist, smaller penalty)
(declaim (inline voices-fulldist))
(defun voices-fulldist (off1 eoff1 off2 eoff2)
  (declare (type (rational 0) off1 eoff1 off2 eoff2))
  (voices-notedist-aux2 off1 eoff1 off2 eoff2 *voice-full-beat-dist* *voice-full-beat-dist-sc*))

;; score for voice-top/bottom, voice-leading, voice-balance
;; also incorporates distance
(defun voices-score (note1 vo1 off1 eoff1 note2 vo2 off2 eoff2)	; note1 must be higher voice (1 & 2 must be in order)
  (declare (type rational note1 note2) (type (integer 1) vo1 vo2) (type (rational 0) off1 eoff1 off2 eoff2))
  (let ((ve (and (>= eoff1 off2) (>= eoff2 off1))))
    (list (if (or (and (< vo1 vo2) (< note1 note2))
		  (and (> vo1 vo2) (> note1 note2)))
	      (if ve *voice-high/low-penalty* (* *voice-high/low-penalty* +voice-notsimult-mult+)) 0.0)
	  (if (= vo1 vo2) (if ve *voice-leading-penalty* (* *voice-leading-penalty* +voice-notsimult-mult+)) 0.0)
	  (if (and (= vo1 vo2) (= off1 off2) (= eoff1 eoff2)) (- *voice-chord-score*) 0.0)
	  (if (= vo1 vo2) (if ve (- *voice-leading-penalty*) (* (- *voice-leading-penalty*) +voice-notsimult-mult+)) 0.0))))
(defun voices-fullscore (score dist)
  (declare (type cons score dist))
  (ave-list (mapcar #'* score dist)))

(declaim (type (integer 1) *voice-engine-heap*))
(defparameter *voice-engine-heap* 30)

(defstruct (voicenode (:copier nil) (:predicate voicenodep))
  (sc 0.0 :type #-(or allegro lispworks) (float 0) #+(or allegro lispworks) float)
  (ret nil :type list)
  (evs nil :type list)
  (evc nil :type list)
  (evd nil :type list)
  (o 0 :type (rational 0))
  (co 0 :type (integer 0)))
(defun voices-bydist (events instr name)
  (declare (type list events) (type instr instr) (type (or string null) name))
  (let ((co 0))
    (declare (type (integer 0) co))
    (flet ((scorefun (no)
	     (declare (type voicenode no))
	     (cons (+ (voicenode-sc no)
		      (loop for e of-type (cons #-(or openmcl allegro) (float 0 1) #+(or openmcl allegro) float *) in (voicenode-evd no) sum (car e)))
		   (voicenode-co no)))
	   (expandfun (no)
	     (declare (type voicenode no))
	     (when (> (voicenode-co no) co) ;; progress
	       (setf co (voicenode-co no))
	       (print-dot))
	     (loop for aa in '(nil t)
		   thereis (loop
			    with f of-type noteex = (first (voicenode-evs no)) and lf = (rest (voicenode-evs no))
			    with oo = (event-off f)
			    with nco = (if (or (null (voicenode-o no)) (> oo (voicenode-o no))) (1+ (voicenode-co no)) (voicenode-co no))
			    and al 
			    for e of-type (integer 1)
			    in (or (when (event-acctie f)
				     (let ((x (find (event-acctie f) (voicenode-evd no) :key (lambda (x) (event-acctie (cdr x))))))
				       (when x (setf al t) (list (event-voice (cdr x))))))
				   (force-list (event-voice f)))
			    for xx = (let ((w (copy-event f :voice e))
					   (s (voicenode-sc no)))
				       (let ((d (cons w 
						      (loop ; keep only relevant notes that will need rescoring (endoff > - 8 beats)
						       for e of-type (cons #-(or openmcl allegro) (float 0 1) #+(or openmcl allegro) float note) in (voicenode-evd no) ; e is (score . event)
						       if (>= (event-off (cdr e)) oo) ; endoff will = offset for grace notes!
						       collect (cdr e) ; collect just the events
						       else do (incf s (car e)))))
					     (c (cons w (let ((o (- oo (* *voice-full-beat-dist* *max-voice-beat-dist-mul*))))
							  (remove-if (lambda (e) (declare (type noteex e)) (<= (event-endoff e) o)) (voicenode-evc no))))))
					 (when (let ((i (instr-simultlim instr)))
						 (or al aa (null i) (<= (count-if (lambda (x)
										    (declare (type noteex x))
										    (and (> (event-endoff x) oo) (= (event-voice x) e))) c) i)))
					   (make-voicenode
					    :sc s :evc c
					    :evd (loop
						  for e of-type noteex in d 
						  collect (cons
							   (loop
							    with su of-type #-(or openmcl allegro) (float 0) #+(or openmcl allegro) float = 0.0
							    and di of-type #-(or openmcl allegro) (float 0) #+(or openmcl allegro) float = 0.0
							    for e0 of-type noteex in c
							    unless (eq e e0)
							    do (let ((d0 (voices-notedist (event-note* e) (event-off e) (event-endoff e)
											  (event-note* e0) (event-off e0) (event-endoff e0)))
								     (s0 (voices-score (event-note* e) (event-voice e) (event-off e) (event-endoff e)
										       (event-note* e0) (event-voice e0) (event-off e0) (event-endoff e0))))
								 (incf su (voices-fullscore s0 d0))
								 (incf di (voices-fulldist (event-off e) (event-endoff e) (event-off e0) (event-endoff e0))))
							    finally (return (if (> di 0.0) (/ su di) 0.0)))
							   e))
					    :ret (cons w (voicenode-ret no))
					    :evs lf :co nco))))
			    when xx collect xx)))
	   (scoregreaterfun (s1 s2) (declare (type (cons #-(or openmcl allegro) (float 0) #+(or openmcl allegro) float *) s1 s2)) (< (car s1) (car s2)))
	   (remscoregreaterfun (r1 r2)
	     (declare (type (cons #-(or openmcl allegro) (float 0) #+(or openmcl allegro) float (integer 0)) r1 r2))
	     (if (= (cdr r1) (cdr r2)) (> (car r1) (car r2)) (< (cdr r1) (cdr r2))))
	   (solutfun (no) (declare (type voicenode no)) (null (voicenode-evs no))))
      (voicenode-ret
       (or (let ((*voice-full-beat-dist* (max *voice-high/low-beat-dist* *voice-leading-beat-dist*)))
	     (let ((*max-voice-beat-dist-mul* (1+ (* (- *max-voice-beat-dist-mul* 1) *quality*)))
		   (*voice-engine-heap* (max (roundint (* *voice-engine-heap* *quality*)) 1))
		   (*voice-high/low-beat-dist-sc* (expt *voice-dist-score* (/ *voice-high/low-beat-dist*)))
		   (*voice-leading-beat-dist-sc* (expt *voice-dist-score* (/ *voice-leading-beat-dist*)))
		   (*voice-full-beat-dist-sc* (expt *voice-dist-score* (/ *voice-full-beat-dist*)))
		   (*voice-octave-dist-sc* (expt *voice-dist-score* (/ *voice-octave-dist*))))
	       (let ((*voice-octave-dist-sc1* (/ *voice-octave-dist-sc*)))
		 (bfs*-engine (list (make-voicenode :evs events))
			      #'scorefun
			      #'expandfun
			      #'solutfun
			      :heaplim *voice-engine-heap*
			      :scoregreaterfun #'scoregreaterfun
			      :remscoregreaterfun #'remscoregreaterfun))))
	   (error "Cannot distribute voices within limits of specified instrument in part ~S" name))))))

(defun voices-setvoice (events name)
  (declare (type list events))
  (loop for e of-type (or noteex restex) in events when (listp (event-voice e)) do
	(setf (event-voice e) (if (event-voice e) (if (list>1p (event-voice e))
						      (error "Only one voice allowed when :AUTO-VOICING is NIL in note at offset ~S, part ~S" (event-foff e) name)
						      (first (event-voice e))) 1))))

(declaim (inline load-voices-modules))
(defun load-voices-modules ()
  (unless (eq (auto-voices-fun) :voices1) (load-fomus-module (auto-voices-fun))))

;; distribute ambiguous voice assignments (lists)
(defun voices (parts)
  (declare (type list parts))
  (loop
   for e of-type partex in parts
   if (is-percussion e) do (voices-setvoice (part-events e) (part-name e))
   else do (multiple-value-bind (evs rs) (split-list (part-events e) #'notep)
	     (setf (part-events e)
		   (sort (nconc (loop ; copy rests to all voices if voice slot is a list
				 for e of-type restex in rs
				 if (listp (event-voice e)) nconc (mapc (lambda (i) (declare (type (integer 1) i)) (copy-event e :voice i)) (event-voice e))
				 else collect e)
				(if (eq (auto-voices-fun) :voices1)
				    (voices-bydist evs (part-instr e) (part-name e)) 
				    (call-module (auto-voices-fun) (list "Unknown voice distribution module ~S" *auto-voices-module*) evs (part-instr e) (part-name e))))
			 #'sort-offdur)))))

(defun voices-generic (parts)
  (declare (type list parts))
  (loop for p of-type partex in parts do (voices-setvoice (part-events p) (part-name p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMBINE VOICES

;; in beats
(declaim (type (real (0)) *min-multivoice-chords-dur*)
	 (type boolean *auto-multivoice-rests* *auto-multivoice-notes*))
(defparameter *min-multivoice-chords-dur* 1)
(defparameter *auto-multivoice-rests* t) ; into single rests
(defparameter *auto-multivoice-notes* t) ; into chords

(declaim (type symbol *auto-multivoice-comb-plugin* *auto-multivoice-comb-module*))
(defparameter *auto-multivoice-comb-plugin* nil)
(defparameter *auto-multivoice-comb-module* t)
(declaim (inline auto-multivoice-comb-fun))
(defun auto-multivoice-comb-fun () (if (truep *auto-multivoice-comb-module*) :comb1 *auto-multivoice-comb-module*))

;; input should have complete rests/notes for each voice
;; combines rests into single rests, notes into chords if all attributes are =
;; relies on higher voices being sorted earlier
(defun comb-notes-sim/bydist (meas)
  (declare (type meas meas))
  (multiple-value-bind (no re) (split-list (meas-events meas) #'notep)
    (when *auto-multivoice-rests*
      (mapc (lambda (x)
	      (declare (type cons x))
	      (mapc (lambda (y) (declare (type restex y)) (setf (event-inv y) t)) ; leave top-most equivalent rest
		    (rest (sort (delete-if #'event-inv x) #'< :key #'event-voice*)))) ; distr-rest function should have left at least one visible voice
	    (split-into-groups re
			       (lambda (x)
				 (declare (type restex x))
				 (list (event-staff x) (event-off x) (event-dur* x) (event-tupfrac x) (sort-marks (important-marks (event-marks x)))))
			       :test 'equal)))
    (if *auto-multivoice-notes*
	(setf (meas-events meas)
	      (sort (nconc re
			   (flet ((sp (no)
				    (declare (type list no))
				    (split-into-groups no (lambda (x)
							    (declare (type noteex x))
							    (list (event-staff x) (event-off x) (event-dur* x) (event-grace x) (event-tupfrac x)
								  (delete-if (lambda (x)
									       (declare (type (or symbol cons) x))
									       (find (if (listp x) (first x) x) +marks-indiv-voices+))
									     (sort-marks (important-marks (event-marks x))))
								  (event-beamlt x) (event-beamrt x)))
						       :test 'equal)))
			     (mapcan (lambda (x0) ; sequence of adjacent notes to assemble into chords
				       (declare (type (cons (cons (rational 0) (rational 0)) cons) x0))
				       (let ((x (cdr x0)))
					 (if (let ((o1 (caar x0)) (o2 (cdar x0))) ; if no good for chord
					       (or (< (- o2 o1) *min-multivoice-chords-dur*)
						   (<= (length (delete-duplicates (mapcar #'event-voice* x))) 1) ; only one voice
						   (some (lambda (x) (declare (type noteex x)) (or-list (force-list (event-tielt x))))
							 (remove-if-not (lambda (y) (declare (type noteex y)) (<= (event-off y) o1)) x))
						   (some (lambda (x) (declare (type noteex x)) (or-list (force-list (event-tiert x))))
							 (remove-if-not (lambda (y) (declare (type noteex y)) (>= (event-endoff y) o2)) x))))
					     x (loop for e of-type cons in (sp x)
						     when (list1p e) collect e
						     else nconc (let* ((vs (sort (delete-duplicates (mapcar #'event-voice* e)) #'<))
								       (ee (make-chord e (first vs))))
								  (cons ee
									(loop for v of-type (integer 1) in (rest vs) collect
									      (make-restex nil :voice v :off (event-off ee) :dur (event-dur ee) :tup (event-tup ee) :inv t))))))))
				     (merge-all (mapcar (lambda (x)
							  (declare (type cons x))
							  (cons (cons (mloop for e of-type noteex in x minimize (event-off e)) ; ((min-off . max-off) . eq-note-group)
								      (mloop for e of-type noteex in x maximize (event-endoff e)))
								x))
							(sp no))
						(lambda (x0 y0)
						  (declare (type (cons (cons (rational 0) (rational 0)) cons) x0 y0))
						  (let ((x (cdr x0)) (y (cdr y0)))
						    (when (and (= (cdar x0) (caar y0)) ; adjacent
							       (= (event-staff (first x)) (event-staff (first y))) ; same staff
							       (equal (sort (delete-duplicates (mapcar #'event-voice* x)) #'<)
								      (sort (delete-duplicates (mapcar #'event-voice* y)) #'<))) ; all same voices
						      (cons (cons (caar x0) (cdar y0)) (nconc x y)))))))))
		    #'sort-offdur))))
  (print-dot))

(defun comb-notes (parts)
  (declare (type list parts))
  (loop
   for p of-type partex in parts
   unless (or (is-percussion p) (> (instr-staves (part-instr p)) 1) (and (instr-simultlim (part-instr p)) (> (instr-simultlim (part-instr p)) 1)))
   do (loop
       for m of-type meas in (part-meas p) do
       (case (auto-multivoice-comb-fun)
	 (:comb1 (comb-notes-sim/bydist m))
	 (otherwise (error "Unknown multiple voice combination module ~S" *auto-multivoice-comb-module*))))))
