;; -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**************************************************************************************************
;; FOMUS
;; staves.lisp
;;**************************************************************************************************

(in-package :fomus)
(compile-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STAVES/CLEFS

(declaim (type boolean *auto-staff/clef-changes*))
(defparameter *auto-staff/clef-changes* t)

(declaim (type symbol *auto-staff/clefs-plugin* *auto-staff/clefs-module*))
(defparameter *auto-staff/clefs-plugin* nil)
(defparameter *auto-staff/clefs-module* t)
(declaim (inline auto-clefs-fun))
(defun auto-clefs-fun () (if (truep *auto-staff/clefs-module*) :staves/clefs1 *auto-staff/clefs-module*))

(declaim (type (real 0) *clef-force-clef-change-dist*))
(defparameter *clef-force-clef-change-dist* 2) ; 2 can be nil

(declaim (type #-allegro (float 0 1) #+allegro float *clef-change-clef-penalty* *clef-change-staff-penalty* *clef-polyphony-perbeat-penalty* *clef-order-perbeat-penalty*))
(defparameter *clef-change-clef-penalty* (float 1)) ; 1
(defparameter *clef-change-staff-penalty* (float 1/2)) ; 1/4 should probably be less than change-clef-penalty

(defparameter *clef-polyphony-perbeat-penalty* (float 1/3)) ; 1/16 per beat
(defparameter *clef-order-perbeat-penalty* (float 1/8)) ; 1/16

;; rests should be filtered out before this function is called
;; staff is a number
(declaim (type cons *clef-midwhites* *clef-switchthrs* *clef-list*))
(declaim (special *clef-midwhites* *clef-switchthrs* *clef-list*))
(defun clefs-getclef (clef events staff)
  (declare (type symbol clef) (type list events) (type (integer 0) staff))
  (let ((mi (mloop for e of-type noteex in events minimize (event-writtennote e))) ; GUESS WRITTENNOTE
	(ma (mloop for e of-type noteex in events maximize (event-writtennote e))))
    (flet ((cf () ; choose clef--mi and ma assumed to be written notes
	     (let ((m (/ (+ (notetowhite mi) (notetowhite ma)) 2)))
	       (loop-return-argmin (diff (the integer (lookup cl *clef-midwhites*)) m)
				   for cl of-type symbol in *clef-list*))))
      (if clef
	  (if (or (null events)
		  (let ((cd (lookup clef *clef-switchthrs*)))
		    (declare (type cons cd))
		    (let ((u (cdr cd)) (l (car cd)))
		      (declare (type (or integer null) u l))
		      (and (if l (>= mi l) t) (if u (<= ma u) t)))))
	      clef (cf))
	  (if events (cf) (or (the symbol (nth staff *clef-list*)) (the symbol (last-element *clef-list*))))))))

(declaim (type (integer 1) *staff-engine-heap*))
(defparameter *staff-engine-heap* 30)

(defstruct (clefnode (:copier nil) (:predicate clefnodep))
  (sc 0.0 :type #-allegro (float 0) #+allegro float)
  (lo 0 :type (rational 0))
  (lg 0 :type (rational 0))
  (ics #() :type (vector symbol))
  (cs #() :type (vector symbol))
  (lvs #() :type (vector list))
  (ret nil :type list)
  (evs nil :type list)
  (o 0 :type (rational 0))
  (co 0 :type (integer 0))) ; ics is vector of init-clef, cs is vector of clefs, lvs = vector of last voices
(defun clefs-bylegscore (events instr name)
  (let ((nst (instr-staves instr))
	(co 0))
    (declare (type (integer 0) co))
    (labels ((cm (vs al &optional (s 0)) ; all combinations of staff/voice assignmets (list of lists of (staff . voice))
	       (declare (type list vs) (type list al) (type (integer 0) s))
	       (when vs
		 (loop for i of-type (integer 0) in (or al (loop for i0 from s below nst collect i0)) nconc
		       (loop for j of-type list in (or (cm (rest vs) al i) '(nil)) collect
			     (cons (cons i (first vs)) j))))))
      (flet ((scorefun (no) (declare (type clefnode no)) (cons (clefnode-sc no) (clefnode-co no)))
	     (expandfun (no)
	       (declare (type clefnode no))
	       (when (> (clefnode-co no) co) ;; progress
		 (setf co (clefnode-co no))
		 (print-dot))
	       (let ((o (event-off (first (clefnode-evs no)))))
		 (multiple-value-bind (al es rs vs gd) ; es = events at cur. offset, rs = rest of evs, vs = sorted voices, gd = 0 or effgracedur if grace notes
		     (loop
		      with g = (event-grace (first (clefnode-evs no))) and al and pe
		      for rs on (clefnode-evs no) for e of-type noteex = (first rs)
		      while (and (= (event-off e) o) (eql (event-grace e) g) (or (null pe) (equal (event-userstaff e) (event-userstaff pe))))
		      collect e into es
		      do (setf al (event-userstaff e) pe e)
		      collect (event-voice* e) into v
		      when g maximize (event-graceeffdur e) into gd
		      finally (return (values al es rs (sort (delete-duplicates v) #'<) #-clisp gd #+clisp (or gd 0))))
		   (loop 
		    with nco = (if (or (null (clefnode-o no)) (> o (clefnode-o no))) (1+ (clefnode-co no)) (clefnode-co no)) 
		    for vas of-type cons in (cm vs (loop for e of-type (integer 0) in al if (< e nst) collect e else do
							 (error "Invalid staff number ~S in :STAFF mark at offset ~S, part ~S" (1+ e) (coerce o 'float) name))) ; iterate over all possible arrangements, vas is a possible combination of (cons staff voice)
		    collect (loop
			     with sc = (clefnode-sc no) ; score
			     and ics = (copy-seq (clefnode-ics no)) ; initial clefs
			     and cs = (copy-seq (clefnode-cs no)) ; current clefs
			     and lvs = (copy-seq (clefnode-lvs no)) ; last voices
			     for sv of-type cons in (split-into-groups vas #'car) ; sv is list of (cons staff voice) for one staff
			     nconc (let* ((vv (mapcar #'cdr sv)) ; vv = voices for this staff
					  (ve (remove-if-not (lambda (x) (declare (type noteex x)) (find (event-voice* x) vv)) es))) ; ve = events for this staff 
				     (let ((s (car (first sv))) ; s = staff
					   (uc (find-if #'identity ve :key #'event-userclef)))
				       (declare (type (integer 0) s) (type (or note null) uc))
				       (let ((c0 (svref cs s))
					     (c (or uc ; override
						    (clefs-getclef nil ve s)))) ; c = new clef (maybe)
					 (if (svref (clefnode-ics no) s)
					     (when (and (not (eq c c0))	; clef change suggested
							(or uc ; forced override
							    (eq c (clefs-getclef c0 ve s)) ; supported by necessary change
							    (loop with li and oo of-type (rational 0) and gg ; look ahead to be sure
								  for e of-type noteex in rs
								  when (find (event-voice* e) vv) do
								  (if li
								      (if (and (= (event-off e) oo) (eql (event-grace e) gg))
									  (push e li)
									  (if (eq c0 (clefs-getclef nil li s)) ; goes back to original clef
									      (return)
									      (if (or (and *clef-force-clef-change-dist*
											  (> (- oo o) *clef-force-clef-change-dist*)) ; going too far
										      (eq c (clefs-getclef c0 li s))) ; finally supported by necessary change
										  (return t)
										  (setf li (list e)))))	; necessary to change
								      (setf oo (event-off e) gg (event-grace e) li (list e)))
								  finally
								  (return (unless (or (null li) (eq c0 (clefs-getclef nil li s))) ; goes back to original clef
									    (or (and *clef-force-clef-change-dist*
										     (> (- oo o) *clef-force-clef-change-dist*)) ; going too far
										(eq c (clefs-getclef c0 li s))))))))
					       (setf (svref cs s) c ve (cons (let ((x (copy-event (first ve))))
									       (addmark x (list :clef c)) x)
									     (rest ve)))
					       (incf sc *clef-change-clef-penalty*))
					     (setf (svref ics s) c (svref cs s) c)))
				       (mapc (lambda (v)
					       (declare (type (integer 1) v))
					       (let ((p (position v lvs :test #'find)))	; p = last staff this voice was on
						 (when (or (null p) (/= p s))
						   (incf sc *clef-change-staff-penalty*)
						   (when p (setf (svref lvs p) (remove v (svref lvs p))))
						   (push v (svref lvs s)))))
					     vv)
				       (if (> nst 1)
					   (mapcar (lambda (x) (declare (type noteex x)) (copy-event x :voice (cons (1+ s) (event-voice* x)))) ve)
					   (copy-list ve)))) into ret
			     finally
			     (incf sc (* (+ (* (loop for e across lvs sum (max (1- (length e)) 0)) ; avoid excessive polyphony on one staff
					       *clef-polyphony-perbeat-penalty*)
					    (* (loop ; clef order
						with xl = (loop for x across cs when x collect (position x +clefs+ :key #'car)) and xh #-clisp while #-clisp xl
						for e of-type (integer 0) = #-clisp (pop xl) #+clisp (if xl (pop xl) (loop-finish))
						count (or (when xh (< (min-list xh) e)) (when xl (> (or (max-list xl) -1) e))) 
						do (push e xh))
					       *clef-order-perbeat-penalty*))
					 (max (- o (clefnode-lo no)) (clefnode-lg no))))
			     (return (make-clefnode :sc sc :lo o :lg gd :ics ics :cs cs :lvs lvs :ret (nconc ret (clefnode-ret no)) :evs rs :o o :co nco))))))) ; ret is out of order
	     (scoregreaterfun (s1 s2) (declare (type (cons #-(or openmcl allegro) (float 0) #+(or openmcl allegro) float *) s1 s2)) (< (car s1) (car s2)))
	     (remscoregreaterfun (r1 r2)
	       (declare (type (cons #-(or openmcl allegro) (float 0) #+(or openmcl allegro) float (integer 0)) r1 r2))
	       (if (= (cdr r1) (cdr r2)) (> (car r1) (car r2)) (< (cdr r1) (cdr r2))))
	     (solutfun (no) (declare (type clefnode no)) (null (clefnode-evs no))))
	(let ((*clef-list* (force-list (instr-clefs instr))))
	  (multiple-value-bind (*clef-midwhites* *clef-switchthrs*) ; so = middle whitenotes, sx = switching thresholds
	      (loop
	       with pp and l = (instr-cleflegls instr) 
	       for (c . p) of-type (symbol . (or integer null)) in +clefs+
	       when (find c *clef-list*)
	       do (setf pp (notetowhite p))
	       and collect (cons c pp) into r
	       and collect (cons c (cons (whitetonote (- pp
							 (* (if (numberp l) l
								(loop
								 with l0 of-type integer
								 for z of-type (or (integer 1) cons) in l
								 for (e a1 a2) of-type ((or (integer 1) symbol) symbol (or (integer 1) null)) in (force-list z)
								 if (numberp e) do (setf l0 e)
								 else if (and (eq e c) (eq a1 :dn)) do (return a2)
								 finally (return l0))) 2)
							 5))
					 (whitetonote (+ pp
							 (* (if (numberp l) l
								(loop
								 with l0 of-type integer
								 for z of-type (or (integer 1) cons) in l
								 for (e a1 a2) of-type ((or (integer 1) symbol) symbol (or (integer 1) null)) in (force-list z)
								 if (numberp e) do (setf l0 e)
								 else if (and (eq e c) (eq a1 :up)) do (return a2)
								 finally (return l0))) 2)
							 5)))) into re
	       finally
	       (setf (cadr (first re)) nil
		     (cddr (last-element re)) nil)
	       (return (values r re)))
	    (let ((n (or (let ((*staff-engine-heap* (max (roundint (* *staff-engine-heap* *quality*)) 1)))
			   (bfs*-engine (list (make-clefnode :ics (make-array nst :initial-element nil)
							   :cs (make-array nst :initial-element nil)
							   :lvs (make-array nst :initial-element nil)
							   :evs events))
				      #'scorefun
				      #'expandfun
				      #'solutfun
				      :heaplim *staff-engine-heap*
				      :scoregreaterfun #'scoregreaterfun
				      :remscoregreaterfun #'remscoregreaterfun))
			 (error "Cannot find staff/clef assignments for part ~S" name))))
	      (values (clefnode-ret n)
		      (loop for i from 0 below nst collect
			    (list :clef (or (svref (clefnode-ics n) i)
					    (clefs-getclef nil nil i))
				  (1+ i)))))))))))

(declaim (inline load-staff/clef-modules))
(defun load-staff/clef-modules ()
  (unless (eq (auto-clefs-fun) :staves/clefs1) (load-fomus-module (auto-clefs-fun))))

(defun clefs (parts)
  (loop
     for p of-type partex in parts
     for ns = (instr-staves (part-instr p))
     do (rmprop p :clef) 
     if (is-percussion p) do
       (loop for i from 1 to ns do (addprop p (list :clef :percussion i)))
       (loop for g of-type cons in (split-into-groups (part-events p) #'event-voice*) do
	    (get-usermarks g :staff :startstaff- :staff- :endstaff-
			   (lambda (e s)
			     (declare (type (or noteex restex) e) (type list s))
			     (when (> ns 1) (setf (event-staff* e) (1- (first s))))) (part-name p)))
       (loop for e of-type (or noteex restex) in (part-events p) do (rmmark e :clef))
     else do ; not percussion..., voices aren't separated yet!--separate voices
       (loop for g of-type cons in (split-into-groups (part-events p) #'event-voice*) do
	    (get-usermarks g :staff :startstaff- :staff- :endstaff-
			   (lambda (e s)
			     (declare (type (or noteex restex) e) (type list s))
			     (if (notep e) (setf (event-userstaff e) (mapcar #'1- (force-list s))))
			     (addmark e (cons :staff s))
			     #|(if (notep e) (setf (event-userstaff e) (mapcar #'1- (force-list s))) (addmark e (print (cons :staff s))))|#)
			   (part-name p)))
       (multiple-value-bind (no re) (split-list (part-events p) #'notep)
	 (get-usermarks no :clef :startclef- :clef- :endclef-
			(lambda (e c)
			  (declare (type (or noteex restex) e) (type list c))
			  (setf (event-userclef e) (first c)))
			(part-name p))
	 (multiple-value-bind (evs prs)
	     (if (eq (auto-clefs-fun) :staves/clefs1)
		 (clefs-bylegscore no (part-instr p) (part-name p))
		 (call-module (auto-clefs-fun) (list "Unknown staff/clefs assignment module ~S" *auto-staff/clefs-module*) no (part-instr p) (part-name p)))
	   (setf (part-events p) (sort (nconc re evs) #'sort-offdur))
	   (mapc (lambda (x) (declare (type cons x)) (addprop p x)) prs)))
       (loop ; temporarily assign rests to staves (will become defaults if distr-rests isn't run)
	  for g of-type cons in (split-into-groups (part-events p) #'event-voice*)
	  do (loop
		with s of-type (or list (integer 1))
		for e of-type (or noteex restex) in (sort g #'sort-offdur)
		if (and (restp e) (null (getmark e :staff))) do (if (listp s) (push e s) (setf (event-staff* e) s))
		else do
		  (let ((v (if (restp e) (second (popmark e :staff)) (event-staff e))))
		    (when v
		      (when (listp s) 
			(mapc (lambda (x) (declare (type restex x)) (setf (event-staff* x) v)) s))
		      (setf s v)))))))

(defun clefs-generic (parts)
  (loop for p of-type partex in parts
	for ns = (instr-staves (part-instr p)) do
	(get-usermarks (part-events p) :staff :startstaff- :staff- :endstaff-
		       (lambda (e s)
			 (declare (type (or noteex restex) e) (type list s))
			 (when (> ns 1) (setf (event-staff* e) (first s))))
		       (part-name p))
	(loop for ee of-type cons in (split-into-groups (part-events p) #'event-staff) do
	      (get-usermarks (part-events p) :clef :startclef- :clef- :endclef-
			     (lambda (e c)
			       (declare (type (or noteex restex) e) (type list c))
			       (setf (event-userclef e) (first c))) (part-name p))
	      (loop with s
		    for e of-type (or noteex restex) in (sort ee #'sort-offdur)
		    unless (eq s (event-userclef e)) do (addmark e :clef) (setf s (event-userclef e))))))

(defun clean-clefs (pts)
  (loop for p of-type partex in pts
	do (loop for s from 1 to (instr-staves (part-instr p)) do
		 (loop with x 
		       for m of-type meas in (reverse (part-meas p))
		       do (loop
			   for p = e
			   for e of-type (or noteex restex)
			   in (reverse (remove-if-not (lambda (x) (declare (type (or noteex restex) x)) (= (event-staff x) s)) (meas-events m))) 
			   when (or (restp e) (and p (< (event-endoff e) (event-off p)))) do (setf x nil)
			   if (and (notep e) (or-list (force-list (event-tielt e))))
			   do (let ((x0 (popmark e :clef)))
				(when (and x0 (null x)) (setf x x0)))
			   else if x do (rmmark e :clef) (addmark e x) (setf x nil)))
		 (loop with x 
		       for m in (part-meas p)
		       do (loop for e of-type (or noteex restex)
				in (remove-if-not (lambda (x) (declare (type (or noteex restex) x)) (= (event-staff x) s)) (meas-events m))
				for m = (getmark e :clef)
				when m do (if (eq (second m) x) (rmmark e :clef) (setf x (second m)))))) (print-dot)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DISTRIBUTE RESTS

(declaim (type (member nil t :some :all) *grandstaff-hide-rests*)
	 (type (real (0)) *min-grandstaff-hide-rests-dur*))
(defparameter *grandstaff-hide-rests* t) ; nil (t or :some) or :all, can override in part properties?
(defparameter *min-grandstaff-hide-rests-dur* 1)

(declaim (type symbol *auto-distr-rests-plugin* *auto-distr-rests-module*))
(defparameter *auto-distr-rests-plugin* nil)
(defparameter *auto-distr-rests-module* t)
(declaim (inline auto-distr-rests-fun))
(defun auto-distr-rests-fun () (if (truep *auto-distr-rests-module*) :rests1 *auto-distr-rests-module*))

;; call AFTER ass-voices but BEFORE anything else (need to know where the rests are)
;; assigns all rests to staves and hides some rests
;; if other voice in OTHER staff crosses to staff w/ rests & includes notes AND flag set & >= min dur AND region includes beg/end of measure, make rests invisible (do in distr-rests)
;; (if rests hidden at beg. of measure, don't hide rest of measure)
(defun distr-rests-byconfl (parts)
  (declare (type list parts))
  (loop
   with rl of-type list ; (cons (cons (rational 0) (rational 0)) list)
   and lo = (meas-endoff (last-element (part-meas (first parts)))) ; list of lists of rests to turn invisible
   for p of-type partex in (remove-if #'is-percussion parts)
   for sv = (> (instr-staves (part-instr p)) 1) do
   (loop
    for v in (loop with v for m of-type meas in (part-meas p) do
		   (loop for e of-type (or noteex restex) in (meas-events m) do (pushnew (event-voice* e) v))
		   finally (return v)) do
    (loop
     with s and r and r0 and r1	; r is rests up to endoff falling even w/ beat, r0 = remaining, r1 = to turn invisible
     and ae0 = (remove-if-not #'notep (loop for m of-type meas in (part-meas p) append (meas-events m))) and ae
     for m of-type meas in (part-meas p) ; all one voice
     and cm = t and ba = nil do	; cm = intersect or align w/ beg/end of measure, ba = already made some rests invisible this measure
     (loop 
      with mo = (let ((x (if (meas-div m)
			     (nconc (loop with o = (meas-off m) with d = (- (meas-endoff m) o)
					  for e of-type (rational (0)) in (meas-div m) collect o do (incf o (* e d))) (list (meas-endoff m)))
			     (list (meas-off m) (meas-endoff m)))))
		  (loop for (e n) of-type ((rational 0) (or (rational 0) null)) on x
			if n nconc (loop for i from e below n collect i)
			else collect e))
      and tu = 0			; tu = tuplet
      for e in (remove-if-not (lambda (x) (declare (type (or noteex restex) x)) (= (event-voice* x) v)) (meas-events m)) and lt = tu ; lt = last tuplet
      do (incf tu (apply #'* (event-tupfrac e)))
      if (restp e) do 
      (if (and (null r) (null r0) s (not (and (integerp lt) (find (event-off e) mo))))
	  (setf (event-voice e) (if sv (cons s v) v)) ; set it and leave it, can't start group yet
	  (if (and (integerp tu) (find (event-endoff e) mo)) ; at end of group--save it
	      (progn (unless ba (prepend r0 r1) (push e r1)) ; r1 to possibly turn invisible
		     (prenconc r0 r) (push e r) (setf r0 nil)) ; r to put on a staff, r0 is remaining ones
	      (push e r0)))
      else if (notep e) do 
      (unless s (setf s (event-staff e))) 
      (let ((ss (event-staff e))) ; decide which staff to stick the rests on
	(if r				; have remaining ones
	    (let ((o1 (event-off (last-element r))) ; o1, o2 offset bounds of r
		  (o2 (event-endoff (first r))))
	      (setf ae (nconc (delete-if (lambda (x) (declare (type noteex x)) (<= (event-endoff x) o1)) ae) ; ae is all notes in span o1 to o2 on all staves (one voice)
			      (loop for x0 on ae0 for x of-type noteex = (first x0) while (< (event-off x) o2) when (> (event-endoff x) o1) collect x finally (setf ae0 x0))))
	      #+debug (check-order ae "DISTR-RESTS-BYCONFL" #'sort-offdur)
	      (flet ((td (s)		; total duration of notes
		       (declare (type (integer 1) s))
		       (multiple-value-bind (gr no) (split-list (remove-if-not (lambda (x) (declare (type noteex x)) (= (event-staff x) s)) ae) #'event-grace)
			 (max (mloop for e of-type noteex in gr maximize (event-graceeffdur e))
			      (loop
			       for (a . b) of-type ((rational 0) . (rational 0))
			       in (merge-linear (mapcar (lambda (x) (declare (type noteex x)) (cons (max (event-off x) o1) (min (event-endoff x) o2))) no)
						(lambda (x y) (declare (type (cons (rational 0) (rational 0)) x y)) (when (<= (car y) (cdr x)) (cons (car x) (cdr y)))))
			       sum (- b a)))))
		     (iv (ts)		; make rests in r1 invisible
		       (when (and (or (eq *grandstaff-hide-rests* :all) (and *grandstaff-hide-rests* (> ts 0)))	; ts > 0 means some conflict w/ notes
				  cm (not ba) r1 (or (null *min-grandstaff-hide-rests-dur*) (>= (- o2 o1) *min-grandstaff-hide-rests-dur*))) ; 4/1/06 r1
			 (mapc (lambda (x) (declare (type restex x)) (setf (event-inv x) t)) r1)
			 (push (cons (cons (event-off (last-element r1)) o2) r1) rl) ; rl = list of ((o1 . o2) . inv-rests)
			 (setf ba t))))	; already made rests inv this measure
		(let ((ts1 nil) (ts2 nil))
		  (if (and (/= s ss) (> (setf ts1 (td s)) (setf ts2 (td ss)))) ; amount of conflict with staves of first/last note--s is staff of last note before e
		      (progn (setf s ss)
			     (mapc (lambda (x) (declare (type restex r)) (setf (event-voice x) (if sv (cons s v) v))) r)
			     (iv ts2))	; go with staff of last note
		      (progn (mapc (lambda (x) (declare (type restex r)) (setf (event-voice x) (if sv (cons s v) v))) r)
			     (setf s ss)
			     (iv (or ts1 (td s))))))))
	    (setf s ss)))		; go with staff of first note
      (mapc (lambda (x) (declare (type restex r)) (setf (event-voice x) (if sv (cons s v) v))) r0) ; set rest of the rests
      (setf r nil r0 nil r1 nil cm nil)) (print-dot)
     finally
     (mapc (lambda (x) (declare (type restex r)) (setf (event-voice x) (if sv (cons (or s 1) v) v))) (nconc r0 r))))
   (loop
    with ee 
    do (loop				; find the holes and plug them
	for e in (setf ee (get-holes (merge-linear (mapcan (lambda (m)
							     (declare (type meas m))
							     (loop for y of-type (or noteex restex) in (meas-events m)
								   unless (and (restp y) (event-inv y))
								   collect (cons (event-off y) (event-endoff y))))
							   (part-meas p))
						   (lambda (x y) (declare (type (cons (rational 0) (rational 0)) x y)) (when (<= (car y) (cdr x)) (cons (car x) (cdr y)))))
				     0 lo))
	for m = (loop-return-argmin
		 (event-voice* (first (cdr i))) ; first voice
		 for i of-type (cons (cons (rational 0) (rational 0)) *)
		 in (loop-return-argmins
		     (cdar i) 
		     for i of-type (cons (cons (rational 0) (rational 0)) *)
		     in (loop-return-argmins
			 (caar i)	; earliest one
			 for i of-type (cons (cons (rational 0) (rational 0)) *)
			 in (loop-return-argmins
			     (- (cdar i) (caar i)) ; smallest one
			     for i of-type (cons (cons (rational 0) (rational 0)) *)
			     in (let ((o1 (car e)) (o2 (cdr e)))
				  (declare (type (rational 0) o1 o2))
				  (or (remove-if-not (lambda (x)
						       (declare (type (cons (cons (rational 0) (rational 0)) *) x))
						       (and (<= (caar x) o1) (>= (cdar x) o2))) rl) ; complete fit
				      (remove-if-not (lambda (x)
						       (declare (type (cons (cons (rational 0) (rational 0)) *) x))
						       (and (< (caar x) o2) (> (cdar x) o1))) rl)))))))	; partial fit
	#+debug unless #+debug m #+debug do #+debug (error "Error in DISTR-RESTS-BYCONFL")
	do
	(mapc (lambda (x) (declare (type restex x)) (setf (event-inv x) nil)) (cdr m)) ; turn back to visible
	(setf rl (delete m rl :test #'equal))
	(print-dot))
    while ee)))

(defun distr-rests (parts)
  (case (auto-distr-rests-fun)
    (:rests1 (distr-rests-byconfl parts))
    (otherwise (error "Unknown rest to staves distribution module ~S" *auto-distr-rests-module*))))

