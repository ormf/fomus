;; -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**************************************************************************************************
;; FOMUS
;; postproc.lisp
;;**************************************************************************************************

(in-package :fomus)
(compile-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; POST PROCESSING

(defun postproc-clefs (pts)
  (declare (type list pts))
  (loop
   for p of-type partex in pts do
   (loop
    for m of-type meas in (part-meas p)		; (rest (part-meas p))
    do (loop for g of-type list in (meas-voices m)
	     when g do
	     (let ((ma (popmark (first g) :clef)))
	       (when ma
		 (destructuring-bind (xxx cl &optional (st 1)) ma
		   (declare (ignore xxx) (type symbol cl) (type (integer 1) st))
		   (addprop m (list :clef cl st))
		   (addmark (the (or noteex restex) (first g)) (list :clef cl :meas))))))) ; the :meas means it's redundant at beg. of measure
   (loop
    for (xxx cl st) of-type (t symbol (or (integer 1) null)) in (getprops p :clef) do
    (loop for m in (part-meas p)
	  for e = (find st (sort (loop for g in (meas-voices m) nconc (copy-list g)) #'sort-offdur) :key #'event-staff)
	  when e do
	  (addmark e (list :clef cl st :part)) ; the :part means it's redundant at beg. of part
	  (return))
    (addmark (first (part-meas p)) (list :clef cl st :part)))
   (print-dot)))

(defun postproc-timesigs (pts)
  (declare (type list pts))
  (loop
   for p of-type partex in pts 
   do (loop
       with n of-type (or (integer 1) null) and d of-type (or (integer 1) null) ; indicate new timesigs (changes)
       and k of-type list
       for m of-type meas in (part-meas p)
       for ts = (meas-timesig m)
       for n0 = (timesig-num ts) and d0 = (timesig-den ts)
       and k0 = (rest (getprop ts :keysig))
       unless (and (eql n n0) (eql d d0)) do (addprop m :startsig) (setf n n0 d d0)
       unless (or (equal k k0) (is-percussion p)) do (addprop m :startkey) (setf k k0))
   (print-dot)))

;; input rat (fraction), writunit (for that rat)
;; returns ratio to display: (cons num1 num2)
(defun tupratio (rat writunit events ts)
  (declare (type (rational (0)) rat writunit) (type cons events) (type timesig-repl ts))
  (let ((m (loop with x of-type rational = (/ writunit (mloop for e of-type (or noteex restex) in events maximize (event-writtendur e ts)))
		 for i = 1 then (* i 2) when (>= i x) do (return i))))
    (cons (* (numerator rat) m) (* (denominator rat) m))))

;; sets event-tup to list of durmults (don't need fractions after this)--backends can use event-tup or event-tupdurmult function
(defun postproc-tuplets (pts)
  (declare (type list pts))
  (loop
   for p of-type partex in pts ; get tuplet indications--puts in :starttup and :endtup marks
   do (loop
       for m of-type meas in (part-meas p)
       do (loop
	   for g of-type list in (meas-voices m)
	   for (gg ee) of-type (list list) = (multiple-value-list (split-list g #'event-grace)) do
	   (loop with to = 0 
		 for (pe ne) of-type ((or noteex restex) (or noteex restex null)) on ee	; remove beam between adjacent tuplets
		 while ne
		 when (event-tupfrac pe) do (incf to (apply #'* (event-tupfrac pe)))
		 when (and (notep pe) (notep ne) (= to 1)
			   (or (= (event-beamlt pe) (event-beamrt pe)) (= (event-beamlt ne) (event-beamrt ne))))
		 do (let ((m (max (1- (min (event-beamrt pe) (event-beamlt ne))) 0)))
;; 		      (when (< (event-beamlt pe) (event-beamrt pe)) (setf (event-beamlt pe) (event-beamrt pe)))
;; 		      (when (< (event-beamrt ne) (event-beamlt ne)) (setf (event-beamrt ne) (event-beamlt ne)))
		      (setf (event-beamrt pe) m (event-beamlt ne) m))
		 when (>= to 1) do (setf to 0))
	   (loop
	    with l = (length *max-tuplet*)
	    with lvl = -1
	    and tp = (make-array l :element-type '(rational 0 1) :initial-element 0)
	    and uu = (make-array l :element-type '(or (rational (0)) null) :initial-element nil)
	    and ll = (make-array l :element-type 'list :initial-element nil)
	    for e of-type (or noteex restex) in ee do
	    (loop 
	     with td = (reverse (event-tupdurmult e))
	     and i = -1
	     for f of-type (rational (0)) in (reverse (event-tupfrac e)) ; larger to smaller
	     and u of-type (rational (0)) in td ; durmults
	     do (incf i)
	     when (> i lvl) do (setf (svref uu i) u (svref ll i) nil) ; start new count
	     when (>= i lvl) do (incf (svref tp i) f)
	     do (push e (svref ll i))
	     finally
	     (loop
	      for j downfrom i
	      while (and (>= j 0) (>= (svref tp j) 1))
	      do
	      (setf (svref tp j) 0)
	      (let* ((el (reverse (svref ll j))) ; events in order
		     (ef (first el)))
		(declare (type (or noteex restex) ef))
		(addmark ef 
			 (let ((w (unitwritdur (- (event-endoff e) (event-off ef)) (nthcdr (- i j) (event-tupdurmult e)) #|(- i j)|# (meas-timesig m))))
			   (multiple-value-bind (wr wd) (writtendur* w)
			     (list :starttup (1+ j)
				   (tupratio (svref uu j) w el (meas-timesig m)) ; tupratio as cons
				   (or	; bracket?
				    (< j (mloop for e of-type (or noteex restex) in el maximize (length (event-tupdurmult e)))) ; not innermost--use bracket
				    (loop ; innermost
				     for (x1 x2 x3) of-type ((or noteex restex null) (or noteex restex null) (or noteex restex null))
				     on (cons nil el) while x2
				     when (or (if x1
						  (or (restp x2) (= (event-beamlt x2) 0))
						  (and (notep x2) (> (event-beamlt x2) 0)))
					      (if x3
						  (or (restp x2) (= (event-beamrt x2) 0))
						  (and (notep x2) (> (event-beamrt x2) 0))))
				     do (return t)))
				   (cons wr wd))))) ; i is tup index, next value is bracket t/nil, next cons is written value of tuplet-unit-dur
		(addmark e (list :endtup (1+ j)))) ; end
	      finally 
	      (setf lvl j))))
	   (loop for e of-type (or noteex restex) in gg do (setf (event-tup e) nil))
	   (loop for e of-type (or noteex restex) in ee do (setf (event-tup e) (reverse (event-tupdurmult e))))) (print-dot))))

(defun postproc-graces (pts)
  (declare (type list pts))
  (loop
   for p of-type partex in pts	   ; insert start/end grace note marks for convenience
   do (loop
       for m of-type meas in (part-meas p)
       do (loop
	   for g of-type list in (meas-voices m) do
	   (loop
	    for (e n) of-type ((or noteex restex null) (or noteex restex null)) on (cons nil g) do
	    (when (and n (or (null e) (not (event-grace e))) (event-grace n)) (addmark n :startgrace))
	    (when (and e (event-grace e) (or (null n) (not (event-grace n)))) (addmark e :endgrace))))) (print-dot)))

(defun postproc-voices (pts)
  (declare (type list pts))
  (loop
   for p of-type partex in pts			 ; group events into event-voice-lists
   for vs of-type list = nil do
   (loop
    for m of-type meas in (part-meas p)
    do (loop for e of-type (or noteex restex) in (meas-events m) do (pushnew (event-voice* e) vs)))
   (setf vs (sort vs #'<))
   (loop
    for m of-type meas in (part-meas p)
    do (setf (meas-events m)
	     (loop
	      for g of-type list in (split-list* (meas-events m) (butlast vs) :key #'event-voice*)
	      for v from 1
	      do (map nil (lambda (x) (declare (type (or noteex restex) x)) (setf (event-voice* x) v)) g)
	      collect (sort g #'sort-offdur))))
   (print-dot)))

(defun postproc-markaccs1 (pts)
  (declare (type list pts))
  (loop for p of-type partex in pts
	for pc = (is-percussion p) do
	(loop for m of-type meas in (part-meas p) do
	      (loop for s of-type cons in (split-into-groups (meas-events m) #'event-staff)
		    do (loop with as = (make-array 128 :element-type '(rational -2 2) :initial-element 0)
			     for e in (sort (delete-if #'restp s) #'sort-offdur) do
			     (loop with n = (if (chordp e) (last-element (event-notes* e)) (event-note* e))
				   for m in +marks-withacc+
				   for (xxx in) of-type (symbol (or integer null)) = (force-list (getmark e m))
				   for wn = (whitetonote (+ (notetowhite (if (chordp e) (last-element (event-writtennotes e)) (event-writtennote e)))
							    (if (find m +marks-withaccdn+) -1 1)))
				   when in do
				   (rmmark e m)
				   (if pc (addmark e (list m in))	; just get rid of the accidental
				       (let ((a (- (+ n in) wn))) 
					 (if (and (or (/= a 0) (/= (svref as wn) 0))
						  (or (/= a 0) *acc-throughout-meas*))
					     (addmark e (list m in a))
					     (addmark e (list m in))))))
			     (loop for n of-type integer in (if (chordp e) (event-writtennotes e) (force-list (event-writtennote e)))
				   and a of-type (integer -2 2) in (if (chordp e) (event-accs e) (force-list (event-acc e)))
				   and aa of-type (rational -1/2 1/2) in (if (chordp e) (event-addaccs e) (force-list (event-addacc e)))
				   do (setf (svref as n) (+ a aa))))))
	(print-dot)))

(defun postproc-markaccs2 (pts)
  (declare (type list pts))
  (loop for p of-type partex in pts do
	(loop for m of-type meas in (part-meas p) do
	      (loop for v of-type list in (meas-voices m) do
		    (loop for e of-type (or noteex restex) in v 
			  for po0 = (popmark e :longtrill)
			  when po0 do
			  (let ((po (force-list po0))
				(a (getmark e :startlongtrill-)))
			    (setf (fourth a) (second po))
			    (nconc a (list (third po)))))))
	(print-dot)))

(defun postproc-staves (pts)
  (declare (type list pts))
  (loop for p of-type partex in pts do
	(loop with sv and sg of-type (or (integer 1) null) and wa1 and wa2 
	      for m of-type meas in (part-meas p) do
	      (loop with fv and mm = (mapcan #'copy-list (meas-voices m))
		    for e of-type (or noteex restex) in (sort (copy-list mm) #'sort-offdur) 
		    for ss = (event-staff e) and oo = (event-off e) and vo = (event-voice* e)
		    for fi = (unless (find vo fv) (push vo fv)) do
		    (let* ((cs (lookup vo sv))
			   (eq (eql cs ss)))
		      (declare (type (or (integer 1) null) cs))
		      (when (or fi (not eq))
			(if eq
			    (addmark e (list :staff :voice ss :meas)) ; the :meas means it's redundant (for ea. separate voice)
			    (progn
			      (setf sv (cons (cons vo ss) (delete vo sv :key #'car))) 
			      (addmark e (list :staff :voice ss))))
			(loop with vv = (if (event-grace e) (list e)
					    (sort (delete-if-not ; simult. notes
						   (lambda (x)
						     (declare (type (or noteex restex) x))
						     (and (<= (event-off x) oo)
							  (>= (event-endoff x) oo)
							  (= (event-staff x) ss)))
						   (copy-list mm))
						  #'sort-offdur))
			      with vp = (sort (delete-duplicates (mapcar #'event-voice* vv)) #'<) and vl = (length vv) ; all current simult. voice#s
			      for a in (mapcar (lambda (v) (declare (type (integer 1) v)) (if (= v vo) e (find v vv :key #'event-voice*))) vp)
			      for ap = (eq a e) ; primary event (e)
			      for ff = (and fi ap) and va = (event-voice* a)
			      do (multiple-value-bind (w1 w2) 
				     (if (= vl 1)
					 (values t t)
					 (let ((pp (1+ (position va vp))))
					   (values pp
						   (if (>= vl 3)
						       (case pp (2 3) (3 2) (otherwise pp))
						       pp))))
				   (let* ((cw (lookup va wa1))
					  (eq (eql cw w1)))
				     (when (and (or ff (not eq)) (or ap (not (getmark a '(:voice :ord1234)))))
				       (when ap (setf eq (let ((x (popmark a '(:voice :ord1234)))) (if x (fourth x) eq))))
				       (if eq
					   (addmark a (list :voice :ord1234 (unless (truep w1) w1) :meas)) ; :meas = redundant (for ea. separate voice)
					   (progn
					     (setf wa1 (cons (cons va w1) (delete va wa1 :key #'car))) 
					     (addmark a (list :voice :ord1234 (unless (truep w1) w1)))))))
				   (let* ((cw (lookup va wa2))
					  (eq (eql cw w2)))
				     (when (and (or ff (not eq)) (or ap (not (getmark a '(:voice :ord1324)))))
				       (when ap (setf eq (let ((x (popmark a '(:voice :ord1324)))) (if x (fourth x) eq))))
				       (if eq
					   (addmark a (list :voice :ord1324 (unless (truep w2) w2) :meas))
					   (progn
					     (setf wa2 (cons (cons va w2) (delete va wa2 :key #'car)))
					     (addmark a (list :voice :ord1324 (unless (truep w2) w2)))))))
				   (setf mm (delete-if (lambda (x) (declare (type (or noteex restex) x))
							       (and (= (event-voice* x) va) (not (eq x a)) (sort-offdur x a)))
						       mm)))))))
	      (loop for g in (meas-voices m) and fi = t then nil do
		    (loop for e of-type (or noteex restex) in g and ff = fi then nil
			  for ss = (event-staff e) do
			  (let ((eq (eql sg ss)))
			    (when (or ff (not eq))
			      (if eq
				  (addmark e (list :staff :global ss :meas)) ; global marks are changes across all measures/voices/events
				  (progn
				    (setf sg ss)
				    (addmark e (list :staff :global ss)))))))))
	(print-dot)))

(defun postproc-measrests (pts)
  (declare (type list pts))
  (loop
   for p of-type partex in pts
   do (loop for m of-type meas in (part-meas p) do
	    (loop for g of-type list in (meas-voices m)
		  when (and (list1p g) (restp (first g)))
		  do (addmark (first g) :measrest))) (print-dot)))

;; replace start/end with 1 mark, shift ends to left if replsym is '<
(defun postproc-spanners (pts)
  (declare (type list pts))
  (loop 
   for (startsym contsym endsym replsym) of-type (symbol symbol symbol symbol) in (append +marks-spanner-voices+ +marks-spanner-staves+) ;; fix any notes with starts/ends on same note
   unless (truep replsym)
   do (loop for p of-type partex in pts
	    do (loop for v from 0 below (mloop for x of-type meas in (part-meas p) maximize (length (meas-voices x))) do
		     (loop with h = (make-hash-table)
			   for (pr e) of-type ((or noteex restex) (or noteex restex null))
			   on (cons nil (loop for x of-type meas in (part-meas p) append (nth v (meas-voices x))))
			   when e do 
			   (loop
			    for ma of-type cons in (mapcar #'force-list (getmarks e endsym))
			    for lv = (second ma) do
			    (if (gethash lv h)
				(if (eq replsym '<)
				    (when (or (getmark e (list :spanner< contsym)) (getmark e (if lv (list startsym lv) startsym)))
				      (rmmark e (if lv (list endsym lv) endsym))
				      (let ((s (if lv (list startsym lv) startsym)))
					(if (getmark pr s) (rmmark pr s)
					    (addmark pr ma))))
				    (remhash lv h))
				(progn (rmmark e (if lv (list startsym lv) startsym))
				       (rmmark e (if lv (list endsym lv) endsym))
				       (when replsym (addmark e (nconc (list replsym lv) (cddr ma))))))
			    finally (rmmark e (list :spanner< contsym)))
			   (loop
			    for ma of-type cons in (mapcar #'force-list (getmarks e startsym)) 
			    do (setf (gethash (second ma) h) t))))
	    (print-dot))))

(defun postproc-barlines (pts)
  (declare (type list pts))
  (loop
   for p of-type partex in pts do
   (rmprop (meas-timesig (first (part-meas p))) :barline)
   (loop for (m1 m2) of-type (meas (or meas null)) on (part-meas p) ; transfer properties from timesigs to meas
	 while m2
	 do (loop for bl = (popprop (meas-timesig m2) :barline)
		  while bl
		  do (addprop m1 bl)))
   (let ((m (last-element (part-meas p)))) ; final barline
     (declare (type meas m))
     (unless (getprop m :barline)
       (addprop m (list :barline :final)))) (print-dot)))

(defun postproc-marksonoff (pts)
  (declare (type list pts))
  (loop for (v . (a . b)) of-type (symbol . ((or symbol list) . (or symbol list))) in +marks-onoff+ when (or (null v) (symbol-value v)) do
	(loop for p of-type partex in pts do
	      (loop with oo = (make-list 4) for m of-type meas in (part-meas p) do
		    (loop for g of-type list in (meas-voices m) and o on oo do
			  (loop
			   for e of-type (or noteex restex) in g
			   do (rmmark e b)
			   if (getmark e a) do (if (first o) (rmmark e a) (setf (first o) t))
			   else when (and (first o) (or (null v) (and (notep e) (not (or-list (force-list (event-tielt e))))))) do (addmark e b) (setf (first o) nil))))
	      (print-dot))))
(defun postproc-marksnodup (pts)
  (declare (type list pts))
  (loop for (v . l) of-type (symbol . list) in +marks-nodup+ when (or (null v) (symbol-value v)) do
	(loop for p of-type partex in pts do
	      (loop with lcs = (make-list 4) and lcl = (make-list 4 :initial-element l) for m of-type meas in (part-meas p) do
		    (loop for g of-type list in (meas-voices m)
			  for cs on lcs and cl on lcl do
			  (loop
			   for e of-type (or noteex restex) in g
			   when (first cs) do (rmmark e (first cs))
			   do (let ((x (find-if (lambda (y) (getmark e y)) (first cl))))
				(when x (let ((a (if (listp x) (first x) x)))
					  (setf (first cs) a (first cl) (remove a l))))))))
	      (print-dot))))

;; preproc-tremolos already
;; must be called before preproc-tuplets, actually, should be before any other postprocs
(defun postproc-tremolos (pts)
  (declare (type list pts))
  (loop with fx
	for p of-type partex in pts do
	(loop for m of-type meas in (part-meas p) do
	      (loop with ee 
		    for e of-type (or noteex restex) in (meas-events m) do
		    (let* ((li nil)
			   (ma (or (force-list (popmark e :tremolo))
				   (loop with xf 
					 for x = (popmark e :tremolofirst)
					 while x
					 unless xf do (setf xf x)
					 do (push (third x) li)
					 finally (when xf (rmmark e :tremolosecond) (return xf)))
				   (loop with xf 
					 for x = (popmark e :tremolosecond)
					 while x
					 unless xf do (setf xf x)
					 do (push (third x) li)
					 finally (return xf)))))
		      (if ma (let* ((d (second ma)) ; dur. of unit
				    (w (if d (let ((x (event-writtendur (copy-event e :dur d) (meas-timesig m))))
					       (loop-return-lastmin (diff i x) for i = 1/8 then (/ i 2)))
					   1/32))) ; writ. trem. unit dur.
			       (let ((wd (event-writtendur e (meas-timesig m))))
				 (multiple-value-bind (d o) (floor wd w)
				   (let ((re (if (> o 0)
						 (let ((x (split-event2 e (- (event-endoff e) (* (event-dur* e) (/ o d))))))
						   (let ((bm (min (event-nbeams (car x) (meas-timesig m)) (event-nbeams (cdr x) (meas-timesig m)))))
						     (setf (event-beamrt (car x)) bm (event-beamlt (cdr x)) bm))
						   (push (cdr x) ee)
						   (setf fx t)
						   (car x))
						 e)))
				     (let ((sy (first ma))
					   (dv (min (/ 1/8 w) (1+ (event-nbeams re (meas-timesig m)))))) ; number of divisions, written durational value of tremolo marking
				       (declare (type symbol sy))
				       (if (or (not (chordp re)) (eq sy :tremolo))
					   (progn (push re ee) (addmark re (list :tremolo (/ d dv) (* w dv))))
					   (loop for n0 of-type rational in (event-notes* re)
						 and nn of-type (cons rational (or (integer -2 2) (cons (integer -2 2) (rational -1/2 1/2)))) in (event-note re)
						 and lt of-type boolean in (event-tielt re)
						 and rt of-type boolean in (event-tiert re)
						 if (if (eq sy :tremolofirst) (find n0 li) (not (find n0 li)))
						 collect nn into n1 and collect lt into lt1
						 else collect nn into n2 and collect rt into rt2
						 finally
						 (if (and n1 n2)
						     (let ((c1 (list>1p n1))
							   (c2 (list>1p n2))
							   (d2 (/ (event-dur* re) 2)))
						       (let ((x (event-tupfrac re)))
							 (when x (setf (car x) (/ (the rational (car x)) 2))))
						       (let ((e1 (copy-event re
									     :note (if c1 n1 (the (cons rational (or (integer -2 2) (cons (integer -2 2) (rational -1/2 1/2))))
											       (first n1)))
									     :tielt (if c1 lt1 (the boolean (first lt1)))
									     :tiert (when c1 (make-list (length lt1)))
									     :beamrt 0))
							     (e2 (copy-event re
									     :off (+ (event-off e) d2)
									     :note (if c2 n2 (the (cons rational (or (integer -2 2) (cons (integer -2 2) (rational -1/2 1/2))))
											       (first n2)))
									     :tielt (when c2 (make-list (length rt2)))
									     :tiert (if c2 rt2 (the boolean (first rt2)))
									     :beamlt 0)))
							 (setf (event-dur* e1) d2 (event-dur* e2) d2)
							 (push e1 ee) (push e2 ee) (setf fx t)
							 (addmark e1 (list :starttremolo (/ d 2) w))
							 (addmark e2 (list :endtremolo (/ d 2) w))))
						     (progn (push re ee) (addmark re (list :tremolo (/ d dv) (* w dv))))))))))))
			  (push e ee)))
		    finally (setf (meas-events m) (sort ee #'sort-offdur))))
	(loop for g of-type cons in (split-into-groups (loop for x of-type meas in (part-meas p) append (meas-events x)) #'event-voice*) do
	      (loop for (a b) of-type ((or noteex restex) (or noteex restex null)) on (sort g #'sort-offdur)
		    when (and b
			      (or (getmark a :tremolo) (getmark a :starttremolo) (getmark a :endtremolo))
			      (or (getmark b :tremolo) (getmark b :starttremolo) (getmark b :endtremolo)))
		    do
		    (setf (event-tiert a) (when (consp (event-tiert a)) (make-list (length (event-tiert a))))
			  (event-tielt b) (when (consp (event-tielt b)) (make-list (length (event-tielt b)))))
		    (when (or (getmark a :starttremolo) (getmark a :endtremolo)
			      (getmark b :starttremolo) (getmark b :endtremolo))
		      (setf (event-beamrt a) 0 (event-beamlt b) 0))))
	(print-dot)
	finally (when fx (clean-ties pts))))

(defun postproc-text (pts)
  (declare (type list pts))
  (loop for p of-type partex in pts
	do (loop for m of-type meas in (part-meas p)
		 do (loop with a = (loop for v of-type list in (meas-voices m)
					 append (remove-if (lambda (x) (declare (type (or noteex restex) x)) (and (restp x) (event-inv x))) v))
			  for v of-type list in (meas-voices m)
			  do (loop for e of-type (or noteex restex) in v do
				   (loop
				    with mks
				    for tx = (or (popmark e :starttext-)  
						 (popmark e :startwedge<) (popmark e :startwedge>) (popmark e :startlongtrill-)
						 (popmark e :text) (popmark e :texttempo) (popmark e :textdyn) (popmark e :textnote))
				    while tx do
				    (loop with o = (event-voice* e)
					  for y of-type (integer 1)
					  in (delete-duplicates
					      (loop for x of-type (or noteex restex) in a
						    when (and (= (event-staff x) (event-staff e))
							      (/= (event-voice* x) o)
							      (> (event-endoff x) (event-off e))
							      (< (event-off x) (event-endoff e)))
						    collect (event-voice* x)))
					  count (< y o) into u ; number of voices above text note
					  count (> y o) into d ; number of voices below text note
					  finally
					  (cond ((= d u)
						 (push (cons (first tx)
							     (nconc
							      (let ((x (find-if #'numberp tx))) (when x (list x)))
							      (list (or (find :up tx) (find :down tx) (find :nopos tx) (find :detached tx)
									(or (lookup (first tx) +marks-defaultdir+)
									    (if (>= (event-staff e) (instr-staves (part-instr p)))
										:up :down)))
								    (let ((x (find-if #'stringp tx))) (when x (remove-newlines x))))))
						       mks))
						((< d u)
						 (push (cons (first tx)
							     (nconc
							      (let ((x (find-if #'numberp tx))) (when x (list x)))
							      (list :down (let ((x (find-if #'stringp tx))) (when x (remove-newlines x))))))
						       mks))
						((> d u)
						 (push (cons (first tx)
							     (nconc
							      (let ((x (find-if #'numberp tx))) (when x (list x)))
							      (list :up (let ((x (find-if #'stringp tx))) (when x (remove-newlines x))))))
						       mks))))
				    finally (mapc (lambda (m) (declare (type cons m)) (addmark e m)) mks)))))
	(print-dot)))
				   
;; not included with other postprocs here--in fomus-proc function
(defun postpostproc-sortprops (pts)
  (declare (type list pts))
  (loop
   for p of-type partex in pts do
   (loop for m of-type meas in (part-meas p) do
	 (loop for g of-type list in (meas-voices m) do
	       (loop for e of-type (or noteex restex) in g do (setf (event-marks e) (sort-marks (event-marks e)))))
	 (setf (meas-props m) (sort-props (meas-props m))))
   (setf (part-props p) (sort-props (part-props p)))
   (print-dot)))

;; should be before postproc-text and -voice
(defun postproc-midimarks (pts)
  (declare (type list pts))
  (loop for p of-type partex in pts do
	(loop with vs = (mapcar (lambda (x) (sort x (complement #'sort-offdur)))
				(split-into-groups (remove-if-not #'notep (loop for m of-type meas in (part-meas p) append (meas-events m))) #'event-voice*))
	      for ((en . o) . m) in (loop for m of-type meas in (part-meas p) nconc ; en nil/t = beg/end
					  (loop for e of-type (or noteex restex) in (meas-events m)
						nconc (loop for m of-type (cons symbol symbol) in +marks-midistaff+ nconc
							    (mapcar (lambda (x) (cons (if (find (first (force-list x)) +marks-midistaffends+)
											  (cons t (event-endoff e)) (cons nil (event-off e)))
										      x))
								    (getmarks e (car m))))))
	      do (loop for s of-type list in vs
		       do (let ((e (if en
				       (loop-return-argmin (diff o (event-endoff e)) for e of-type noteex in s)
				       (loop-return-argmin (diff o (event-off e)) for e of-type noteex in s))))
			    (if (popmark e m)
				(addmark e (if (consp m) (cons (lookup (car m) +marks-midistaff+) (rest m)) (lookup m +marks-midistaff+)))
				(addmark e m)))))
	(print-dot)))

(defun postproc-userties (pts)
  (declare (type list pts))
  (loop for p of-type partex in pts do
	(loop for v of-type cons in (split-into-groups (remove-if-not #'notep (loop for m of-type meas in (part-meas p) append (meas-events m))) #'event-voice*)
	      do (loop for (e1 e2) of-type (noteex (or null noteex)) on (sort v #'sort-offdur) #-clisp while #-clisp e2
		       for i = #-clisp (getmark e1 '(:tie :after)) #+clisp (if e2 (getmark e1 '(:tie :after)) (loop-finish))
		       when (and i (>= (event-endoff e1) (event-off e2))) do
		       (if (chordp e1) 
			   (setf (nth (position (second i) (event-notes* e1)) (event-tielt e1)) t)
			   (setf (event-tiert e1) t))
		       (if (chordp e2) 
			   (setf (nth (position (second i) (event-notes* e2)) (event-tielt e2)) t)
			   (setf (event-tielt e2) t))))
	(print-dot)))

;; do lots of nice things for the backend functions
(defun postproc (pts)
  (postproc-tremolos pts)
  (postproc-timesigs pts)
  (postproc-markaccs1 pts) 
  (postproc-midimarks pts)
  (postproc-userties pts)
  (postproc-voices pts)	;; voices now separated into lists
  (postproc-spanners pts)
  (postproc-clefs pts)
  (postproc-staves pts)
  (postproc-measrests pts)
  (postproc-tuplets pts)
  (postproc-graces pts)
  (postproc-marksonoff pts)
  (postproc-marksnodup pts)
  (postproc-text pts)
  (postproc-markaccs2 pts)
  (postproc-barlines pts))
