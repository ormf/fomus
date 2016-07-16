;; -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**************************************************************************************************
;; FOMUS
;; marks.lisp
;;**************************************************************************************************

(in-package :fomus)
(compile-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-mark-accs (pts)
  (declare (type list pts))
  (loop for p of-type partex in pts do
	(loop for e of-type (or noteex restex) in (part-events p) do
	      (loop for k of-type symbol in +marks-withacc+
		    for (m o) = (force-list (getmark e k))
		    when o do
		    (let* ((n (note-to-num o))
			   (d (- n (event-note* e))))
		      (if (if (find m +marks-withaccdn+) (and (>= d -2) (<= d 0)) (and (>= d 0) (<= d 2)))
			  (progn (rmmark e k) (addmark e (list m d)))
			  (error "~S mark is greater than a whole step at offset ~S, part ~S" m (event-foff e) (part-name p))))))
	(print-dot)))

(defparameter *auto-grace-slurs* t)

(defun grace-slurs (pts)
  (declare (type list pts))
  (loop
   for p of-type partex in pts do
   (loop
    for e of-type cons in (delete-if (lambda (x) (declare (type cons x)) (notany #'event-grace x)) (split-into-groups (part-events p) #'event-off))
    for s = (sort e (complement #'sort-offdur))
    do (loop with sl and li 
	     for x of-type (or noteex restex) in s
	     when (or (getmark x :endgraceslur-) (getmark x :graceslur-)) do
	     (when sl (error "Missing :STARTGRACESLUR- mark in offset offset ~S, part ~S" (event-foff x) (part-name p)))
	     (setf sl t)
	     (when li (addmark (first li) :startgraceslur-) (addmark (last-element li) :endgraceslur-) (setf li nil))
	     unless sl do (push x li)
	     when (getmark x :startgraceslur-) do
	     (if sl (setf sl nil) (error "Missing :GRACESLUR-/:ENDGRACESLUR- slur mark in offset ~S, part ~S" (event-foff x) (part-name p)))
	     finally
	     (when li (addmark (first li) :startgraceslur-) (addmark (last-element li) :endgraceslur-))))
   (print-dot)))

;; ***** UPDATED COMMENTS *****
;; sorts through user's potential mess of spanner mark starts, ends, continuations
;; user level numbers only need to be relative to each other, lower levels are "inner" (closer to the notes)
(defun clean-spanners (pts spanners)
  (declare (type list pts) (type cons spanners))
  (loop for (startsym contsym endsym replsym symorg) of-type (symbol symbol symbol symbol (or symbol (integer 1))) in spanners
	for symlvl = (when (numberp symorg) symorg) ; symlvl = forced level
	do (loop for p of-type partex in pts do
		 (loop			; iterate through note events
		  with ss and sta and staa and mor of-type list
		  for (e nxe) of-type ((or noteex restex) (or noteex restex null)) on (reverse (part-events p)) do ; go backwards, find endsyms
		  (setf mor nil)
		  (loop 
		   for (xxx a1) of-type (t (or (integer 1) null)) ; loop through contsyms and endsyms
		   in (sort (nconc (when contsym (loop for x = (popmark e contsym) while x collect (force-list x))) ; a1 = user-specified level
				   (loop for x = (popmark e endsym) while x collect (force-list x)))
			    #'< :key (lambda (x) (or (second x) 1))) ; sort by level (default is 1)
		   do (let ((lv (or symlvl a1 1))) ; lv = actual level
			(if (find lv ss) ; ss = list of current spans and their levels
			    (push lv mor) ; problem, two endsyms, need a matching startsym--save reinserting endsym for later (after dealing w/ startsyms)
			    (progn (push lv ss)
				   (addmark e (list endsym lv)))))) ; reinsert proper endsym & level
		  (loop			; find startsyms
		   for rr0 of-type cons
		   in (sort (loop for x = (popmark e startsym) while x collect (force-list x) into sp finally
				  (when (and sp (eq replsym '<)) (addmark e (list :spanner< contsym))) ; :spanner< is message for postproc functions
				  (return sp))
			    #'< :key (lambda (x) ; sort by level, have to find it since startsym is more complicated
				       (declare (type cons x))
				       (destructuring-bind (xxx &optional a1 a2) x
					 (declare (ignore xxx))
					 (if (numberp a2) a2 (if (numberp a1) a1 1)))))	; level is the only number present
		   do (multiple-value-bind (a1 a2 a3) ; find and sort out mark arguments
			  (let ((rr (rest rr0)))
			    (values (find-if #'numberp rr) (find-if #'symbolp rr) (find-if #'stringp rr))) ; a1 = level, a2 = modifier (ex. :dotted), a3 = string
			(let ((lv (or symlvl a1 1))) ; in = inserted startsym?, lv = actual level
			  (if (find lv ss) ; startsym matches an endsym
			      (progn (setf ss (delete lv ss)) 
				     (addmark e (nconc (list startsym lv) (when a3 (list a3)) (when a2 (list a2))))) ; reinsert mark (startsym level string modifier)
			      (progn	; problem, no matching endsym
				(let ((lk (or (lookup lv sta) staa))) ; sta = stack for backtrace (forward) through events (lookup by level), staa = events up to the end
				  (when lk ; lk = list of events to backtrack through & add contsyms
				    (loop for (a b) of-type ((or noteex restex) (or noteex restex null)) on lk
					  if b do (addmark a (list contsym lv)) else do (addmark a (list endsym lv))
					  finally (addmark e (nconc (list startsym lv) (when a3 (list a3)) (when a2 (list a2)))))))))
			  (let ((x (assoc lv sta))) ; reinserted startsym, so reset backtrace stack for this level
			    (if x (setf (cdr x) nil) (push (cons lv nil) sta))))))
		  (loop for lv of-type (integer 1) in mor do ; ok to reinsert rest of endsyms (ones that didn't have a startsym following it)
			(unless (find lv ss) ; reinsert endsym if it's safe (not in the middle of a span)
			  (push lv ss)
			  (addmark e (list endsym lv))))
		  (loop for l of-type (integer 1) in ss	; reinsert contsyms (or startsym if at the beginning)
			if nxe do (unless (getmark e (list endsym l)) (addmark e (list contsym l)))
			else do (addmark e (list startsym l)))
		  (map nil (lambda (x) (push e (cdr x))) sta) ; push this event onto all the backtrace stacks
		  (push e staa)) ; push this event onto the backtrace-to-the-end stack
		 (loop for v of-type cons in (split-into-groups (part-events p) #'event-voice*)	; replace user levels with ones backends understand (1, 2, etc.)--voices might not be separated here, make sure they are
		       do (loop with x = (make-hash-table) and xx ; x = hash-table indexed by level containing (off . list-of-start/cont-marks), used as temporary container
				for e of-type (or noteex restex) in (sort v #'sort-offdur)
				when (or (truep replsym) (keywordp replsym)) do	; if there is a replsym, do start/cont marks first
				(loop for m of-type cons in (getmarks e contsym) do (push m (cdr (or (gethash (second m) x) '(nil . nil)))))
				(loop for m of-type cons in (getmarks e startsym) do (setf (gethash (second m) x) (cons (event-off e) (list m))))
				do (loop for m of-type cons in (getmarks e endsym) do
					 (let* ((n (second m))
						(z (gethash n x)))
					   (when z ; added "when z" 5/24/07
					     (push m (cdr z)) 
					     (push (cons n (cons (cons (car z) (event-off e)) (cdr z))) xx) ; add (level . ((off . endoff) . list-of-start/cont-marks)) to xx
					     (remhash n x))))
				unless (or (truep replsym) (keywordp replsym)) do ; if there isn't a replsym, do start/cont marks last
				(loop for m of-type cons in (getmarks e contsym) do (push m (cdr (gethash (second m) x))))
				(loop for m of-type cons in (getmarks e startsym) do (setf (gethash (second m) x) (cons (event-off e) (list m))))
				finally (loop with pr
					      for ((o1 . o2) . ms) of-type (((rational 0) . (rational 0)) . cons)
					      in (mapcar #'cdr (sort xx #'< :key (if (eq symorg 's) (lambda (k) (- (cdadr k) (caadr k))) #'car)))
					      do (let ((nu (1+ (mloop for ((c1 . c2) . lv) of-type (((rational 0) . (rational 0)) . (integer 1)) in pr
								      when (and (> o2 c1) (< o1 c2)) maximize lv))))
						   (map nil (lambda (z) (setf (second z) nu)) ms) ; ok to replace, marks should all be new
						   (push (cons (cons o1 o2) nu) pr)))))
		 (print-dot))))

;; ***** CONTINUE UPDATING COMMENTS HERE *****

(defun expand-marks (pts)
  (loop for ((ma . del) . (rs . re)) of-type ((symbol . boolean) . (symbol . symbol)) in +marks-expand+ do
	(loop for p of-type partex in pts
	      do (loop for e of-type (or noteex restex) in (part-events p)
		       for po of-type list = (force-list (if del (popmark e ma) (getmark e ma)))
		       when po do (addmark e (if (rest po) (cons rs (rest po)) rs)) (addmark e re))
	      (print-dot))))

;; distribute mark objects to note/rest objects
(defun distribute-marks (pts mks)
  (declare (type list pts mks))
  (loop with pas = (loop for p of-type partex in pts collect
			 (cons (mapcan
				(lambda (x) (declare (type cons x)) (sort x #'sort-offdur))
				(sort (split-into-groups (part-events p) #'event-off)
				      #'< :key (lambda (x) (declare (type cons x)) (event-off (first x)))))
			       (mapcan
				(lambda (x) (declare (type cons x)) (sort x #'sort-offdur))
				(sort (split-into-groups (part-events p) #'event-endoff)
				      #'> :key (lambda (x) (declare (type cons x)) (event-endoff (first x)))))))
	for k = (pop mks) while k do
	(loop with fu = (listp (event-off k)) ; fuzzy offset? (next available note forwards or backwards)
	      with nu = (if fu (first (event-off k)) (event-off k))
	      with o0 = (abs nu) and di = (>= nu 0) ; offset and direction
	      for m in (event-marks k) do
	      (loop with fl = (force-list m)
		    with sy = (first fl)
		    with re = (if (eq sy :mark) (is-restmarksym (third fl)) (is-restmarksym sy)) ; allowed in rest?
		    and sta = (and (listp (event-voice k)) (eq (first (event-voice k)) :staff))	; is staff spec?
		    with vo = (if sta (rest (event-voice k)) (force-list (event-voice k)))
		    for p of-type partex in pts and (fo . ba) of-type (list . list) in pas
		    when (or (null (event-partid k)) (find (part-partid p) (force-list (event-partid k))))
		    do (let ((ev (flet ((rm (l)
					  (declare (type list l))
					  (let ((r (if sta ; if user specified staves
						       (loop for e0 on l for e of-type (or noteex restex) = (first e0)
							     for pr = nil then (delete-if-not (lambda (x)
												(declare (type (or noteex restex) x))
												(and (< (event-off x) (event-endoff e))
												     (< (event-off e) (event-endoff x))))
											      pr) ; only previous intersection events
							     for ne = (loop for x of-type (or noteex restex)
									    in (rest e0) while (and (< (event-off x) (event-endoff e))
												    (< (event-off e) (event-endoff x)))
									    collect x) ; only following intersecting events
							     when (some (lambda (s)
									  (declare (type integer s))
									  (let ((as (abs s))) ; as = staff number
									    (and (= (event-staff e) as)	; collect note if on correct staff and...
										 (let ((j (if (minusp s)
											      (min (mloop for i of-type (or noteex restex)
													  in pr when (= (event-staff i) as) ; topmost voice
													  minimize (event-voice* i))
												   (mloop for i of-type (or noteex restex)
													  in ne when (= (event-staff i) as)
													  minimize (event-voice* i)))
											      (max (mloop for i of-type (or noteex restex)
													  in pr when (= (event-staff i) as) ; bottommost voice
													  maximize (event-voice* i))
												   (mloop for i of-type (or noteex restex)
													  in ne when (= (event-staff i) as)
													  maximize (event-voice* i))))))
										   (cond ((= j 0) (setf (event-textdir e) -1))
											 ((= j (event-voice* e)) (setf (event-textdir e) 1)))))))
									vo) ; choices
							     collect e
							     do (push e pr))
						       (mapc (lambda (x) (declare (type (or noteex restex) x)) (setf (event-textdir x) nil))
							     (if (null vo) l (remove-if-not (lambda (e)
											      (declare (type (or noteex restex) e))
											      (find (event-voice* e) vo))
											    l)))))) ; user specified voices
					    (if re r (remove-if #'restp r)))))
				   (let ((o (let ((q (getprop p :quant))) ; fix quantize error
					      (if q (let ((x (find-if (lambda (x)
									(declare (type (cons (cons (function ((rational 0) (rational 0)) boolean) (real 0))
											     (rational 0))
										       x))
									(and (funcall (caar x) o0 (cdar x)) ; o0 = offset of mark
									     (if (< (cdar x) (cdr x))
										 (< o0 (cdr x))	; -->
										 (> o0 (cdr x))))) ; <--
								      (rest q))))
						      (if x (cdr x) o0))
						  o0))))
				     (if di
					 (if fu
					     (loop for e of-type (or noteex restex) in (rm fo) until (> (event-off e) o) finally (return e))
					     (loop for (e1 e2) of-type ((or noteex restex null) (or noteex restex))
						   on (cons nil (rm fo)) until (or (null e2) (> (event-off e2) o)) 
						   finally (return (or e1 e2))))
					 (if fu
					     (loop for e of-type (or noteex restex) in (rm ba) until (< (event-endoff e) o) finally (return e))
					     (loop for (e1 e2) of-type ((or noteex restex null) (or noteex restex))
						   on (cons nil (rm ba)) until (or (null e2) (< (event-endoff e2) o)) 
						   finally (return (or e1 e2)))))))))
			 (if (eq sy :mark)
			     (push (copy-event k :off (second fl) :voice (event-voice* ev)
					       :marks (list (let ((x (cddr fl))) (if (list>1p x) x (first x)))))
				   mks)
			     (cond
			       ((or (null (event-textdir ev)) (find :up m) (find :down m)) (addmark ev m))
			       ((= (event-textdir ev) -1) (addmark ev (append m '(:up))))
			       (t #+debug (or (= (event-textdir ev) 1) (error "Error in DISTRIBUTE-MARKS")) (addmark ev (append m '(:down)))))))))
	(print-dot)
	finally (loop for p of-type partex in pts do
		      (rmprop p :quant)
		      (loop for e of-type (or noteex restex) in (part-events p) do
			    (setf (event-marks e) (remove-duplicates (event-marks e) :test #'equal))))))

;; duplicate marks w/ before/after slots
(defun marks-beforeafter (pts)
  (declare (type list pts))
  (loop with xx for p of-type partex in pts do
	(loop for m of-type meas in (part-meas p) do
	      (loop for (e0 e1 e2) of-type (noteex (or noteex null) (or noteex null))
		    on (cons nil (remove-if-not #'notep (meas-events m))) while e1 do
		    (loop for (a . d) of-type (symbol . symbol) in +marks-before-after+
			  for k = (force-list (popmark e1 a))
			  when k do
			  (push (cons (ecase (or (second k) d)
					(:before e0)
					(:after e1))
				      (list (first k) :after))
				xx)
			  (push (cons (ecase (or (second k) d)
					(:before e1)
					(:after e2))
				      (list (first k) :before))
				xx))))
	(print-dot)
	finally
	(loop for (e . m) of-type ((or noteex restex) . cons) in xx when e do (addmark e m))))
				
(defun preproc-userties (pts)
  (declare (type list pts))
  (loop for p of-type partex in pts do
	(loop for m of-type meas in (part-meas p) do
	      (loop for e of-type (or noteex restex) in (meas-events m) 
		    for k = (force-list (popmark e :tie))
		    when k do (addmark e (append k (list (event-note* e))))))))
