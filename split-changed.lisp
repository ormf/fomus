;; -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**************************************************************************************************
;; FOMUS
;; split.lisp
;;**************************************************************************************************

(in-package :fomus)
(compile-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENGINE

;; each node is a subset of its parent node's problem
;; all functions are relative to ea. node (solutfun returns t if subsolution is correct, complete solution is correct if all subsolutions are correct)
;; score for each node must be optimistic
;; expandfun returns or-list of and-lists (if returns values, then second value = t if it's an and-combine-only node (with only one choice)), if an and-list is nil, it's considered empty and skipped
;; assemfun reassembles subsolutions
(declaim (type (integer -1) *itdepfirst*-id*))
(declaim (special *itdepfirst*-id*))
(defstruct (itdepfirst*-andnode (:copier nil) (:predicate itdepfirst*-andnode-p))
  sol dat/hp) ; dat/hp is data if either depth is 0 or sol is t
(defstruct (itdepfirst*-ornode (:copier nil) (:predicate itdepfirst*-ornode-p))
  (id (incf *itdepfirst*-id*) :type (integer 0) :read-only t)
  (ands nil :type list)
  assem score
  (depth 0 :type (integer 0)))
 
(defun expandfun (node off endoff lm)                   ; expand (into or-list)
  (declare (type splitnode node))
  ;; (when (<= (- (splitnode-of2 node) (splitnode-of1 node)) lm) (er off endoff))
  (unless (or (basenodivp (splitnode-rl node)) (<= (- (splitnode-of2 node) (splitnode-of1 node)) lm));; if this returns non-nil, we are on a dead end and can't expand any further!
    (let ((rt (loop
                 for ru of-type (cons (or (rational (0) (1)) cons) list)
                 in (loop for e #|of-type baserule|#
                       in (split-rules-bylevel
                           (splitnode-rl node)
                           (let ((du (- (splitnode-of2 node) (splitnode-of1 node))))
                             (and (or (null *min-tuplet-dur*) (>= du *min-tuplet-dur*))
                                  (or (null *max-tuplet-dur*) (<= du *max-tuplet-dur*)))))
                       collect e)     ; ors, ru = new rule
                 for div = (or (when (basesplitp (splitnode-rl node)) (rule-init (splitnode-rl node)))
                               (splitnode-div node))
                 for sp = (append (force-list (first ru)) '(1)) and rr = (rest ru) ; sp =(unless (and tv (eq (first sp) :grandstaff)) (first sp)) split points (last one is a), rr = replacement rules
                 collect (if (splitnode-pts node)
                             (loop	; iterate through parts
                                with al = (make-list (length rr) :initial-element t)
                                for p of-type (or (member t) cons) in (splitnode-evs node)
                                for xx = (if (or (truep p) (split-valid p off endoff (splitnode-rl node))) al (spl p sp rr node))
                                for li = (mapcar #'list xx) then (cons-list xx li)
                                finally (return (loop 
                                                   for e of-type cons in li and r #|of-type baserule|# in rr
                                                   and (o1 o2) of-type ((rational 0 1) (rational 0 1)) on (cons 0 sp) ; ands
                                                   collect (make-splitnode :rl r :pts t :par node :evs (nreverse e) :of1 (of o1 node) :of2 (of o2 node) :div div)))) ; evs might contain t
                             (loop
                                for e of-type cons in (spl (splitnode-evs node) sp rr node) and r #|of-type baserule|# in rr
                                and (o1 o2) of-type ((rational 0 1) (rational 0 1)) on (cons 0 sp) ; ands
                                collect (make-splitnode :rl r :pts nil :par node :evs e :of1 (of o1 node) :of2 (of o2 node) :div div))))))
      (if (and (splitnode-pts node) (not (split-allparts (splitnode-rl node))))
          (cons (mapcar (lambda (p)
                          (make-splitnode :rl (splitnode-rl node) :pts nil :par node
                                          :evs (if (or (truep p) (split-valid p off endoff (splitnode-rl node))) t p)
                                          :of1 (splitnode-of1 node) :of2 (splitnode-of2 node) :div (splitnode-div node)))
                        (splitnode-evs node))
                rt)
          rt))))

;; proc seems to be the optimizing (recursive) process to find the
;; best splitpoints.

(defun proc (node depth scoregreaterfun expandfun assemfun scorefun solutfun off endoff lm) ; node is and-node--shouldn't be called if it's a solution
	       (when (<= depth 0) ; if first time, replace data with heap of ors
		 (setf (itdepfirst*-andnode-dat/hp node)
		       (make-heap (lambda (x y)
				    (or (funcall scoregreaterfun (itdepfirst*-ornode-score x) (itdepfirst*-ornode-score y))
					(unless (funcall scoregreaterfun (itdepfirst*-ornode-score y) (itdepfirst*-ornode-score x))
					  (< (itdepfirst*-ornode-id x) (itdepfirst*-ornode-id y)))))
				  :initial-contents (loop
                                                       for e in (funcall expandfun (itdepfirst*-andnode-dat/hp node) off endoff lm) ; e = and-list, heap temporarily contains data to expand
						     when e ; at least something in and-list
						     collect (let ((a (funcall assemfun e)))
							       (make-itdepfirst*-ornode
								:ands (mapcar (lambda (i) (make-itdepfirst*-andnode :dat/hp i :sol (funcall solutfun i off endoff))) e)
								:assem a :score (funcall scorefun a)))))))
	       (loop
		for n = (or (heap-peek (itdepfirst*-andnode-dat/hp node)) (return)) ; n is or-node
		for so = (every #'itdepfirst*-andnode-sol (itdepfirst*-ornode-ands n)) ; so = or-node is complete (all and-nodes inside it are complete)
		until (or so (>= (itdepfirst*-ornode-depth n) depth))
		do
		(heap-rem (itdepfirst*-andnode-dat/hp node))
		(loop
		 for e in (itdepfirst*-ornode-ands n) ; e is and-node
		 if (itdepfirst*-andnode-sol e) collect (itdepfirst*-andnode-dat/hp e) into d
                   else collect (or (proc e (itdepfirst*-ornode-depth n) scoregreaterfun expandfun assemfun scorefun solutfun off endoff lm) (return))
                   into d ; if dead-end, don't put back into heap
		 finally
		 (incf (itdepfirst*-ornode-depth n))
		 (let ((a (funcall assemfun d)))
		   (setf (itdepfirst*-ornode-assem n) a
			 (itdepfirst*-ornode-score n) (funcall scorefun a)))
		 (heap-ins n (itdepfirst*-andnode-dat/hp node)))
		finally
		(when so (setf (itdepfirst*-andnode-sol node) t ; this node is a solution--save it
			       (itdepfirst*-andnode-dat/hp node) (itdepfirst*-ornode-assem n)))
                    (return (itdepfirst*-ornode-assem n))))



;;; central splitting engine, called once from split-engine-byscore:

(defun itdepfirst*-engine (init-node scoregreaterfun scorefun expandfun assemfun solutfun off endoff lm)
;;  (declare (type (function (t) list) scorefun assemfun solutfun scoregreaterfun expandfun))
  (let* ((*itdepfirst*-id* -1))
    ; return assembled data or nil if dead-end
    (loop
       with tn = (make-itdepfirst*-andnode :dat/hp init-node :sol (funcall solutfun init-node))
         #-clisp until #-clisp (itdepfirst*-andnode-sol tn)
       for depth from 0
         #+clisp until #+clisp (itdepfirst*-andnode-sol tn)
       do (or (proc tn depth scoregreaterfun expandfun assemfun scorefun solutfun off endoff lm) (return))
       finally (return (itdepfirst*-andnode-dat/hp tn)))))

;; SAVE THIS!--old version w/ extra and-nodes, wouldn't want to rewrite this
;; (defun itdepfirst*-engine (init-node scorefun expandfun assemfun solutfun &key scoregreaterfun)
;;   (let* ((*itdepfirst*-id* -1))
;;     (labels ((proc (nd de) ; nd is and-node--shouldn't be called if it's a solution
;; 	       (when (<= de 0) ; if first time, replace data with heap of ors
;; 		 (setf (itdepfirst*-andnode-dat/hp nd)
;; 		       (multiple-value-bind (ii dd) (funcall expandfun (itdepfirst*-andnode-dat/hp nd))
;; 			 (if dd
;; 			     (mapcar (lambda (i) (make-itdepfirst*-andnode :dat/hp i :sol (funcall solutfun i))) ii)
;; 			     (make-heap (lambda (x y)
;; 					  (or (funcall scoregreaterfun (itdepfirst*-ornode-score x) (itdepfirst*-ornode-score y))
;; 					      (unless (funcall scoregreaterfun (itdepfirst*-ornode-score y) (itdepfirst*-ornode-score x))
;; 						(< (itdepfirst*-ornode-id x) (itdepfirst*-ornode-id y)))))
;; 					:initial-contents (loop
;; 							   for e in ii ; e = and-list, heap temporarily contains data to expand
;; 							   when e ; at least something in and-list
;; 							   collect (let ((a (funcall assemfun e)))
;; 								     (make-itdepfirst*-ornode
;; 								      :ands (mapcar (lambda (i) (make-itdepfirst*-andnode :dat/hp i :sol (funcall solutfun i))) e)
;; 								      :assem a :score (funcall scorefun a)))))))))
;; 	       (if (heapp (itdepfirst*-andnode-dat/hp nd))
;; 		   (loop
;; 		    for n = (or (heap-peek (itdepfirst*-andnode-dat/hp nd)) (return)) ; n is or-node
;; 		    for so = (every #'itdepfirst*-andnode-sol (itdepfirst*-ornode-ands n)) ; so = or-node is complete (all and-nodes inside it are complete)
;; 		    until (or so (>= (itdepfirst*-ornode-depth n) de))
;; 		    do
;; 		    (heap-rem (itdepfirst*-andnode-dat/hp nd))
;; 		    (loop
;; 		     for e in (itdepfirst*-ornode-ands n) ; e is and-node
;; 		     if (itdepfirst*-andnode-sol e) collect (itdepfirst*-andnode-dat/hp e) into d
;; 		     else collect (or (proc e (itdepfirst*-ornode-depth n)) (return)) into d ; if dead-end, don't put back into heap
;; 		     finally
;; 		     (incf (itdepfirst*-ornode-depth n))
;; 		     (let ((a (funcall assemfun d)))
;; 		       (setf (itdepfirst*-ornode-assem n) a
;; 			     (itdepfirst*-ornode-score n) (funcall scorefun a)))
;; 		     (heap-ins n (itdepfirst*-andnode-dat/hp nd)))
;; 		    finally
;; 		    (when so (setf (itdepfirst*-andnode-sol nd) t ; this node is a solution--save it
;; 				   (itdepfirst*-andnode-dat/hp nd) (itdepfirst*-ornode-assem n)))
;; 		    (return (itdepfirst*-ornode-assem n)))
;; 		   (loop ; and-node of ands (pass on processing to lower depths without incrementing depth)
;; 		    for e in (or (itdepfirst*-andnode-dat/hp nd) (return))
;; 		    if (itdepfirst*-andnode-sol e) collect (itdepfirst*-andnode-dat/hp e) into d
;; 		    else collect (or (proc e de) (return)) into d ; if one and fails, entire node fails
;; 		    finally
;; 		    (let ((so (every #'itdepfirst*-andnode-sol (itdepfirst*-andnode-dat/hp nd)))
;; 			  (a (funcall assemfun d)))
;; 		      (when so (setf (itdepfirst*-andnode-sol nd) t
;; 				     (itdepfirst*-andnode-dat/hp nd) a))
;; 		      (return a)))))) ; return assembled data or nil if dead-end
;;       (loop
;;        with tn = (make-itdepfirst*-andnode :dat/hp init-node :sol (funcall solutfun init-node))
;;        until (itdepfirst*-andnode-sol tn)
;;        for de from 0
;;        do (or (proc tn de) (return))
;;        finally (return (itdepfirst*-andnode-dat/hp tn))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PREPROCESS

;; adds rests, ties overlapping notes of different durs
;; returns values: notes in measure, notes outside measure
;; expects voices separated into parts, input is sorted, output is sorted

(defun split-preproc (evs off endoff voc)
  (declare (type list evs) (type (rational 0) off endoff) (type (integer 1) voc))
  (multiple-value-bind (gs ns) (split-list evs #'event-grace)
    (loop				; get rid of unison overlaps
     for el on ns do
     (loop
      with e1 of-type (or noteex restex) = (first el)
      for e2 in (rest el) until (>= (event-off e2) (event-endoff e1)) ; e1 and e2 overlap
      do (cond ((and (notep e1) (notep e2) (= (event-note* e1) (event-note* e2)))
		(setf (event-dur e2) (- (max (event-endoff e1) (event-endoff e2)) (event-off e2))
		      (event-dur e1) (- (event-off e2) (event-off e1)))
		(when (<= (event-dur* e1) 0) (setf (event-marks e2) (combmarks (list e1 e2)))))
	       ((and (notep e1) (restp e2)) (setf (event-dur e2) 0))
	       ((and (restp e1) (notep e2)) (setf (event-dur e1) 0) (return)))))
    (setf ns (delete-if (lambda (x) (declare (type (or noteex restex) x)) (<= (event-dur* x) 0)) ns))
    (setf gs (delete-duplicates gs :test
				(lambda (x y)
				  (declare (type (or noteex restex) x y))
				  (and (= (event-note* x) (event-note* y))
				       (= (event-off x) (event-off y))
				       (= (event-grace x) (event-grace y))))))
    (setf ns (nconc (mapcar (lambda (x) (declare (type (cons (rational 0) (rational 0)) x)) (make-restex nil :off (car x) :dur (- (cdr x) (car x)) :voice voc))
			    (get-holes (merge-linear (mapcar (lambda (x) (declare (type (or noteex restex))) (cons (event-off x) (event-endoff x))) ns)
						     (lambda (x y) (declare (type (cons (rational 0) (rational 0)) x y)) (when (<= (car y) (cdr x)) (cons (car x) (cdr y)))))
				       off endoff)) 
		    ns))
    (loop
     for x of-type (or noteex restex) in ns ; split overlapping events
     collect (event-off x) into s
     collect (event-endoff x) into s
     finally
     (loop
      for i of-type (rational 0) in (delete-duplicates (cons endoff s))	; include endoff
      do (setf ns (loop
		   for e of-type (or noteex restex) in ns
		   for (j . k) = (split-event e i)
		   when j collect j
		   when k collect k))))
    (setf ns (loop
	      for e of-type cons in (split-into-groups ns #'event-off) ; put vertical notes into chords (note = list of notes, combine all attributes)
	      if (list>1p e) collect (make-chord e) else collect (first e)))
    (setf gs (loop
	      for e of-type cons in (split-into-groups gs (lambda (x) (declare (type (or noteex restex) x)) (cons (event-off x) (event-grace x))) :test 'equal)	; put vertical notes into chords (note = list of notes, combine all attributes)
	      if (list>1p e) collect (make-chord e) else collect (first e)))
    (loop			  ; split places at grace note offsets
     for g of-type (or noteex restex) in gs
     for i = (event-off g)
     do (setf ns (loop
		  for e of-type (or noteex restex) in ns
		  for (j . k) = (split-event e i)
		  when j collect j
		  when k collect k)))
    (loop
     for e of-type (or noteex restex) in (nconc gs ns) ; separate notes belonging to next measure--notes after endoff already split
     if (< (event-off e) endoff) collect e into v1
     else collect e into v2
     finally (print-dot) (return (values (sort v1 #'sort-offdur) v2)))))

(defun preproc (parts)
  (loop for p of-type partex in parts do		; tie notes across measures
	(loop
	 with r	of-type list 			; leftover tied notes
	 for m of-type meas in (part-meas p) do
	 (multiple-value-bind (e n) (split-preproc (nconc r (meas-events m)) (meas-off m) (meas-endoff m)
						   (let ((i (find-if #'meas-events (part-meas p))))
						     (if i (event-voice* (first (meas-events i))) 1)))
	   (setf (meas-events m) e
		 r (loop for x of-type (or noteex restex) in n if (chordp x)
                      nconc (mapcar (lambda (y t1 t2) (declare (type (or rational list) y) (type (or boolean cons) t1 t2))
                                       (copy-event x :note y :tielt t1 :tiert t2))
                                    (event-note x) (event-tielt x) (event-tiert x))
                      else collect x))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SPLITTER

(declaim (type (real (0)) *min-split-all-parts-dur*))
(defparameter *min-split-all-parts-dur* 3/2)

(defun not-all-tied-p (x)
  (declare (type (or boolean cons) x))
  (not (if (consp x) (and-list x) x)))

(defun notateable-p (division rule evt)
  (declare (type (rational (0) 1) division))
;;;               (break "(no ~a), rule-comp: ~a" di (rule-comp rule))
  (expof2 (* (if (rule-comp rule) (* (event-effectdur evt) 3/2) (event-effectdur evt)) division)))


;; return t if all parts should split together
(defun split-allparts (rule)
  (declare (type baserule rule))
  (when (or (initdivp rule) (sigp rule))
    (let ((n (/ (rule-num rule) (* (rule-den rule) (rule-beat rule)))))
      (and (> n 3/2) (not (expof2 n))))))

;; note/rest is valid duration
;; off, endoff are boundaries for entire measure--for special full-meaure rest cases
(defun split-valid (events off endoff rule)
  (declare (type cons events) (type (rational 0) off endoff) (type baserule rule))
  (when (list1p events)			; must be 1 event only
    (let ((evt (first events))) ; shouldn't be dealing with grace notes here
      (declare (type (or noteex restex) evt))
      ; something wrong here
      (etypecase rule
        (initdiv
         (etypecase evt
           (rest t)
           (note (or (notateable-p 1 rule evt) (notateable-p 2/3 rule evt))))) ;;; should also check for dotted notes as whole bar notes in 3/4 meters
        (sig
         (etypecase evt
           (rest (or (and (= (event-off evt) off) (= (event-endoff evt) endoff))
                     (if (or (rule-comp rule) (rule-irr rule)) ; irr = irregular, like 5/8 meter
                         (or (notateable-p 1 rule evt) (notateable-p 2/3 rule evt))
                         (notateable-p 1 rule evt))))
           (note (if (or (rule-comp rule) (rule-irr rule))
                     (or (notateable-p 1 rule evt) (notateable-p 2/3 rule evt))
                     (notateable-p 1 rule evt)))))
        (unit
         (cond ((= (rule-div rule) 2) (notateable-p 1 rule evt))
               ((and (= (rule-div rule) 3) (rule-irr rule)) (notateable-p 2/3 rule evt))))
        (sig-nodiv ; tlt/trt: nil = ties not allowed, t = tie is possible 
         (etypecase evt
           (rest nil #|(if (sig-nodiv-comp rule) (notateable-p 2/3) (notateable-p 1))|#)
           (note (let ((boundary-p (or (not-all-tied-p (event-tielt evt))
                                       (not-all-tied-p (event-tiert evt)))))
                   (and ; these are special, so duration is assumed to be valid
                    (or (rule-tlt rule) boundary-p) ; at least one note not tied, if ties aren't allowed by rule.
                    (or (rule-trt rule) boundary-p)
                    (if (rule-comp rule) (notateable-p 2/3 rule evt) (or (notateable-p 1 rule evt) (notateable-p 2/3 rule evt) (notateable-p 4/7 rule evt) #|(and (notateable-p 4/7) (not (event-noddot evt)))|#))))))) ;; lilypond fix
        (unit-nodiv ; tlt/trt: nil = ties not allowed, t = tie is possible 
         (etypecase evt
           (rest #|nil|# (and (rule-rst rule) (notateable-p 1 rule evt))) ; 
           (note (let ((boundary-p (or (not-all-tied-p (event-tielt evt))
                                       (not-all-tied-p (event-tiert evt)))))
                   (and ; these are special, so duration is assumed to be valid
                    (or (rule-tlt rule) boundary-p)
                    (or (rule-trt rule) boundary-p)
                    (or (notateable-p 1 rule evt) (notateable-p 2/3 rule evt) (notateable-p 4/7 rule evt) #|(and (notateable-p 4/7 rule evt) (not (event-noddot evt)))|#)))))))))) ;; lilypond fix


(declaim (type (real (0)) +event-score+ +tuplet-score+ +tupsmalldur-score+ +tupsmalldur-thresh+))
(defparameter +event-score+ 1)
(defparameter +tuplet-score+ 3/2)
(defparameter +smalltupnote-score+ 2) ; should be slightly higher than tuplet-score

;; returns list: lower number is better
;; first value MUST be optimistic (increasing only (worse) with each descent (note splitting) in search)
;; remaining values are heuristics for resolving ties
(defun split-score (events)
  (declare (type list events))
  (loop
   with ntu
   for (e en) of-type ((or noteex restex) (or noteex restex null)) on (sort (copy-list events) #'< :key #'event-off) ; no overlapping offsets should exist here
   for d = (event-dur* e)
   sum +event-score+ into su
   when (event-tupfrac e)
   sum (loop with tf = (event-tupfrac e) with le = (length tf)
	     for i from 1 and x of-type (rational (0)) in tf
	     for m = x then (* m x)
	     sum m into s
	     when (> i 1) do (push (cons (cons (- le i) (cons (event-off e) (event-endoff e))) (cons m x)) ntu)
	     finally (return (* s le +tuplet-score+))) into su
   and when (< (* (numerator (first (event-tupdurmult e))) (first (event-tupfrac e))) 1)
   sum (* (first (event-tupfrac e)) +smalltupnote-score+) into su
   maximize d into ad
   minimize d into id
   when en sum (diff d (event-dur* en)) into ce
   collect (event-off e) into fs
   collect (event-endoff e) into fs
   finally
   (loop with c = 0 and va and f0 = 0 ; look for contiguous nested tuplets of same duration, give them a boost
	 for l0 = 0 then l and o02 = -1 then o2	; l = level, o1/o2 = off/endoff, m = fraction of entire tuplet, f = fraction of tuplet at that level
	 and ((l . (o1 . o2)) . (m . f)) of-type (((integer 0) . ((rational 0) . (rational 0))) . ((rational (0)) . (rational (0))))
	 in (sort ntu (lambda (x y) (declare (type (cons (cons (integer 0) (cons (rational 0) (rational 0)))) x y))
			      (if (= (caar x) (caar y)) (< (cadar x) (cadar y)) (> (caar x) (caar y)))))
	 if (or (/= l0 l) (/= o02 o1) (>= c 1))
	 do (when va (decf su (* (1- (/ f)) c +tuplet-score+))) (setf c m f0 0)
	 else do (incf c m)		; new group, reset everything
	 if (= f0 0) do (setf f0 f va t) else when (/= f0 f) do (setf va nil)
	 finally (when va (decf su (* (1- (/ f)) c +tuplet-score+))))
   (return (list su ; number of notes/tuplets (can only increase with splitting)
		 #-clisp (/ ad id) #+clisp (/ (or ad 0) id) ; difference in durations--might not always increase, but still functions well as a heuristic
		 ce (ave-list (delete-duplicates fs)))))) ; average offset location

;; (defun split-score (events)
;;   (declare (type list events))
;;   (loop
;;    with ntu and ntg
;;    for (e en) of-type ((or noteex restex) (or noteex restex null)) on (sort (copy-list events) #'< :key #'event-off) ; no overlapping offsets should exist here
;;    for d = (event-dur* e)
;;    sum +event-score+ into su
;;    when (event-tupfrac e)
;;    sum (loop with tf = (event-tupfrac e) with le = (length tf)
;; 	     for i from 1 and x of-type (rational (0)) in tf
;; 	     for m = x then (* m x)
;; 	     sum m into s
;; 	     do (if (> i 1)
;; 		    (push (cons (cons (- le i) (cons (event-off e) (event-endoff e))) (cons m x)) ntu)
;; 		    (push (cons (cons (event-off e) (event-endoff e)) (cons m x)) ntg))
;; 	     finally (return (* s le +tuplet-score+))) into su
;;    and when (< (* (numerator (first (event-tupdurmult e))) (first (event-tupfrac e))) 1)
;;    sum (* (first (event-tupfrac e)) +smalltupnote-score+) into su
;;    maximize d into ad
;;    minimize d into id
;;    when en sum (diff d (event-dur* en)) into ce
;;    collect (event-off e) into fs
;;    collect (event-endoff e) into fs
;;    finally
;;    (loop with c = 0 and va and f0 = 0 ; look for contiguous nested tuplets of same duration, give them a boost
;; 	 for l0 = 0 then l and o02 = -1 then o2	; l = level, o1/o2 = off/endoff, m = fraction of entire tuplet, f = fraction of tuplet at that level
;; 	 and ((l . (o1 . o2)) . (m . f)) of-type (((integer 0) . ((rational 0) . (rational 0))) . ((rational (0)) . (rational (0))))
;; 	 in (sort ntu (lambda (x y) (declare (type (cons (cons (integer 0) (cons (rational 0) (rational 0)))) x y))
;; 			      (if (= (caar x) (caar y)) (< (cadar x) (cadar y)) (> (caar x) (caar y)))))
;; 	 if (or (/= l0 l) (/= o02 o1) (>= c 1))
;; 	 do (when va (decf su (* (1- (/ f)) c +tuplet-score+))) (setf c m f0 0)	; new group, reset everything
;; 	 else do (incf c m)	
;; 	 if (= f0 0) do (setf f0 f va t) else when (/= f0 f) do (setf va nil)
;; 	 finally (when va (decf su (* (1- (/ f)) c +tuplet-score+))))
;;    (loop with c = 0 and va and f0 = 0 ; look for contiguous nested tuplets of same duration, give them a boost
;; 	 for o02 = -1 then o2 ; o1/o2 = off/endoff, m = fraction of entire tuplet, f = fraction of tuplet at that level
;; 	 and ((o1 . o2) . (m . f)) of-type (((rational 0) . (rational 0)) . ((rational (0)) . (rational (0))))
;; 	 in (sort ntg (lambda (x y) (declare (type (cons (cons (rational 0) (rational 0))) x y)) (< (caar x) (caar y))))
;; 	 if (>= c 1) do (when va (decf su (* (1- (/ f)) c +tuplet-score+)))
;; 	 if (or (/= o02 o1) (>= c 1)) (setf c m f0 0) else do (incf c m)
;; 	 if (= f0 0) do (setf f0 f va t) else when (/= f0 f) do (setf va nil)
;; 	 finally (when va (decf su (* (1- (/ f)) c +tuplet-score+))))
;;    (return (list su ; number of notes/tuplets (can only increase with splitting)
;; 		 #-clisp (/ ad id) #+clisp (/ (or ad 0) id) ; difference in durations--might not always increase, but still functions well as a heuristic
;; 		 ce (ave-list (delete-duplicates fs)))))) ; average offset location

;; = and < function for score tuplet

(defun splsc< (x y)
  (declare (type cons x y))
  (loop
   for x0 of-type real in x and y0 of-type real in y
   if (< x0 y0) do (return t)
   if (> x0 y0) do (return nil)))

;; maximum duration span of tuplets in number of beats (or nil)
;; (declaim (type (real (0)) *min-simple-tuplet-dur*))
;; (defparameter *min-simple-tuplet-dur* #|2 4/6/06|# nil)

;; events = list of parallel event-lists
;; expects voices separated into parts
;; DESTRUCTIVE
(defstruct (splitnode (:copier nil) (:predicate splitnodep))
  (rl (make-initdiv) :type baserule)
  (pts t :type boolean)
  (par nil :type (or splitnode null))
  (evs nil :type (or list boolean))
  (of1 0 :type (rational 0))
  (of2 0 :type (rational 0))
  (div nil :type list)) ; msc = missing score points, pts = if evs is list of part-event-lists

(defun merge-rests (e1 e2 es)   ; es is copied, so can destroy it
  "e1 and e2 are the (sorted) first two evts of es. If e1 and e2 are
rests and have the same marks, duration and tuplet and the offset of
e2 is not in the nomerge list of e1, combine them into a single rest
evt. Returns es."
  (declare (type (or noteex restex null) e1 e2) (type cons es))
  (if (and (restp e1) (restp e2)
           (not (find (event-off e2) (event-nomerge e1)))
           (equal (list (event-dur* e1) (sort-marks (important-marks (event-marks e1))) (event-tup e1))
                  (list (event-dur* e2) (sort-marks (important-marks (event-marks e2))) (event-tup e2))))
      (cons (copy-event e1
                        :dur (* (event-dur* e1) 2)
                        :tup (cons (when (car (event-tup e1))
                                     (cons (* (caar (event-tup e1)) 2)
                                           (cdar (event-tup e1))))
                                   (cdr (event-tup e1))))
            (delete e1 (delete e2 es)))
      es))

(defun drst (evts rule) ; move the rest over
  (declare (type cons evts) (type (or initdiv sig unit sig-nodiv unit-nodiv) rule))
  (when (or (initdivp rule) (basesplitp rule))
    (when (or (initdivp rule) (rule-alt rule))
      (let ((sorted-evts (sort (copy-list evts) #'sort-offdur)))
        (setf evts (merge-rests (first sorted-evts) (second sorted-evts) sorted-evts))))
    (when (or (initdivp rule) (rule-art rule))
      (let ((sorted-evts (sort (copy-list evts) (complement #'sort-offdur)))) ;;; sort evts backwards
        (setf evts (merge-rests (second sorted-evts) (first sorted-evts) sorted-evts)))))
  evts)

(defun er (off endoff) (error "Rhythm too difficult to notate at offsets ~S through ~S" (float off) (float endoff)))

(defun spl (evs sp rr nd)           ; return list of split event-lists
  (declare (type cons sp rr))
  (loop
     with nx = evs and td0 = (- (splitnode-of2 nd) (splitnode-of1 nd))
     for o0 = 0 then o
     for o of-type (rational (0) 1) in sp and r in rr ; o = split offset, r = rule
     collect (loop
                with u = (when (baseunitp r) (rule-tup r)) ; u = tuplet list--rule should have all tuplet information for note
                and m = (when (baseunitp r) (rule-dmu r))
                and td = (* (- o o0) td0)
                for e of-type (or noteex restex) in nx
                for (l . x) = (split-event e (of o nd) (when u (cons (* (first u) (/ (event-dur e) td)) (rest u))) m)
                when l collect l into ll
                when x collect x into xx
                finally
                  (setf nx xx)
                  (return ll))))

(defun scorefun (node)                    ; score relative to ea. level
  (declare (type splitnode node))
  (if (splitnode-pts node)
      (loop
         for e in (remove-if #'truep (splitnode-evs node)) ; if t then part is already complete in a higher branch
         for ts = (split-score e) then (mapcar #'+ ts (split-score e))
         finally (return (cons 0 ts)))
      (cons 1 (split-score (splitnode-evs node)))))

(defun of (o nd) (declare (type (rational 0) o)) (+ (splitnode-of1 nd) (* o (- (splitnode-of2 nd) (splitnode-of1 nd)))))

(defun assemfun (nds) ; assemble and-list of splitnodes (some might = t, some might have parts that = t, there should be at least 1 node struct somewhere)
  (declare (type list nds))
  (let* ((f (first nds)) ; find a node struct--par and pts should be same for all
         (pa (splitnode-par f))
         (rl (splitnode-rl pa)))
    (declare (type splitnode f))
    (flet ((mn (vs)
             (make-splitnode :rl rl :pts (splitnode-pts pa) :par (splitnode-par pa)
                             :evs vs :of1 (splitnode-of1 pa) :of2 (splitnode-of2 pa)
                             :div (first (stable-sort (loop for i of-type splitnode in nds when (splitnode-div i) collect (splitnode-div i))
                                                      #'> :key (lambda (x) (declare (type list x)) (count x nds :key #'splitnode-div :test #'equal)))))))
      (if (splitnode-pts f)
          (loop
             with li
             for s in nds               ; all s's should be structs
             and fl = t then nil
             do (let ((xx (mapcar (lambda (x y)
                                    (declare (type (or (member t) cons) x y))
                                    (if (truep x)
                                        (when fl (unless (truep y) y))
                                        x))
                                  (splitnode-evs s) (splitnode-evs pa))))
                  (if li (prepend-lists xx li) (setf li xx)))
             finally (return (mn (mapcar (lambda (x) (declare (type list x)) (if x (drst x rl) t)) li))))
          (if (splitnode-pts pa)
              (mn (mapcar (lambda (x0 y)
                            (declare (type splitnode x0) (type (or (member t) cons) y))
                            (let ((x (splitnode-evs x0))) (if (truep x) (if (truep y) y (drst y rl)) (drst x rl))))
                          nds (splitnode-evs pa))) 
              (mn (drst (loop for e of-type splitnode in nds append (splitnode-evs e)) rl)))))))

;;; check that the offsets of all events of node are aligning to rules
(defun solutfun (node off endoff)                    ; complete/valid?
  (declare (type splitnode node))
  (if (splitnode-pts node) ;; are the evts of the node a list of lists (=> in different parts)?
      (let ((rule (splitnode-rl node)))
        (every (lambda (evt) (declare (type (or (member t) cons) evt))
                  (or (truep evt)
                      (split-valid evt off endoff rule)))
               (splitnode-evs node)))
      (or (truep (splitnode-evs node))
          (split-valid (splitnode-evs node) off endoff (splitnode-rl node)))))

;;; event-nomerge is a list of offsets where events shouldn't get
;;; merged: in split-engine-byscore these points are set to positions,
;;; where grace notes occur.

(defun split-engine-byscore (events off endoff timesig)
  (declare (type cons events) (type (rational 0) off endoff) (type timesig-repl timesig))
  (let ((lm (/ (* (beat-division timesig) 8)))) ;;; lm is 1/128 in non-compound and 1/192 in compound time-signatures; seems to be the smallest unit for expanding divisions.
    (multiple-value-bind (evs grs) ;;; split evts into list of evts
                                   ;;; and list of corresponding grace
                                   ;;; notes. In each evtlist mark the
                                   ;;; offsets of the grace notes of
                                   ;;; all rest evts as nomerge.
        (loop
           for p of-type cons in events
           for (gr . ev) = (multiple-value-bind (a b) (split-list p #'event-grace) (cons a b))
           collect ev into evs
           collect gr into grs
           finally (return (values evs grs)))
      (loop for li of-type cons in evs and gr of-type list in grs do
           (loop with g = (delete-duplicates (mapcar #'event-off gr))
              for e of-type (or noteex restex) in li when (restp e) do (setf (event-nomerge e) g)))
      (let ((result (or (itdepfirst*-engine
                         (make-splitnode :rl (first-splitrule timesig)
                                         :evs evs
                                         :of1 off :of2 endoff)
                         #'splsc< #'scorefun #'expandfun #'assemfun #'solutfun off endoff lm)
                        (er off endoff))))
        (print-dot)
        (values (let ((rule (splitnode-rl result)))
                  (mapcar (lambda (evt grace) (declare (type cons evt) (type list grace)) (sort (nconc grace (drst evt rule)) #'sort-offdur)) (splitnode-evs result) grs))
                (splitnode-div result))))))

#|

(trace (flet er split-engine-by-score))

(defun split-engine-byscore (events off endoff timesig)
  (declare (type cons events) (type (rational 0) off endoff) (type timesig-repl timesig))
  (flet ((er () (error "Rhythm too difficult to notate at offsets ~S through ~S" (float off) (float endoff)))
	 (drst (li rl)			; move the rest over
	   (declare (type cons li) (type (or initdiv sig unit sig-nodiv unit-nodiv) rl))
	   (flet ((ex (e1 e2 es)     ; es is copied, so can destroy it
		    (declare (type (or noteex restex null) e1 e2) (type cons es))
		    (if (and (restp e1) (restp e2)
			     (not (find (event-off e2) (event-nomerge e1)))
			     (equal (list (event-dur* e1) (sort-marks (important-marks (event-marks e1))) (event-tup e1))
				    (list (event-dur* e2) (sort-marks (important-marks (event-marks e2))) (event-tup e2))))
			(cons (copy-event e1
					  :dur (* (event-dur* e1) 2)
					  :tup (cons (when (car (event-tup e1))
						       (cons (* (caar (event-tup e1)) 2)
							     (cdar (event-tup e1))))
						     (cdr (event-tup e1))))
			      (delete e1 (delete e2 es)))
			es)))
	     (when (or (initdivp rl) (basesplitp rl))
	       (when (or (initdivp rl) (rule-alt rl))
		 (let ((x (sort (copy-list li) #'sort-offdur)))
		   (setf li (ex (first x) (second x) x))))
	       (when (or (initdivp rl) (rule-art rl))
		 (let ((x (sort (copy-list li) (complement #'sort-offdur)))) ;;; sort list backwards
		   (setf li (ex (second x) (first x) x))))))
	   li))
    (let ((lm (/ (* (beat-division timesig) 8))))
      (flet ((scorefun (nd)		; score relative to ea. level
	       (declare (type splitnode nd))
	       (if (splitnode-pts nd)
		   (loop
		    for e in (remove-if #'truep (splitnode-evs nd)) ; if t then part is already complete in a higher branch
		    for ts = (split-score e) then (mapcar #'+ ts (split-score e))
		    finally (return (cons 0 ts)))
		   (cons 1 (split-score (splitnode-evs nd)))))
	     (expandfun (nd)		; expand (into or-list)
	       (declare (type splitnode nd))
	       ;; (when (<= (- (splitnode-of2 nd) (splitnode-of1 nd)) lm) (er))
	       (unless (or (basenodivp (splitnode-rl nd)) (<= (- (splitnode-of2 nd) (splitnode-of1 nd)) lm))
		 (let ((rt (labels ((of (o) (declare (type (rational 0) o)) (+ (splitnode-of1 nd) (* o (- (splitnode-of2 nd) (splitnode-of1 nd)))))
				    (spl (evs sp rr) ; return list of split event-lists
				      (declare (type cons sp rr))
				      (loop
				       with nx = evs and td0 = (- (splitnode-of2 nd) (splitnode-of1 nd))
				       for o0 = 0 then o
				       for o of-type (rational (0) 1) in sp and r in rr	; o = split offset, r = rule
				       collect (loop
						with u = (when (baseunitp r) (rule-tup r)) ; u = tuplet list--rule should have all tuplet information for note
						and m = (when (baseunitp r) (rule-dmu r))
						and td = (* (- o o0) td0)
						for e of-type (or noteex restex) in nx
						for (l . x) = (split-event e (of o) (when u (cons (* (first u) (/ (event-dur e) td)) (rest u))) m)
						when l collect l into ll
						when x collect x into xx
						finally
						(setf nx xx)
						(return ll)))))
			     (loop
			      for ru of-type (cons (or (rational (0) (1)) cons) list)
			      in (loop for e 
				       in (split-rules-bylevel
					   (splitnode-rl nd)
					   (let ((du (- (splitnode-of2 nd) (splitnode-of1 nd))))
					     (and (or (null *min-tuplet-dur*) (>= du *min-tuplet-dur*))
						  (or (null *max-tuplet-dur*) (<= du *max-tuplet-dur*)))))
				       collect e) ; ors, ru = new rule
			      for div = (or (when (basesplitp (splitnode-rl nd)) (rule-init (splitnode-rl nd)))
					    (splitnode-div nd))
			      for sp = (append (force-list (first ru)) '(1)) and rr = (rest ru)	; sp =(unless (and tv (eq (first sp) :grandstaff)) (first sp)) split points (last one is a), rr = replacement rules
			      collect (if (splitnode-pts nd)
					  (loop	; iterate through parts
					   with al = (make-list (length rr) :initial-element t)
					   for p of-type (or (member t) cons) in (splitnode-evs nd)
					   for xx = (if (or (truep p) (split-valid p off endoff (splitnode-rl nd))) al (spl p sp rr))
					   for li = (mapcar #'list xx) then (cons-list xx li)
					   finally (return (loop 
							    for e of-type cons in li and r in rr
							    and (o1 o2) of-type ((rational 0 1) (rational 0 1)) on (cons 0 sp) ; ands
							    collect (make-splitnode :rl r :pts t :par nd :evs (nreverse e) :of1 (of o1) :of2 (of o2) :div div)))) ; evs might contain t
					  (loop
					   for e of-type cons in (spl (splitnode-evs nd) sp rr) and r in rr
					   and (o1 o2) of-type ((rational 0 1) (rational 0 1)) on (cons 0 sp) ; ands
					   collect (make-splitnode :rl r :pts nil :par nd :evs e :of1 (of o1) :of2 (of o2) :div div)))))))
		   (if (and (splitnode-pts nd) (not (split-allparts (splitnode-rl nd))))
		       (cons (mapcar (lambda (p)
				       (make-splitnode :rl (splitnode-rl nd) :pts nil :par nd
						       :evs (if (or (truep p) (split-valid p off endoff (splitnode-rl nd))) t p)
						       :of1 (splitnode-of1 nd) :of2 (splitnode-of2 nd) :div (splitnode-div nd)))
				     (splitnode-evs nd))
			     rt)
		       rt))))
	     (assemfun (nds) ; assemble and-list of splitnodes (some might = t, some might have parts that = t, there should be at least 1 node struct somewhere)
	       (declare (type list nds))
	       (let* ((f (first nds)) ; find a node struct--par and pts should be same for all
		      (pa (splitnode-par f))
		      (rl (splitnode-rl pa)))
		 (declare (type splitnode f))
		 (flet ((mn (vs)
			  (make-splitnode :rl rl :pts (splitnode-pts pa) :par (splitnode-par pa)
					  :evs vs :of1 (splitnode-of1 pa) :of2 (splitnode-of2 pa)
					  :div (first (stable-sort (loop for i of-type splitnode in nds when (splitnode-div i) collect (splitnode-div i))
								   #'> :key (lambda (x) (declare (type list x)) (count x nds :key #'splitnode-div :test #'equal)))))))
		   (if (splitnode-pts f)
		       (loop
			with li
			for s in nds	; all s's should be structs
			and fl = t then nil
			do (let ((xx (mapcar (lambda (x y)
					       (declare (type (or (member t) cons) x y))
					       (if (truep x)
						   (when fl (unless (truep y) y))
						   x))
					     (splitnode-evs s) (splitnode-evs pa))))
			     (if li (prepend-lists xx li) (setf li xx)))
			finally (return (mn (mapcar (lambda (x) (declare (type list x)) (if x (drst x rl) t)) li))))
		       (if (splitnode-pts pa)
			   (mn (mapcar (lambda (x0 y)
					 (declare (type splitnode x0) (type (or (member t) cons) y))
					 (let ((x (splitnode-evs x0))) (if (truep x) (if (truep y) y (drst y rl)) (drst x rl))))
				       nds (splitnode-evs pa))) 
			   (mn (drst (loop for e of-type splitnode in nds append (splitnode-evs e)) rl)))))))
	     (solutfun (nd)		; complete/valid?
	       (declare (type splitnode nd))
	       (if (splitnode-pts nd)
		   (let ((x (splitnode-rl nd)))
		     (every (lambda (n) (declare (type (or (member t) cons) n)) (or (truep n) (split-valid n off endoff x))) (splitnode-evs nd)))
		   (or (truep (splitnode-evs nd))
		       (split-valid (splitnode-evs nd) off endoff (splitnode-rl nd))))))
	(multiple-value-bind (evs grs)
	    (loop
	     for p of-type cons in events
	     for (gr . ev) = (multiple-value-bind (a b) (split-list p #'event-grace) (cons a b))
	     collect ev into evs
	     collect gr into grs
	     finally (return (values evs grs)))
	  (loop for li of-type cons in evs and gr of-type list in grs do
		(loop with g = (delete-duplicates (mapcar #'event-off gr))
		      for e of-type (or noteex restex) in li when (restp e) do (setf (event-nomerge e) g)))
	  (let ((re (or (itdepfirst*-engine
			 (make-splitnode :rl (first-splitrule timesig)
					 :evs evs
					 :of1 off :of2 endoff)
			 #'scorefun #'expandfun #'assemfun #'solutfun
			 :scoregreaterfun #'splsc<)
			(er))))
	    (print-dot)
	    (values (let ((rl (splitnode-rl re)))
		      (mapcar (lambda (ev gr) (declare (type cons ev) (type list gr)) (sort (nconc gr (drst ev rl)) #'sort-offdur)) (splitnode-evs re) grs))
		    (splitnode-div re))))))))
|#

(declaim (type symbol *split-plugin* *split-module*))
(defparameter *split-plugin* nil)
(defparameter *split-module* t)
(declaim (inline split-fun))
(defun split-fun () (if (truep *split-module*) :split1 *split-module*))

(declaim (inline load-split-modules))
(defun load-split-modules ()
  (unless (eq (split-fun) :split1) (load-fomus-module (split-fun))))

;; the main function--events must be organized into measures (by offsets) first
(defun split (parts)
  (declare (type list parts))
  (apply #'mapc
	 (lambda (&rest ms)		; list of parallel measures
	   (loop for ml of-type cons
		 in (split-into-groups ms ; m is list of measures with matching time signature
				       (lambda (x)
					 (declare (type meas x))
					 (let ((s (meas-timesig x)))
					   (list (timesig-num s) (timesig-den s) (timesig-div* s) (timesig-comp s) (timesig-beat* s)
						 (meas-off x) (meas-endoff x))))
				       :test 'equal)
		 do (multiple-value-bind (sp di) (let ((f (first ml)))
						   (declare (type meas f))
						   (if (eq (split-fun) :split1)
						       (split-engine-byscore (mapcar #'meas-events ml) (meas-off f) (meas-endoff f) (meas-timesig f))
						       (call-module (split-fun) (list "Unknown split module ~S" *split-module*)
								    (mapcar #'meas-events ml) (meas-off f) (meas-endoff f) (meas-timesig f))))
		      (mapc
		       (lambda (re m)
			 (declare (type list re) (type meas m))
			 (setf (meas-events m) re (meas-div m) di))
		       sp ml))))
	 (mapcar #'part-meas parts)))

