;; -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**************************************************************************************************
;; FOMUS
;; beams.lisp
;;**************************************************************************************************

(in-package :fomus)
(compile-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEAMS

(declaim (type symbol *auto-beam-plugin* *auto-beam-module*))
(defparameter *auto-beam-plugin* nil)
(defparameter *auto-beam-module* t)
(declaim (inline auto-beam-fun))
(defun auto-beam-fun () (if (truep *auto-beam-module*) :beams1 *auto-beam-module*))

(declaim (type boolean *auto-beams*))
(defparameter *auto-beams* t)

(declaim (type (member nil t :always) *long-eighth-beams*)
	 (type (integer 1) *long-eighth-beam-count*)
	 (type (integer 1) *comp-long-eighth-beam-count*))
(defparameter *long-eighth-beams* t) ; set to t (half-note under certain conditions), nil (quarter-note) or :always
(defparameter *long-eighth-beam-count* #|3 5/9/06|# 3) ; number of 1/8 notes to activate large grouping, if 0, will ALWAYS beam a whole 1/2 note if possible
(defparameter *comp-long-eighth-beam-count* 5)

;; decision to group four or six eigth notes together
;; receives all events overlapping into area <= proper duration
;; events are separated into different voices
(defun beams-grouplarge (events timesig)
  (declare (type list events) (type timesig-repl timesig))
  (if (timesig-comp timesig)
      (or (eq *long-eighth-beams* :always) 
	  (and *long-eighth-beams* 
	       (>= (count-if (lambda (x) (declare (type (or noteex restex) x)) (and (notep x) (null (event-tupfrac x)) (= (event-writtendur* x timesig) 1/8))) events)
		   *comp-long-eighth-beam-count*)))
      (or (eq *long-eighth-beams* :always)
	  (and *long-eighth-beams* 
	       (>= (count-if (lambda (x) (declare (type (or noteex restex) x)) (and (notep x) (null (event-tupfrac x)) (= (event-writtendur* x timesig) 1/8))) events)
		   *long-eighth-beam-count*)))))

;; top level generic rules for dividing/beaming measures
;; input written duration
;; beamdur starts at 1/8, then 1/16, etc.
(defun beams-rules (events off writdur beamdur timesig)
  (declare (type list events) (type (real 0) off) (type (rational (0)) writdur beamdur) (type timesig-repl timesig))
  (flet ((fi (wd)
	   (declare (type (rational (0)) wd))
	   (loop with o = (+ off (/ wd (timesig-beat* timesig)))
		 for e in events
		 when (< (event-off e) o) collect e into r1
		 when (> (event-endoff e) o) collect e into r2
		 finally (return (values r1 r2 o)))))
    (if (timesig-comp timesig)
	(cond
	  ((and (<= writdur 6/8) (beams-grouplarge events timesig) (>= beamdur (/ (timesig-den timesig)))) (list writdur)) ; 6/8
	  ((<= writdur 3/8) (list writdur))
	  ((and (<= writdur 6/8) (>= (timesig-den timesig) 8)) (list 3/8 (- writdur 3/8))) ; 9/19/06 added (>= (timesig-den timesig) 8)
	  (t (multiple-value-bind (e1 e2 off2) (fi 6/8)
	       (nconc (beams-rules e1 off 6/8 beamdur timesig)
		      (when (> writdur 6/8) (beams-rules e2 off2 (- writdur 6/8) beamdur timesig))))))
	(cond
	  ((and (<= writdur 6/8) (beams-grouplarge events timesig)) (list writdur)) ; 4/8
	  ((<= writdur 2/8) (list writdur))
	  ((<= writdur 4/8) (list 2/8 (- writdur 2/8)))
	  ((<= writdur 5/8) (list 3/8 (- writdur 3/8)))
	  (t (multiple-value-bind (e1 e2 off2) (fi 4/8)
	       (nconc (beams-rules e1 off 4/8 beamdur timesig)
		      (when (> writdur 4/8) (beams-rules e2 off2 (- writdur 4/8) beamdur timesig)))))))))

;; must be before postproc adds tuplet marks???
(defun beams-standbydiv (meas)		; list of measures
  (declare (type list meas))
  (loop for m of-type meas in meas
	do (multiple-value-bind (grs evs) (split-list (meas-events m) #'event-grace)
	     (let ((ts (meas-timesig m)))
	       (labels ((spt (evs wd wl &optional dmu (tf 0)) ; return events unsorted in their groups
			  (declare (type cons evs) (type (or (rational (0)) null) wd) (type list wl) (type list dmu) (type (integer 0) tf))
			  (let ((wll (loop for e of-type (rational (0)) in wl ; is nil for tuplet areas
					   nconc (loop while (> e wd)
						       collect wd into re
						       do (decf e wd)
						       finally (return (nconc re (list e)))))))
			    (flet ((pwd (o)
				     (declare (type (rational 0) o))
				     (loop while (and wll (>= o (the (rational (0)) (car wll)))) do (decf o (pop wll)) finally (return o))))
			      (loop
			       with ee = evs and o of-type (rational 0) = 0 and re and rr ; re and rr should be in reverse order, re is group-list, rr is list of group-lists
			       #-clisp while #-clisp ee
			       for e of-type (or noteex restex) = #-clisp (car ee) #+clisp (if ee (car ee) (loop-finish)) 
			       do (if (not (equal (event-tupdurmult e) dmu)) ; different tuplet region
				      (progn
					(prenconc
					 (let ((xa (when (and re (> o 0)) (copy-event (first re)))) ; oo = t if tuplet section doesn't start on boundary
					       (x (loop	; gather events in tuplet
						   with pr = 0
						   for ee0 on ee
						   for e0 = (car ee0)
						   do (incf pr (apply #'* (butlast (event-tupfrac e0) tf)))
						   while (<= pr 1)
						   collect e0
						   do (incf o (event-writtendur e0 ts dmu))
						   finally (setf ee ee0)))) ; x is in forward order
					   (when re (push re rr) (setf re nil))	; first of re is the largest offset
					   (let ((xr (spt x nil nil (event-tupdurmult e) (1+ tf))))
					     (when xa (nconc (last-element xr) (list xa))) ; "prepend" for continuous beaming
					     xr))
					 rr)
					(setf o (pwd o))
					(when (> o 0) (setf re (list (copy-event (first (first rr))))))) ; "append" copy for continuous beaming
				      (progn
					(push e re)
					(incf o (event-writtendur e ts))
					(when (and wll (>= o (car wll)))
					  (push re rr)
					  (setf re nil)
					  (setf o (pwd o)))
					(setf ee (rest ee))))
			       finally
			       (return (if re (cons re rr) rr))))))) 
		 (flet ((bm (evs dv wl)	; dv = number of beams, ad = written beam duration
			  (declare (type cons evs) (type (integer 1) dv) (type cons wl))
			  (let ((ad (if (= dv 1) 3/4 (expt 1/2 dv)))) ; ad is written division duration
			    (let ((spf (nreverse (mapcar #'nreverse (spt evs ad wl))))
				  (spb (nreverse (mapcar #'nreverse (spt (reverse evs) ad (nreverse wl))))))
			      #+debug (check-order spf "BEAMS-STANDBYDIV (1)" (lambda (x y) (<= (event-off (first x)) (event-off (first y)))))
			      #+debug (mapc (lambda (i) (check-order i "BEAMS-STANDBYDIV (2)" (lambda (x y) (<= (event-off x) (event-off y))))) spf)
			      #+debug (check-order spb "BEAMS-STANDBYDIV (3)" (lambda (x y) (>= (event-off (first x)) (event-off (first y)))))
			      #+debug (mapc (lambda (i) (check-order i "BEAMS-STANDBYDIV (4)" (lambda (x y) (>= (event-off x) (event-off y))))) spb)
			      (loop for ee of-type cons in spf
				    do (loop
					for (e0 e1) of-type ((or noteex restex) (or noteex restex null)) on ee while e1
					when (and (notep e0) (notep e1))
					do (setf (event-beamlt e1) (min dv (event-nbeams e0 ts) (event-nbeams e1 ts)))))
			      (loop for ee of-type cons in spb
				    do (loop
					for (e0 e1) of-type ((or noteex restex) (or noteex restex null)) on ee while e1
					when (and (notep e0) (notep e1))
					do (setf (event-beamrt e1) (min dv (event-nbeams e0 ts) (event-nbeams e1 ts)))))
			      (cons spf spb))))
			(fb (spf spb)
			  (declare (type cons spf spb))
			  (let ((ll nil) (lr nil)) ; fix beams that don't have enough
			    (loop for ee of-type cons in spf
				  do (loop
				      for (e0 e1) of-type ((or noteex restex) (or noteex restex null)) on ee #-clisp while #-clisp e1
				      for nb = #-clisp (event-nbeams e1 ts) #+clisp (if e1 (event-nbeams e1 ts) (loop-finish))
				      when (and (notep e0) (notep e1) (> (event-beamrt e0) 0)
						(and (< (event-beamlt e1) nb) (or (< (event-beamrt e1) nb) (= (event-beamrt e0) nb))))
				      do (push (cons (event-nbeams e1 ts) e1) ll)))
			    (loop for ee of-type cons in spb
				  do (loop for (e0 e1) of-type ((or noteex restex) (or noteex restex null)) on ee #-clisp while #-clisp e1
					   for nb = #-clisp (event-nbeams e1 ts) #+clisp (if e1 (event-nbeams e1 ts) (loop-finish))
					   when (and (notep e0) (notep e1) (> (event-beamlt e0) 0)
						     (and (or (< (event-beamlt e1) nb) (= (event-beamlt e0) nb)) (< (event-beamrt e1) nb)))
					   do (push (cons (event-nbeams e1 ts) e1) lr)))
			    (loop for (nb . e) of-type ((integer 0) . noteex) in ll do (setf (event-beamlt e) nb))
			    (loop for (nb . e) of-type ((integer 0) . noteex) in lr do (setf (event-beamrt e) nb)))))
		   (loop
		    with dv = (let ((nb (timesig-nbeats ts)))
				(or (mapcar (lambda (x) (declare (type (rational (0)) x)) (* x nb)) (meas-div m)) (list nb)))
		    for i from 1 to (mloop for e in evs maximize (event-nbeams e ts))
		    collect (bm evs i (loop
				       for x of-type (rational (0)) in dv 
				       and d0 = (meas-off m) then d1
				       for d1 = (+ (meas-off m) x) then (+ d1 x)
				       nconc (beams-rules (loop for e of-type (or noteex restex) in evs
								when (and (> (event-endoff e) d0)
									  (< (event-off e) d1))
								collect e)
							  d0 (* x (timesig-beat* ts)) (/ 1/4 (expt 2 i)) ts)))
		    into ag finally
		    (loop for (f . b) of-type (cons . cons) in (nreverse ag) do (fb f b))
		    (fb (list evs) (list (reverse evs))))))
	       (let ((gg (mapcar (lambda (x) (declare (type cons x)) (sort x #'sort-offdur)) (split-into-groups grs #'event-off))))
		 (loop for gr of-type cons in gg
		       do (loop for (e1 e2) of-type ((or noteex restex) (or noteex restex null)) on gr #-clisp while #-clisp e2
				for nb = #-clisp (event-nbeams e1 ts) #+clisp (if e2 (event-nbeams e1 ts) (loop-finish))
				when (and (notep e1) (notep e2)) do (let ((x (min (event-nbeams e2 ts) nb))) (setf (event-beamrt e1) x (event-beamlt e2) x))))
		 (let ((ll nil) (lr nil)) ; fix beams that don't have enough
		   (loop for ee of-type cons in gg
			 do (loop for (e0 e1) of-type ((or noteex restex) (or noteex restex null)) on ee #-clisp while #-clisp e1
				  for nb = #-clisp (event-nbeams e1 ts) #+clisp (if e1 (event-nbeams e1 ts) (loop-finish))
				  when (and (notep e0) (notep e1) (> (event-nbeams e0 ts) 0)
					    (and (< (event-beamlt e1) nb) (< (event-beamrt e1) nb)))
				  do (push (cons nb e1) ll)))
		   (loop for ee of-type cons in gg
			 do (loop for (e1 e2) of-type ((or noteex restex) (or noteex restex null)) on ee #-clisp while #-clisp e2
				  for nb = #-clisp (event-nbeams e1 ts) #+clisp (if e2 (event-nbeams e1 ts) (loop-finish))
				  when (and (notep e1) (notep e2) (> (event-nbeams e2 ts) 0)
					    (and (< (event-beamlt e1) nb) (< (event-beamrt e1) nb)))
				  do (push (cons nb e1) lr)))
		   (loop for (nb . e) of-type ((integer 0) . noteex) in ll do (setf (event-beamlt e) nb))
		   (loop for (nb . e) of-type ((integer 0) . noteex) in lr do (setf (event-beamrt e) nb)))))
	     (setf (meas-events m) (sort (nconc grs evs) #'sort-offdur))
	     #+debug (loop for e in (meas-events m)
			   when (and (notep e) (or (> (event-beamlt e) 0) (> (event-beamrt e) 0))
				     (/= (max (event-beamlt e) (event-beamrt e)) (event-nbeams e (meas-timesig m))))
			   do (error "Error in BEAMS-STANDBYDIV (5)"))) (print-dot)))

(defun beams (parts)
  (declare (type list parts))
  (loop for p of-type partex in parts
	do (case (auto-beam-fun)
	     (:beams1 (beams-standbydiv (part-meas p)))
	     (otherwise (error "Unknown auto-beam module ~S" *auto-beam-module*)))))

