;; -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**************************************************************************************************
;; FOMUS
;; parts.lisp
;;**************************************************************************************************

(in-package :fomus)
(compile-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INITIALIZE PARTS

(declaim (type (or symbol list) *ensemble-type*))
(defparameter *ensemble-type* :orchestra)

(defun instr-groups ()
  (when *ensemble-type*
    (if (symbolp *ensemble-type*)
	(rest (or (assoc *ensemble-type* *instr-groups*)
		  (assoc *ensemble-type* +instr-groups+)
		  (error "Invalid ensemble type ~S" *ensemble-type*)))
	*ensemble-type*)))

(defun init-parts (timesigs parts)
  (declare (type list timesigs parts))
  (let ((h1 (make-hash-table :test 'eq))
	(h2 (make-hash-table :test 'eq)))
    (mapc (lambda (e) (declare (type partex e)) (setf (gethash e h1) (part-events e))) parts)
    (get-timesigs-aux timesigs parts
		      (lambda (p ts o1 o2)
			(declare (type partex p) (type timesig-repl ts) (type (rational 0) o1 o2))
			(push
			 (make-meas    ; measures are in reverse order
			  :timesig ts
			  :off o1
			  :endoff o2
			  :events (loop
				   for e on (gethash p h1)
				   for f = (car e)
				   until (>= (event-off f) o2)
				   collect f into re
				   finally
				   (setf (gethash p h1) e)
				   (return re)))
			 (gethash p h2))
			(print-dot)))
    (loop for e of-type partex in parts do (setf (part-events e) (nreverse (gethash e h2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GROUPING

(defun sort-parts (parts)
  (declare (type list parts))
  (labels ((fl (l)
	     (declare (type list l))
	     (loop for e of-type (or cons symbol) in l
		   if (consp e) nconc (fl (rest e)) else collect e)))
    (let ((l (fl (instr-groups))))
      (flet ((srt (x y)
	       (let ((px (position (instr-sym (part-instr x)) l))
		     (py (position (instr-sym (part-instr y)) l)))
		 (if (or (null px) (null py) (= px py))
		     (< (part-userord x) (part-userord y))
		     (< px py)))))
	(prog1 (sort parts #'srt) (print-dot))))))

(defun group-parts (pts)
  (declare (type list pts))
  (labels ((nu (in sp tv &optional i)
	     (declare (type symbol in) (type (cons symbol list) sp) (type boolean tv) (type (or (integer 0) null) i))
	     (loop
	      with fs = (unless (and tv (eq (the symbol (first sp)) :grandstaff)) (the symbol (first sp)))
	      for s of-type (or cons symbol) in (rest sp)
	      and j from 0
	      if (consp s)
	      do (let ((l (nu in s tv j)))
		   (when l (return (cons (cons i fs) l))))
	      else if (eq in s) do (return (list (cons i fs))))))
    (flet ((en (p l ty) 
	     (declare (type partex p) (type (integer 1) l) (type symbol ty))
	     (if (and (getprop p (list :startgroup l)) (not (eq ty :grandstaff)))	; eliminate 1-staff braces
		 (rmprop p (list :startgroup l))
		 (addprop p (list :endgroup l))))
	   (ad (p l ty)
	     (declare (type partex p) (type (integer 1) l) (type symbol ty))
	     (addprop p (list :startgroup l ty))))
      (loop
       for (lp p) of-type ((or part null) (or part null)) on (cons nil pts) and ii downfrom -1
       and l = g
       for g = (when p (or (rest (nu (instr-sym (part-instr p)) (cons nil (instr-groups)) (<= (instr-staves (part-instr p)) 1)))
			   (if (> (instr-staves (part-instr p)) 1)
			       (list (cons ii :grandstaff))
			       (list (cons ii nil)))))
       do
       (loop
	for ll on l and gg on g and i from 1
	while (equal (the (cons integer symbol) (first ll)) (the (cons integer symbol) (first gg)))
	do (let ((x (cdr (the (cons * symbol) (first ll))))) (when (eq x :grandstaff) (en lp i x) (ad p i x)))
	finally
	(loop
	 for l on ll and g on gg and j from i
	 do
	 (let ((x (cdr (the (cons * symbol) (first l))))) (en lp j x))
	 (let ((x (cdr (the (cons * symbol) (first g))))) (ad p j x))
	 finally
	 (loop
	  for ll on l and k from j
	  do (let ((x (cdr (the (cons * symbol) (first ll))))) (en lp k x)))
	 (loop
	  for gg on g and k from j
	  do (let ((x (cdr (the (cons * symbol) (first gg))))) (ad p k x)))))
       (print-dot))
      (let ((f (first pts))
	    (l (last-element pts)))
	(declare (type partex f l))
	(unless (and (getprop f '(:startgroup 1))
		     (notany (lambda (p) (declare (type partex p)) (getprop p '(:startgroup 1))) (rest pts))
		     (getprop l '(:endgroup 1)))
	  (addprop f '(:startgroup 0)) ; add a global group if there isn't one
	  (addprop l '(:endgroup 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DISTRIBUTE VOICES

(defun distr-voices (pts)
  (declare (type list pts))
  (loop with ad
	for p of-type partex in pts for d = (getprop p :distr)
	when d do
	(loop for e of-type (or noteex restex) in (part-events p)
	      for pp = (loop with v = (event-voice* e)
			     for (pa . li) of-type (symbol . list) in (rest (force-list d))
			     when (loop for l of-type (or (integer 1) list) in li and i from 1
					if (and (listp l) (= (first l) v))
					do (setf (event-voice* e) (second l)) (return t)
					else if (and (numberp l) (= l v))
					do (setf (event-voice* e) i) (return t))
			     do (return pa))
	      if pp do (push (cons (or (find pp pts :key #'part-partid) (error "No part with partid ~S in option :DISTR of part ~S" pp (part-partid p))) e) ad)
	      else collect e into ee
	      finally (setf (part-events p) ee))
	finally
	(loop with so for (p . e) of-type (partex . (or noteex restex)) in ad do (push e (part-events p)) (pushnew p so)
	      finally (loop for pp in so do (setf (part-events pp) (sort (part-events pp) #'sort-offdur))))))
	