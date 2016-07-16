;; -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**************************************************************************************************
;; FOMUS
;; quantize.lisp
;;**************************************************************************************************

(in-package :fomus)
(compile-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; QUANTIZE

(declaim (type symbol *auto-quantize-plugin* *auto-quantize-module*))
(defparameter *auto-quantize-plugin* nil)
(defparameter *auto-quantize-module* t)
(declaim (inline auto-quantize-fun))
(defun auto-quantize-fun () (if (truep *auto-quantize-module*) :quantize1-rmse *auto-quantize-module*))

(declaim (type boolean *auto-quantize*) 
	 (type integer *default-grace-num*))
(defparameter *auto-quantize* t)
(defparameter *default-grace-num* 0) 

(defun byfit-score-rmse (evpts qpts)
  (declare (type list evpts) (type list qpts))
  (sqrt (loop for e of-type (real 0) in evpts sum (let ((x (diff (loop-return-firstmin (diff i e) for i of-type (rational 0) in qpts) e))) (* x x)))))
(defun byfit-score-ave (evpts qpts)
  (declare (type list evpts) (type list qpts))
  (ave-list (loop for e of-type (real 0) in evpts collect (diff (loop-return-firstmin (diff i e) for i of-type (rational 0) in qpts) e))))

(defun quantize-byfit (timesigs parts scfun)
  (declare (type list timesigs parts) (type (function (t t) t) scfun))
  (let ((h (get-timesigs timesigs parts)))
    (flet ((adj (l)			; list?
	     (declare (type list l))
	     #+debug (check-order (delete-duplicates (mapcan #'copy-list (mapcar #'cdr l))) "QUANTIZE-BYFIT (1)" #'<)
	     #+debug (check-order (delete-duplicates (mapcan #'copy-list (mapcar #'car l))) "QUANTIZE-BYFIT (2)" #'<)
	     (loop for (e0 . e1) of-type (list . list) in l
		   append e0 into l0
		   nconc e1 into l1
		   finally (return (cons (remove-duplicates l0) (delete-duplicates l1)))))
	   (sel (l) ; select best (orig-point-list . quant-point-list) match
	     (declare (type list l))
	     (loop-return-argmin
	      (funcall scfun (car x) (cdr x))
	      for x of-type (cons list list) in (remove-if (lambda (x) (declare (type (cons list list) x)) (or (null (car x)) (null (cdr x)))) l))))
      (labels ((dv (o1 o2 pts rl lm) ; pts = user points
		 (declare (type (rational 0) o1 o2) (type (rational (0)) lm) (type baserule rl))
		 (let ((du (- o2 o1)))
		   (when (and pts (>= du lm))
		     (sel (cons (cons pts (list o1 o2))
				(unless (or (unit-nodiv-p rl) (sig-nodiv-p rl) (every (lambda (x) (declare (type (real 0) x)) (or (<= x o1) (>= x o2))) pts)) ; (or (<= x o1) (>= x o2))
				  (loop for (s . r) in (split-rules-bylevel
							rl (and (or (null *min-tuplet-dur*) (>= du *min-tuplet-dur*))
								(or (null *max-tuplet-dur*) (<= du *max-tuplet-dur*))))
					unless (some (lambda (x) (declare (type baserule x)) (or (unit-nodiv-p x) (sig-nodiv-p x))) r)
					collect (flet ((of (o) (declare (type (rational 0) o)) (+ o1 (* o du))))
						  (adj
						   (loop for (o1 o2) of-type ((rational 0 1) (or (rational 0 1) null)) on (cons 0 (append (force-list s) '(1)))
							 #-clisp while #-clisp o2
							 for oo1 = #-clisp (of o1) #+clisp (if o2 (of o1) (loop-finish))
							 #-clisp and #+clisp for oo2 = (of o2) and ru in r
							 for di = (dv oo1 oo2 (remove-if (lambda (e) (or (< e oo1) (> e oo2))) pts) ru lm)
							 when di collect di #|else do (return-from qua)|#)))))))))))
	(loop for p of-type partex in parts
	      for ph = (gethash p h)	; ph = timesigs for part
	      do (let* ((ee (sort (delete-duplicates (loop for e of-type (or noteex restex) in (part-events p) collect (event-off e) collect (event-endoff e))) #'<)) ; offset points
			(qs (sort
			     (delete-duplicates
			      (mapcan #'cdr
				      (loop with ep = ee
					    for (e n) of-type (timesig (or timesig null)) on (reverse ph) nconc
					    (loop with eo = (or (if n (timesig-off n) (last-element ep)) (return nil)) ; last offset
						  for o from (timesig-off e) below eo by (timesig-nbeats e)
						  do (loop until (or (null ep) (>= (first ep) o)) do (pop ep))
						  collect (let ((o2 (+ o (timesig-nbeats e)))) 
							    (dv o o2
								(loop for e in ep while (<= e o2) collect e)
								(first-splitrule e)
								(/ 3/4 (beat-division e))))
						  do (print-dot)))))
			     #'<)))
		   (loop with mg = (or (max-list (loop for e in (part-events p) when (event-grace e) collect (event-grace e)))
				       (1- *default-grace-num*))
			 and ad
			 for e of-type (or noteex restex) in (part-events p) do
			 (let ((o (event-off e)))
			   (loop while (and (list>1p qs) (< (second qs) o)) do (pop qs))
			   (let ((e1 (loop-return-firstmin (diff x o) for x of-type (rational 0) in qs))) 
			     (flet ((aa (oo ee)
				      (declare (type (real 0) oo) (type (rational 0) ee))
				      (cond ((< oo ee) (push (cons (cons #'>= oo) ee) ad)) ; -->
					    ((> oo ee) (push (cons (cons #'< oo) ; <--
								   (loop for (x1 x2) of-type ((rational 0) (or (rational 0) null)) on qs until (or (null x2) (>= x2 ee))
									 finally (return x1)))
							     ad)))))
			       (if (event-grace e)
				   (progn
				     (cond ((< (event-off e) e1) (push (cons (cons #'>= (event-off e)) e1) ad))	; -->
					   ((> (event-off e) e1) (push (cons (cons #'<= (event-off e)) e1) ad))) ; <--
				     (setf (event-off e) e1
					   (event-dur* e) (let ((bd (/ (beat-division (loop for s of-type timesig in ph until (<= (timesig-off s) e1) finally (return s))))))
							    (max bd (roundto (event-gracedur e) bd)))))
				   (let ((e2 (let ((o (event-endoff e))) (loop-return-lastmin (diff x o) for x of-type (rational 0) in qs))))
				     (aa (event-off e) e1)
				     (setf (event-off e) e1)
				     (let ((x (- e2 e1)))
				       (if (<= x 0)
					   (progn
					     (aa (event-endoff e) e1)
					     (setf (event-dur e)
						   (cons *default-grace-dur* (incf mg))))
					   (progn
					     (aa (event-endoff e) e2)
					     (setf (event-dur* e) x)))))))))
			 (print-dot)
			 finally
			 (addprop p (cons :quant ; temporary prop: collection of all movements to points
					  (merge-all ad (lambda (x y)
							  (declare (type (cons (cons (function ((rational 0) (rational 0)) boolean) (real 0)) (rational 0)) x y))
							  (let ((x1 (cdar x)) (x2 (cdr x))
								(y1 (cdar y)) (y2 (cdr y)))
							    (cond ((and (< x1 x2) (< y1 y2)) ; -->
								   (when (and (>= x2 y1) (>= y2 x1)) ; always #'>=
								     (cons (if (< x1 y1) (car x) (car y))
									   (max x2 y2))))
								  ((and (> x1 x2) (> y1 y2)) ; <-- 
								   (cond ((or (and (> x1 y2) (> y1 x2))	; overlap
									      (and (= x1 y2) (eq (caar x) #'<=)) ; touching
									      (and (= y1 x2) (eq (caar y) #'<=)))
									  (cons (cond ((= x1 y1) (cons (if (or (eq (caar x) #'<=)
													       (eq (caar y) #'<=))
													   #'<= #'<)
												       x1))
										      ((> x1 y1) (car x))
										      (t (car y)))
										(min x2 y2))))))))
						     :call-rev nil)))
			 (setf (part-events p) (sort (part-events p) #'sort-offdur)))))))))

(declaim (inline load-quantize-modules))
(defun load-quantize-modules ()
  (unless (member (auto-quantize-fun) '(:quantize1-rmse :quantize1-ave)) (load-fomus-module (auto-quantize-fun))))

(defun quantize (timesigs parts)
  (case (auto-quantize-fun)
    (:quantize1-rmse (quantize-byfit timesigs parts #'byfit-score-rmse))
    (:quantize1-ave (quantize-byfit timesigs parts #'byfit-score-ave))
    (otherwise (call-module (auto-quantize-fun) (list "Unknown quantize module ~S" *auto-quantize-module*) timesigs parts))))

(defun quantize-generic (parts)
  (loop for p in parts do
	(loop for e in (part-events p) do
	      (setf (event-dur* e) (rationalize (or (event-gracedur e) (event-dur* e))) (event-off e) (rationalize (event-off e))))))
