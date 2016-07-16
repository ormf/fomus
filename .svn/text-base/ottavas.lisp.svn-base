;; -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**************************************************************************************************
;; FOMUS
;; ottavas.lisp
;;**************************************************************************************************

(in-package :fomus)
(compile-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type boolean *auto-ottavas*))
(defparameter *auto-ottavas* t)

(declaim (type symbol *auto-ottavas-plugin* *auto-ottavas-module*))
(defparameter *auto-ottavas-plugin* nil)
(defparameter *auto-ottavas-module* t)
(declaim (inline auto-ottavas-fun))
(defun auto-ottavas-fun () (if (truep *auto-ottavas-module*) :ottavas1 *auto-ottavas-module*))

;; maximum number of beats of rest before new ottava must be started
(declaim (type (real (0)) *max-ottava-rest-dist*))
(defparameter *max-ottava-rest-dist* 3)

(defun ottavas-byleglines (instr events)
  (declare (type instr instr) (type list events))
  (when (instr-8uplegls instr)
    (loop
     with ub = (+ (notetowhite (lookup (loop-return-argmax (position c +clefs+ :key #'car) for c in (force-list (instr-clefs instr))) +clefs+)) 5)
     with u = (whitetonote (+ ub (* (car (instr-8uplegls instr)) 2)))
     and u0 = (whitetonote (+ ub (* (cdr (instr-8uplegls instr)) 2)))
     and uu and lo of-type (rational 0) = 0 and le 
     for (e en) of-type (noteex (or noteex null)) on (remove-if-not #'notep events)
     for fo = (popmark e :8up)
     when (and (null uu) (or fo (> (event-writtennote e) u))) do (setf uu t) (addmark e :start8up-)
     when (> (event-writtennote e) u) do (setf lo (max lo (event-endoff e)) le e) ; last offset, last event
     when (and uu (not fo)
	       (or (null en) ;; (>= (- (event-off e) lo) *max-ottava-rest-dist*)
		   (and (<= (event-writtennote e) u0) (>= (- (event-off e) lo) *max-ottava-rest-dist*)))) do (setf uu nil) (addmark le :end8up-)
     finally
     (when uu (addmark le :end8up-))))
  (when (instr-8dnlegls instr)
    (loop
     with lb = (- (notetowhite (lookup (loop-return-argmin (position c +clefs+ :key #'car) for c in (force-list (instr-clefs instr))) +clefs+)) 5)
     with l = (whitetonote (- lb (* (car (instr-8dnlegls instr)) 2)))
     and l0 = (whitetonote (- lb (* (cdr (instr-8dnlegls instr)) 2)))
     and ll and lo of-type (rational 0) = 0 and le 
     for (e en) of-type (noteex (or noteex null)) on (remove-if-not #'notep events)
     for fo = (popmark e :8down)
     when (and (null ll) (or fo (< (event-writtennote e) l))) do (setf ll t) (addmark e :start8down-)
     when (< (event-writtennote e) l) do (setf lo (max lo (event-endoff e)) le e)
     when (and ll (not fo)
	       (or (null en) ;; (>= (- (event-off e) lo) *max-ottava-rest-dist*)
		   (and (>= (event-writtennote e) l0) (>= (- (event-off e) lo) *max-ottava-rest-dist*)))) do (setf ll nil) (addmark le :end8down-)
     finally
     (when ll (addmark le :end8down-))))
  (print-dot))

(defun ottavas-rmmarks (events)
  (loop for e of-type (or noteex restex) in events
	do (loop for m of-type symbol in '(:8up :start8up- :8up- :end8up- :8down :start8down- :8down- :end8down-)
		 do (rmmark e m))))

(defun ottavas (parts)
  (loop
   for p of-type partex in parts
   if (is-percussion p) do (ottavas-rmmarks (part-events p))
   else do
   (get-usermarks (part-events p) :8up :start8up- :8up- :end8up- (lambda (e m) (declare (type (or noteex restex) e) (ignore m)) (addmark e :8up)) (part-name p))
   (get-usermarks (part-events p) :8down :start8down- :8down- :end8down- (lambda (e m) (declare (type (or noteex restex) e) (ignore m)) (addmark e :8down)) (part-name p))
   (case (auto-ottavas-fun)
     (:ottavas1 (ottavas-byleglines (part-instr p) (part-events p)))
     (otherwise (error "Unknown ottavas module ~S" *auto-ottavas-module*)))))

(defun ottavas-generic (parts)
  (loop for p of-type partex in parts when (is-percussion p) do (ottavas-rmmarks (part-events p))))