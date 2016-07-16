;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-

;;; Copyright (c) 2006, Kilian Sprotte. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; this is currently a very preliminary version

;;; reference
;;; ftp://ftp.inria.fr/INRIA/Projects/contraintes/publications/ADAPTIVE/saga01.html


(deffomusmodule
    (:keyname :nokey2) (:type :accidentals) (:entryfun acc-nokey2)
    (:use :iterate) ; automatically uses common-lisp and fomus packages
    (:export #:make-int-var-from-to #:make-int-var-domain #:post #:ads #:example #:acc-nokey2)
    (:preload (asdf:operate 'asdf:load-op :iterate))
    (:documentation "(Experimental) Note-spelling algorithm by Kilian Sprotte using an adaptive search approach"))
  
;;; so SLIME knows...
(IN-PACKAGE "FOMUS-NOKEY2")

(setf *print-circle* t)	; for safety - not that I am really reading all this #1# stuff...

;;; var
(defstruct var
  domain constraints value (tabu 0)
  proj-cost		; caching the cost projection on this variable
  )

(defun var-set-randomly (var)
  (setf (var-value var)
	(nth (random (length (var-domain var))) (var-domain var))))

(defun var-compute-proj-cost (var)
  (iter
    (for c in (var-constraints var))
    (summing (constraint-cost c))))

(defun var-update-proj-cost (var)
  (setf (var-proj-cost var) (var-compute-proj-cost var)))

(defun make-int-var-from-to (from to)
  (make-var :domain (iter (for i from from to to) (collect i))))

(defun make-int-var-domain (domain)
  (make-var :domain domain))

;;; constraint
(defstruct constraint
  vars costfn
  cost				    ; caching the actual computed cost
  )

(defun constraint-compute-cost (constraint)
  (apply (constraint-costfn constraint)
	 (constraint-vars constraint)))

(defun constraint-update-cost (constraint)
  (setf (constraint-cost constraint) (constraint-compute-cost constraint)))

(defun post (form &rest vars)
  (flet ((var-syms (num)
	   (iter
	     (for i from 1 to num)
	     (collect (intern (format nil "V~A" i) "FOMUS-NOKEY2")))))
    (let* ((var-syms (var-syms (length vars)))
	   (c (make-constraint
	       :vars vars
	       :costfn (compile nil
				`(lambda (,@var-syms)
				   (let (,@(loop
					      for s in var-syms
					      collect `(,s (var-value ,s))))
				     ,form))))))
      (dolist (v vars)
	(push c (var-constraints v)))
      c)))

;;; ads
(defun collect-constraints (vars)
  (iter
    top
    (for v in vars)
    (iter
      (for c in (var-constraints v))
      (in top (adjoining c)))))

(defun print-info (vars global-cost)
  (terpri)
  (princ (list '***var-value
	       (iter
		 (for v in vars)
		 (collect (if (zerop (var-tabu v))
			      (format nil " ~a" (var-value v))
			      (format nil "^~a" (var-value v)))))
	       'global-cost
	       global-cost
	       'proj-cost
	       (mapcar #'var-proj-cost vars))))

(defun consistency-check (vars constraints global-cost)
  (let* ((constraints-costs (iter
			      (for c in constraints)
			      (collect (constraint-compute-cost c))))
	 (gc (apply #'+ constraints-costs))
	 (vars-projs (iter
		       (for var in vars)
		       (collect
			   (iter
			     (for c in (var-constraints var))
			     (for cost = (nth (position c constraints) constraints-costs))
			     (summing cost))))))
    (assert (= gc global-cost))
    (iter
      (for c in constraints)
      (for cost in constraints-costs)
      (assert (= (constraint-cost c) cost)))
    (iter
      (for var in vars)
      (for proj in vars-projs)
      (assert (= proj (var-proj-cost var))))))

(defun ads (vars &key
	    (tabu-tenure 3)
	    (max-iterations 20)		; later this will be 100000
	    (reset-limit (length vars)) ; number of tabu variables that will cause
	    (reset-percentage 100)) ; a percentage of those to be randomly reset
  (unless (= 100 reset-percentage)
    (warn "Sorry, assuming (= 100 reset-percentage)"))
  (let ((constraints (collect-constraints vars)))
    ;; initial random config
    (dolist (v vars) (var-set-randomly v))    
    (dolist (c constraints) (constraint-update-cost c))
    (dolist (v vars) (var-update-proj-cost v))
    ;; the big search loop
    (iter
      (with best-sol)
      (with best-cost = most-positive-fixnum)
      (with tabu-count = 0)
      (repeat max-iterations)
      (for global-cost = (iter
			   (for c in constraints)
			   (summing (constraint-cost c))))
      ;; (print-info vars global-cost)
      (consistency-check vars constraints global-cost) ; only for debugging
      ;; (print (list 'tabu-count tabu-count))
      ;;       (print (list 'var-proj-cost
      ;;		   (mapcar #'(lambda (x) (if (= x most-positive-fixnum) 'inf x))
      ;;			   (mapcar #'var-proj-cost vars))))
      ;;       (print (list 'var-tabu (mapcar #'var-tabu vars)))
      ;; when cost is 0 we have a perfect solution
      (when (zerop global-cost)
	(setf best-cost 0
	      best-sol (mapcar #'var-value vars))
	(terminate))
      ;; reset
      (when (<= reset-limit tabu-count)
	(when (< global-cost best-cost)
	  (setf best-cost global-cost
		best-sol (mapcar #'var-value vars)))
	(dolist (v vars)
	  (var-set-randomly v)
	  (setf (var-tabu v) 0))
	(setf tabu-count 0)
	(dolist (c constraints) (constraint-update-cost c))
	(dolist (v vars) (var-update-proj-cost v))
	(setq global-cost
	      (iter
		(for c in constraints)
		(summing (constraint-cost c))))
	;; (print '+resetting-vars)
	;; 	(print-info vars global-cost)
	(consistency-check vars constraints global-cost)) ; only for debugging
      ;; move
      (let ((worst-var (iter
			 (for v in vars)
			 (cond
			   ((< 1 (var-tabu v))
			    ;; v is tabu and cannot be moved
			    (decf (var-tabu v)))
			   (t
			    (when (= 1 (var-tabu v))
			      ;; v was tabu but will be reactivated
			      (decf (var-tabu v))
			      (decf tabu-count))
			    ;; (print (list 'looking 'at (var-value v)))
			    (finding v maximizing (var-proj-cost v)))))))
	(unless worst-var (error "no var is movable"))
	;; (print (list 'worst-var (var-value worst-var)))
	(iter
	  (with old-value = (var-value worst-var))
	  (for possible-move in (var-domain worst-var))
	  (setf (var-value worst-var) possible-move)
	  (dolist (c (var-constraints worst-var)) (constraint-update-cost c))
	  (for proj = (var-compute-proj-cost worst-var))
	  (finding (cons possible-move proj) minimizing proj into move-proj)
	  (finally (destructuring-bind (move . proj) move-proj
		     (cond
		       ;; found better move
		       ((< proj (var-proj-cost worst-var))
			(setf (var-value worst-var) move
			      (var-proj-cost worst-var) proj
			      ;; its tabu in the next iteration
			      (var-tabu worst-var) 2)
			(incf tabu-count)
			;; (print (list 'moving 'to m))
			)
		       ;; no better move
		       (t
			;; (print 'tabu)
			(setf (var-value worst-var) old-value
			      (var-tabu worst-var) tabu-tenure)
			(incf tabu-count))))
		   (iter
		     (for c in (var-constraints worst-var))
		     (constraint-update-cost c)
		     (dolist (v (constraint-vars c))
		       (adjoining v into dirty-vars))
		     (finally (mapc #'var-update-proj-cost dirty-vars))))))
      (finally (when best-sol
		 (iter
		   (for v in vars)
		   (for x in best-sol)
		   (setf (var-value v) x)))
	       (dolist (c constraints) (constraint-update-cost c))
	       (dolist (v vars) (var-update-proj-cost v))
	       (setq global-cost (iter
				   (for c in constraints)
				   (summing (constraint-cost c))))
	       (consistency-check vars constraints global-cost)
	       (return (values (mapcar #'var-value vars)
			       global-cost))))))

(defun example ()
  (let ((l (list (make-int-var-from-to 1 5)
		 (make-int-var-from-to 1 5)
		 (make-int-var-from-to 1 5))))
    ;; smallest distance as possible
    ;; between first and second
    (post
     '(abs (- v1 v2))
     (first l) (second l))
    ;; biggest distance as possible
    ;; between second and third
    (post
     '(max (- 10 (abs (- v1 v2))) 0)
     (second l) (third l))
    ;; the entire sum should
    ;; be greater than 10
    (post
     '(let ((sum (+ v1 v2 v3)))
       (if (< 10 sum)
	   0
	   (- 10 sum)))
     (first l) (second l) (third l))
    (ads l)))
;; the optimal sol is (5 5 1), 6
;; which is already reached sometimes

(defun cartesian-prod (list1 list2)
  (iter
    top
    (for e1 in list1)
    (iter
      (for e2 in list2)
      (in top (collect (list e1 e2))))))

(defun acc-nokey2 (evs)
  (let* ((midis (mapcar #'fm:event-note evs))
	 (domain-data (iter
			(for m in midis)
			(collect
			    (iter
			      (for acc in '(0 -1 1))
			      (for white-note = (fm::notespelling m acc))
			      (when white-note
				;; (list m white-note acc)
				(collect (list m white-note acc)))))))
	 (vars (iter
		 (for d in domain-data)
		 (collect (make-int-var-from-to 0 (1- (length d)))))))
    ;; (print domain-data)
    ;;     (print vars)
    ;; constraint ivs
    (iter
      (for a in vars)
      (for b in (cdr vars))
      (for adata in domain-data)
      (for bdata in (cdr domain-data))
      (for cart = (cartesian-prod (var-domain a) (var-domain b)))
      (for costs = (coerce
		    (iter
		      top
		      (for b-ind in (var-domain b))
		      (for bd = (nth b-ind bdata))
		      (iter
			(for a-ind in (var-domain a))
			(for ad = (nth a-ind adata))
			(for (values iv-value iv-quality) = (fm::interval
							     (first ad) (third ad)
							     (first bd) (third bd)))
			;; would be nice to do pattern-matching on (list iv-value iv-quality)
			(for cost = (case iv-quality
				      ((-2 2)
				       (if (= -2 iv-quality)
					   (case iv-value (4 0) (otherwise 1))
					   (case iv-value (3 0) (otherwise 1))))
				      ((-3 3) 20)
				      (otherwise 0)))
			(in top (collect cost))))
		    'vector))
      (post `(svref ,costs (+ v1 (* ,(length (var-domain a)) v2)))
	    a b))
    ;; constrain individual pitches
    (iter
      (for v in vars)
      (for data in domain-data)
      (for cost = (coerce
		   (iter
		     (for (m wn acc) in data)
		     (collect
			 ;; of course that is not the way
			 ;; to express this
			 ;; probably this data exists already
			 ;; in accidentals.lisp, just was not sure
			 ;; how to use it
			 (cond
			   ;; avoid b#
			   ((and (= wn 6)
				 (= acc 1))
			    5)
			   ;; cb
			   ((and (= wn 0)
				 (= acc -1))
			    5)
			   ;; fb
			   ((and (= wn 3)
				 (= acc -1))
			    5)
			   ;; etc
			   (t 0))))
		   'vector))
      (post  `(svref ,cost v1) v))
    (multiple-value-bind (inds cost)
	(ads vars
	     :tabu-tenure 5
	     :max-iterations 30000
	     :reset-limit 5)
      (print (list 'cost cost))      
      (iter	
	(for ind in inds)
	(for data in domain-data)
	(for entry = (nth ind data))
	(for ev in evs)
	(for proj in (mapcar #'var-proj-cost vars))
	(setf (fm:event-note ev) (cons (first entry) (third entry)))
	(unless (zerop proj)
	  (push `(:text ,(princ-to-string proj)) (fm:event-marks ev)))))
    evs))


