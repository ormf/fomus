;; -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**************************************************************************************************
;; FOMUS
;; interface.lisp
;;**************************************************************************************************

(in-package :fomus)
(compile-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERFACE SINGLE FUNCTION CALL

(defparameter *fomus-args* nil)

(defun run-fomus (&rest args &key allow-other-keys &allow-other-keys)
  (macrolet ((fma () (let ((n (mapcar (lambda (x) (declare (type cons x)) (intern (symbol-name (first x)) :fomus)) +settings+))
			   (v (mapcar (lambda (k) (declare (type cons k)) (find-symbol (conc-strings "*" (symbol-name (first k)) "*") :fomus)) +settings+)))
		       #+debug (when (position nil v) (error "Error in FOMUS")) 
		       `(destructuring-bind (&key ,@(mapcar (lambda (x y) (list x y)) n v) &allow-other-keys) args
			 (progv (quote ,v) (list ,@n)
			   (fomus-main)))))
	     (fm () (let ((n (mapcar (lambda (x) (declare (type cons x)) (intern (symbol-name (first x)) :fomus)) +settings+))
			  (v (mapcar (lambda (k) (declare (type cons k)) (find-symbol (conc-strings "*" (symbol-name (first k)) "*") :fomus)) +settings+)))
		      #+debug (when (position nil v) (error "Error in FOMUS")) 
		      `(destructuring-bind (&key ,@(mapcar (lambda (x y) (list x y)) n v) other-keys) args
			(declare (ignore other-keys))
			(progv (quote ,v) (list ,@n)
			  (fomus-main))))))
    (if allow-other-keys
	#+(or cmu sbcl) (muffwarn (fma)) #-(or cmu sbcl) (fma)
	#+(or cmu sbcl) (muffwarn (fm)) #-(or cmu sbcl) (fm))))

(defun fomus (&rest args)
  "Interface function/main entry point:
Runs FOMUS's algorithms on input data or file"
  (typecase (first args)
    ((or string pathname) (fomus-text (first args) (rest args) #'fomus-textexec))
    (list (let ((z (mapcar (lambda (x)
			   (typecase x
			     (fomuschunk x)
			     ((or string pathname) (fomus-text x (rest args) #'fomus-textexec))
			     (otherwise (error "Expected LIST of STRING or FOMUSCHUNK"))))
			 (first args))))
	    (apply #'run-fomus :chunks z (append (rest args) (loop for e of-type fomuschunk in z append (fomuschunk-settings e))))))
    (t (apply #'run-fomus args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERFACE MULTIPLE FUNCTION CALL

(declaim (type list *fomus-global* *fomus-parts* *fomus-events*))
(defparameter *fomus-global* nil)
(defparameter *fomus-parts* nil)
(defparameter *fomus-events* nil)

(defun fomus-init (&rest args)
  "Interface function:
Erases stored data and initializes FOMUS for FOMUS-EXEC, FOMUS-NEWTIMESIG,
FOMUS-NEWPART, FOMUS-NEWNOTE, FOMUS-NEWREST and FOMUS-NEWMARK functions"
  (progn (setf *fomus-args* args *fomus-global* nil *fomus-parts* nil *fomus-events* nil) t))

(defun fomus-newtimesig (&rest args)
  "Interface function:
Creates and stores a TIMESIG object"
  (let ((ts (apply #'make-instance 'timesig args)))
    (push ts *fomus-global*)
    t))
(defun fomus-newpart (partid &rest args)
  "Interface function:
Creates and stores a PART object"
  (declare (type (or symbol real) partid))
  (let ((pa (apply #'make-instance 'part :partid partid args)))
    (push pa *fomus-parts*)
    t))
(defun fomus-newnote (partid &rest args)
  "Interface function:
Creates and stores a NOTE object"
  (declare (type (or symbol real) partid))
  (let ((no (apply #'make-instance 'note :partid partid args)))
    (push no *fomus-events*)
    t))
(defun fomus-newrest (partid &rest args)
  "Interface function:
Creates and stores a REST object"
  (declare (type (or symbol real) partid))
  (let ((re (apply #'make-instance 'rest :partid partid args)))
    (push re *fomus-events*)
    t))
(defun fomus-newmark (partid &rest args)
  "Interface function:
Creates and stores a MARK object"
  (declare (type (or symbol real) partid))
  (let ((re (apply #'make-instance 'mark :partid partid args)))
    (push re *fomus-events*)
    t))

;;(declaim (inline fomus-part))
(defun fomus-part (sym)
  "Utility function:
Returns a PART object given an ID value"
  (declare (type (or symbol real) sym))
  (find sym *fomus-parts* :key #'part-partid))

;; should this function save additional objects for future calls?
(defun fomus-exec (&rest args)
  "Interface function/main entry point:
Runs FOMUS's algorithms on data specified previously with FOMUS-INIT,
FOMUS-NEWTIMESIG, FOMUS-NEWPART, FOMUS-NEWNOTE, FOMUS-NEWREST and FOMUS-NEWMARK"
  (unwind-protect
       (apply #'run-fomus
	      :global (append *global* *fomus-global*)
	      :parts (append *parts* (nreverse *fomus-parts*))
	      :events (append *events* *fomus-events*)
	      (append args *fomus-args*))
    (fomus-init)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEXT INTERFACE

(defun fomus-textexec (args) 
  (apply #'run-fomus
	 :global (append *global* *fomus-global*)
	 :parts (append *parts* (nreverse *fomus-parts*))
	 :events (append *events* *fomus-events*)
	 (nconc *fomus-args* args)))
(defun fomus-textret (args) 
  (values
   (append *parts* (nreverse *fomus-parts*))
   (append *events* *fomus-events*)
   (append *global* *fomus-global*)
   (nconc *fomus-args* args)))
(defun fomus-text (filename args exe)
  (let ((*fomus-args* args)
	(*fomus-global* nil)
	(*fomus-parts* nil)
	(*fomus-events* nil))
    (destructuring-bind (&key (verbose *verbose*) &allow-other-keys) args
      (when (and (numberp verbose) (>= verbose 1)) (out ";; Loading input file ~S...~%" filename)))
    (funcall exe
	     (let ((li 0) (lin "") (of nil))
	       (with-open-file (f filename :direction :input)
		 (handler-case
		     (flet ((git (rs rrs)
			      (unless (symbolp rs) (error "Invalid tag ~S" rs))
			      (when (numberp of)
				(let ((m (member :off rrs)))
				  (when (and m (numberp (second m))) (setf (second m) (+ of (second m))))))
			      (case (intern (symbol-name rs) :keyword)
				(:init (if (find (first rrs) +settings+ :key #'first) rrs (progn (format t ";; WARNING: Unknown setting ~A~%" (first rrs)) nil)))
				(:timesig (apply #'fomus-newtimesig rrs) nil)
				(:part (apply #'fomus-newpart rrs) nil)
				((:note :notes) (destructuring-bind (pa &rest args &key notes &allow-other-keys) rrs
						  (if notes
						      (let ((as (member :notes args )))
							(setf (first as) :note)
							(map nil (lambda (no) (setf (second as) no) (apply #'fomus-newnote pa args)) notes))
						      (apply #'fomus-newnote pa args)))
				 nil)
				(:rest (apply #'fomus-newrest rrs) nil)
				(:mark (apply #'fomus-newmark rrs) nil)
				(:off (setf of (first rrs)) nil)
				(otherwise (error "Invalid tag ~S" rs)))))
		       (loop
			with *package* = #.(find-package :fomus)
			for re = (progn (incf li) (setf lin "") (read f nil 'eof)) until (eq re 'eof)
			do (setf lin (format nil "~S ..." re))
			if (listp re) nconc (git (first re) (uglify (rest re)))
			else nconc (with-input-from-string (st (loop
								with st = (read-line f nil "")
								for s = (string-right-trim " " st)
								while (and (> (length s) 1) (char= (aref s (1- (length s))) #\\))
								do (setf st (conc-strings (subseq s 0 (1- (length s))) " " (read-line f))
									 lin (format nil "~S ~A ..." re st)) 
								finally (setf lin (format nil "~S ~A" re st)) (return st)))
				     (git re (loop for e = (read st nil 'eof) until (eq e 'eof) collect (uglify e))))))
		   (error (err)
		     (error "Entry ~D, ~S: ~A" li (remove-newlines lin) err))))))))

(defun fomus-file (filename &optional args)
  "Utility/file IO function:
Loads a \".fms\" file and returns (values parts events global args)"
  (fomus-text filename args #'fomus-textret))
