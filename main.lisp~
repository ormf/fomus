;; -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**************************************************************************************************
;; FOMUS
;; main.lisp
;;**************************************************************************************************

(in-package :fomus)
(compile-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROCESS NOTATION

(declaim (type (or null string) *debug-filename*))
(defparameter *debug-filename* (namestring (merge-pathnames "fomus.dbg" +tmp-path+)))

(declaim (type list *global* *timesigs* *parts* *events*))
(defparameter *global* nil)
(defparameter *timesigs* nil)
(defparameter *parts* nil)
(defparameter *events* nil)

(declaim (type list *chunks*))
(defparameter *chunks* nil)

#+debug
(defun fomus-proc-check (pts n)
  (loop
   for p in pts 
   if (measp (first (part-events p)))
   do (loop for m in (part-meas p) do (if (listp (first (meas-events m)))
					  (mapc (lambda (x) (check-order x (format nil "FOMUS-PROC-CHECK (1, ~A)" n) #'sort-offdur)) (meas-events m))
					  (check-order (meas-events m) (format nil "FOMUS-PROC-CHECK (2, ~A)" n) #'sort-offdur)))
   else do (check-order (part-events p) (format nil "FOMUS-PROC-CHECK (3, ~A)" n) #'sort-offdur)))
	  
(defun save-debug ()
  (when (>= *verbose* 2) (out "~&; Saving debug file ~S..." *debug-filename*))
  (unless (ignore-errors
	    (with-open-file (f *debug-filename* :direction :output :if-exists :supersede)
	      (format f ";; -*-lisp-*-~%;; ~A v~A.~A.~A~%;; ~A~%;; ~A~%~%(FOMUS~%"
		      +title+ (first +version+) (second +version+) (third +version+)
		      (lisp-implementation-type) (lisp-implementation-version))
	      (map nil (lambda (s)
			 (declare (type cons s))
			 (format f "  ~S ~S~&" (first s)
				 (let ((x (symbol-value (find-symbol (conc-strings "*" (symbol-name (first s)) "*") :fomus))))
				   (if (and (or (symbolp x) (listp x)) (not (or (null x) (truep x) (symbolp x))))
				       (list 'quote x)
				       x))))
		   +settings+)
	      (format f ")~%"))
	    t)
    (format t "~%;; WARNING: Couldn't create debug file ~S~%" *debug-filename*)))

(defparameter *indata-ignore*
  (nconc
   (mapcar #'first +deprecated-repl+)
   '(:global :parts :events :debug-filename
     :lilypond-exe :lilypond-opts :lilypond-out-ext :lilypond-view-exe :lilypond-view-opts
     :cmn-view-exe :cmn-view-opts)))
(defun save-indata (fn parts mks)
  (declare (type list parts))
  (when (>= *verbose* 1) (out "~&;; Saving input data file ~S..." fn))
  (setprints
    (let* ((*prepend-fm* t)
	   (pd (mapcar #'part-partid parts))
	   (*output* (remove-if (lambda (x) (member (first (force-list x)) '(:data :fomus))) (force-list2some *output*))))
      (with-open-file (f fn :direction :output :if-exists :supersede)
	(format f ";; -*-lisp-*-~%;; ~A v~A.~A.~A Input Data File~%~%"
		+title+ (first +version+) (second +version+) (third +version+))
	(map nil (lambda (s)
		   (declare (type cons s))
		   (unless (find (first s) *indata-ignore*)
		     (format f "INIT ~S ~A~%" (first s) (deuglify (symbol-value (find-symbol (conc-strings "*" (symbol-name (first s)) "*") :fomus))))))
	     +settings+)
	(format f "~%")
	(map nil (lambda (p0 id) (let ((p (or (gethash p0 *old-objects*) p0))) (format f "~A~%" (out-format (copy-part p :partid id))))) parts pd) 
	(format f "~%")
	(map nil (lambda (ob0)
		   (let ((ob (or (gethash (cdr ob0) *old-objects*) (cdr ob0))))
		     (format f "~A~%" (out-format (typecase ob
						    ((or note rest) (copy-event ob :partid (nth (car ob0) pd)))
						    (otherwise ob))))))
	     (sort (nconc
		    (mapcar (lambda (x) (cons -1 x)) *timesigs*)
		    (loop for p of-type partex in parts and n from 0 nconc (mapcar (lambda (x) (cons n x)) (part-events p)))
		    (let ((lp (length parts))) (mapcar (lambda (x) (cons lp x)) mks)))
		   (lambda (ob1 ob2)
		     (if (= (event-off (cdr ob1)) (event-off (cdr ob2)))
			 (if (= (car ob1) (car ob2))
			     (when (or (notep (cdr ob1)) (restp (cdr ob1)))
			       (sort-offdur (cdr ob1) (cdr ob2)))
			     (< (car ob1) (car ob2)))
			 (< (event-off (cdr ob1)) (event-off (cdr ob2)))))))
	(format f "~%")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHUNK STRUCTURE

(defstruct (fomuschunk (:copier nil) (:predicate fomuschunk))
  (settings nil :type list)
  (parts nil :type list))
(defmethod print-object ((x fomuschunk) s)
  (declare (type stream s))
  (print-unreadable-object (x s :type t :identity t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MERGE

;; backup info to possibly be restored later by chunkmerger
(defun backup-props (pts)
  (declare (type list pts))
  (loop with li
	for p of-type partex in pts do 
	(loop for m of-type meas in (part-meas p) do
	      (loop with i
		    for e of-type (or noteex restex) in (meas-events m) 
		    when (notep e) do (setf i t)
		    do (addmark e (cons :backup (event-marks e)))
		    finally (when i (pushnew (cons (meas-off m) (meas-endoff m)) li :test #'equal)))
	      (addprop m (cons :backup (meas-props m))))
	(addprop p (cons :backup (part-props p)))))

;; unbackup backuped props & marks
(defun unbackup-props (p)
  (declare (type partex p))
  (copy-part p :props (rest (getprop p :backup)) :events
	     (loop for m of-type meas in (part-meas p) collect
		   (copy-meas m :props (rest (getprop m :backup)) :events
			      (loop for v of-type list in (meas-events m) nconc
				    (loop for e of-type (or noteex restex) in v collect
					  (copy-event e :marks (rest (getmark e :backup)))))))))
  
(defun postproc-parts (pts)
  (declare (type list pts))		  
  (when (>= *verbose* 2) (out "~&; Post processing..."))
  (clean-clefs pts) #+debug (fomus-proc-check pts 'cleanclefs)
  (postaccs pts) #+debug (fomus-proc-check pts 'postaccs)
  (postproc pts) #+debug (fomus-proc-check pts 'postproc)
  (setf pts (sort-parts pts)) #+debug (fomus-proc-check pts 'sortparts)
  (group-parts pts) #+debug (fomus-proc-check pts 'groupparts)
  (postpostproc-sortprops pts) #+debug (fomus-proc-check pts 'sortprops)
  (when (>= *verbose* 1) (format t "~&"))
  pts)  

(defmacro set-fomusproc (&body forms)
  `(let ((*max-tuplet* (force-list *max-tuplet*))
	 (*old-objects* (make-hash-table)))
    ,@forms)) ; normalize some parameters
  
(defun fomus-merge ()
  (when (>= *verbose* 1) (out "~&; Merging chunks..."))
  (set-fomusproc
    (track-progress +progress-int+
      (let ((pts (loop with al and lo = (mloop for ch of-type fomuschunk in *chunks*
					       maximize (mloop for p of-type partex in (fomuschunk-parts ch) maximize (meas-endoff (last-element (part-meas p)))))
		       for (p1 . re) of-type (partex . list) on (mapcar #'unbackup-props (loop for ch of-type fomuschunk in *chunks* append (fomuschunk-parts ch)))
		       unless (find (part-partid p1) al) collect
		       (loop for p2 of-type partex in re 
			     when (eql (part-partid p1) (part-partid p2)) do
			     (setf (part-events p1)
				   (sort (delete-duplicates 
					  (stable-sort (nconc (copy-list (part-meas p1)) (copy-list (part-meas p2)))
						       (lambda (x y)
							 (declare (type meas x y))
							 (let ((xn (find-if #'notep (meas-events x)))
							       (yn (find-if #'notep (meas-events y))))
							   (when (and xn yn (> (meas-endoff x) (meas-off y)) (< (meas-off x) (meas-endoff y)))
							     (error "Overlapping/conflicting notation between chunks at offset ~S, part ~S"
								    (float (max (meas-off x) (meas-off y))) (part-name p1)))
							   (and xn (not yn)))))	; empty measures go to end
					  :from-end t
					  :test (lambda (m1 m2)
						  (declare (type meas m1 m2))
						  (and (> (meas-endoff m1) (meas-off m2)) (< (meas-off m1) (meas-endoff m2)))))
					 #'< :key #'meas-off))
			     finally
			     (flet ((nm (mo1 mo2 ts1)
				      (let ((ts (let* ((tt (* (timesig-beat* ts1) (- mo2 mo1)))
						       (nu (numerator tt))
						       (de (denominator tt)))
						  (loop while (< de tt) do (setf nu (* nu 2) de (* de 2)))
						  (copy-timesig ts1 :off mo1 :time (cons nu de) :div nil :props nil))))
					(multiple-value-bind (ev di) (split-engine-byscore (list (list (make-rest :off mo1 :dur (- mo2 mo1)))) mo1 mo2 ts)
					  (make-meas :timesig ts :off mo1 :endoff mo2 :events ev :props '(:measrest) :div di)))))
			       (loop for (m1 m2) of-type (meas (or meas null)) on (part-meas p1)
				     collect m1
				     when (and m2 (> (meas-off m2) (meas-endoff m1)))
				     collect (nm (meas-endoff m1) (meas-off m2) (meas-timesig m1)) into re
				     finally
				     (return (nconc re (loop with nb = (timesig-nbeats (meas-timesig m1))
							     for o from (meas-endoff m1) below lo by nb collect
							     (nm o (min (+ o nb) lo) (meas-timesig m1)))))))
			     (push (part-partid p1) al)
			     (return p1)))))
	;; prepostproc-parts (prepostproc preparation)
	(postproc-parts pts)	  ; this should also reorder the parts
	;; ...
	#|pts|#))))

;; keysigs not implemented yet
;; returns data structure ready for output via backends
(defun fomus-proc (svdata dir &aux (someout (find-if-not (lambda (x) (member (if (listp x) (first x) x) '(:fomus :data))) (force-list2some *output*))))
  (when (and someout (numberp *verbose*) (>= *verbose* 1)) (out "~&;; Formatting music..."))
  (when *debug-filename* (save-debug))
  (when (and (numberp *verbose*) (>= *verbose* 2)) (out "~&; Checking types..."))
  (check-setting-types)
  (check-settings)
  (load-quantize-modules)
  (load-split-modules)
  (load-acc-modules)
  (load-voices-modules)
  (load-staff/clef-modules)
  (set-fomusproc
    (set-instruments
      (set-note-precision
	(set-quality
	  (set-acc-modulevar
	   (multiple-value-bind (*timesigs* rm) (split-list *global* #'timesigp)
	     #-debug (declare (ignore rm))
	     #+debug (when rm (error "Error in FOMUS-PROC"))
	     (multiple-value-bind (*events* mks) (split-list *events* (lambda (x) (declare (type (or note rest mark) x)) (or (notep x) (restp x)))) 
	       (let ((pts (progn
			    (loop with co = 0
				  for p of-type part in *parts* and i from 0
				  do (multiple-value-bind (ti evs ma)
					 (split-list (part-events p) #'timesigp
						     (lambda (x) (declare (type (or note rest mark timesig) x)) (or (notep x) (restp x)))) ; separate timesigs/keysigs out of part tracks
				       (unless (part-partid p)
					 (setf (part-partid p)
					       (loop
						for s = (incf co)
						while (find s *parts* :key #'part-partid)
						finally (return s))))
				       (map nil (lambda (x)
						  (declare (type timesig x))
						  (unless (timesig-partids x)
						    (setf (timesig-partids x) (part-partid p))))
					    ti)
				       (map nil (lambda (x)
						  (declare (type mark x))
						  (unless (event-partid x)
						    (setf (event-partid x) (part-partid p))))
					    ma)
				       (prenconc ti *timesigs*)
				       (prenconc ma mks)
				       (multiple-value-bind (eo ep) (split-list evs #'event-partid)
					 (setf (part-events p) ep)
					 (prenconc eo *events*))))
			    (setf *timesigs* (mapcar #'make-timesigex* *timesigs*))
			    (loop
			     with h = (get-timesigs *timesigs* *parts*)
			     for i from 0 and e in *parts*
			     for (evs rm) of-type (list list) on (split-list* *events* (mapcar #'part-partid *parts*) :key #'event-partid)
			     collect (make-partex* e i evs (gethash e h))
			     finally (when rm (error "No matching part for event with partid ~S" (event-partid (first rm)))))))) ; make copied list of part-exs w/ sorted events 
		 #+debug (fomus-proc-check pts 'start)
		 (loop for e in svdata do
		       (destructuring-bind (&key (filename (change-filename *filename* :ext "fms")) &allow-other-keys)
			   (rest (force-list e))
			 (save-indata (namestring (merge-pathnames filename dir)) pts mks)))
		 (when someout
		   (setf *old-objects* nil)
		   (track-progress +progress-int+
		     (preproc-keysigs *timesigs*)
		     (fixinputbeat pts *timesigs* mks)
		     (when (find-if #'is-percussion pts)
		       (when (>= *verbose* 2) (out "~&; Percussion...")) ; before voices & clefs
		       (percussion pts)) ; was after accs
		     (autodurs-preproc pts)
		     (if *auto-quantize*
			 (progn (when (>= *verbose* 2) (out "~&; Quantizing..."))
				(quantize *timesigs* pts) #+debug (fomus-proc-check pts 'quantize))
			 (quantize-generic pts))
		     (when *check-ranges*
		       (when (>= *verbose* 2) (out "~&; Ranges..."))
		       (check-ranges pts) #+debug (fomus-proc-check pts 'ranges))	     
		     (preproc-noteheads pts) ; set acctie TEMPSLOT for accidentals and voicing algorithms
		     (check-mark-accs pts)
		     (check-useraccs pts)
		     (when *transpose*
		       (when (>= *verbose* 2) (out "~&; Transpositions..."))
		       (transpose pts) #+debug (fomus-proc-check pts 'transpose))
		     (if *auto-voicing*
			 (progn (when (>= *verbose* 2) (out "~&; Voices..."))
				(voices pts) #+debug (fomus-proc-check pts 'voices))
			 (voices-generic pts))
		     (distr-voices pts)
		     (if *auto-accidentals*
			 (progn (when (>= *verbose* 2) (out "~&; Accidentals..."))
				(accidentals pts *timesigs*) #+debug (fomus-proc-check pts 'accs))
			 (accidentals-generic pts))
		     (reset-tempslots pts nil)
		     (if *auto-staff/clef-changes*
			 (progn (when (>= *verbose* 2) (out "~&; Staves/clefs...")) ; staves/voices are now decided
				(clefs pts) #+debug (fomus-proc-check pts 'clefs))
			 (clefs-generic pts))
		     (reset-tempslots pts nil)
		     (distribute-marks pts mks)
		     (reset-tempslots pts nil)
		     (setf pts (sep-staves pts)) ; ********** STAVES SEPARATED
		     (when *auto-ottavas* ; (before clean-spanners)
		       (when (>= *verbose* 2) (out "~&; Ottavas..."))
		       (ottavas pts) #+debug (fomus-proc-check pts 'ottavas))
		     (when (>= *verbose* 2) (out "~&; Staff spanners..."))
		     (clean-spanners pts +marks-spanner-staves+) #+debug (fomus-proc-check pts 'spanners1)
		     (setf pts (sep-voices (assemble-parts pts))) ; ********** STAVES TOGETHER, VOICES SEPARATED
		     (when (>= *verbose* 2) (out "~&; Voice spanners..."))
		     (expand-marks pts) #+debug (fomus-proc-check pts 'expandmarks)
		     (clean-spanners pts +marks-spanner-voices+) #+debug (fomus-proc-check pts 'spanners2)
		     (when (>= *verbose* 2) (out "~&; Miscellaneous items..."))
		     (when (find-if #'is-percussion pts) (autodurs *timesigs* pts)) ;; uses beamrt (autodur) TEMPSLOT until after split function
		     (preproc-tremolos *timesigs* pts)
		     (preproc-cautaccs pts)
		     (when *auto-grace-slurs*
		       (grace-slurs pts) #+debug (fomus-proc-check pts 'graceslurs))
		     (when (>= *verbose* 2) (out "~&; Measures..."))
		     (init-parts *timesigs* pts) ; ----- MEASURES
		     #+debug (fomus-proc-check pts 'measures)
		     #+debug (check-same pts "FOMUS-PROC (MEASURES)" :key (lambda (x) (meas-endoff (last-element (part-meas x)))))
		     (when *auto-cautionary-accs*
		       (when (>= *verbose* 2) (out "~&; Cautionary accidentals..."))
		       (cautaccs pts) #+debug (fomus-proc-check pts 'cautaccs))
		     (when (>= *verbose* 2) (out "~&; Chords..."))
		     (marks-beforeafter pts)
		     (preproc-userties pts)
		     (preproc pts) #+debug (fomus-proc-check pts 'preproc) ; ----- CHORDS, RESTS
		     (clean-ties pts) #+debug (fomus-proc-check pts 'cleanties1)
		     (when (>= *verbose* 2) (out "~&; Splits/ties/rests..."))
		     (split pts) #+debug (fomus-proc-check pts 'ties)
		     (reset-tempslots pts 0)
		     (reset-resttempslots pts)
		     (clean-ties pts) #+debug (fomus-proc-check pts 'cleanties2)
		     (when *auto-beams*
		       (when (>= *verbose* 2) (out "~&; Beams..."))
		       (beams pts) #+debug (fomus-proc-check pts 'beams))
		     (when (>= *verbose* 2) (out "~&; Staff/voice layouts..."))
		     (setf pts (assemble-parts pts)) #+debug (fomus-proc-check pts 'assvoices) ; ********** VOICES TOGETHER
		     (distr-rests pts) #+debug (fomus-proc-check pts 'distrrests)
		     (when (or *auto-multivoice-rests* *auto-multivoice-notes*)
		       (comb-notes pts) #+debug (fomus-proc-check pts 'combnotes))
		     (backup-props pts)
		     (postproc-parts pts))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAIN

(defmacro resolve-deprecated (&body forms)
  `(progn
    ,@(loop for (d . r) of-type (symbol . symbol) in +deprecated-repl+
	    for ds = (find-symbol (format nil "*~A*" (symbol-name d)) :fomus)
	    and rs = (find-symbol (format nil "*~A*" (symbol-name r)) :fomus)
	    if r collect `(when ,ds (format t ";; WARNING: Setting ~S is deprecated--use ~S instead~%" (quote ,d) (quote ,r))
			   (setf ,rs ,ds ,ds nil))
	    else collect `(when ,ds (format t ";; WARNING: Setting ~S is deprecated" (quote ,d)) (setf ,ds nil)))
    ,@forms))

(defun fomus-main ()
  (resolve-deprecated
    (find-cm)
    (when (find :cmn (force-list2some *output*) :key (lambda (x) (first (force-list x)))) (find-cmn))
    (let ((dir #+cmu (ext:default-directory)
	       #+sbcl (conc-strings (sb-unix:posix-getcwd) "/")
	       #+clisp (ext:default-directory)
	       #+openmcl (ccl:mac-default-directory)
	       #+allegro (excl:current-directory)
	       #+lispworks (hcl:get-working-directory)))
      (let ((r (if *chunks*
		   (fomus-merge)
		   (fomus-proc (remove-if-not (lambda (x) (member x '(:data :fomus))) (force-list2some *output*) :key (lambda (x) (first (force-list x)))) dir))))
	(loop for x of-type (or symbol cons) in (force-list2some *output*)
	      do (let ((xx (force-list x)))
		   (destructuring-bind (ba &key filename process play view &allow-other-keys) xx
		     (declare (type symbol ba) (type boolean process view))
		     (backend ba
			      (namestring (merge-pathnames (or filename (change-filename *filename* :ext (lookup ba *backendexts*))) dir))
			      dir r (rest xx) (or process view) play view))))
	(if r
	    (make-fomuschunk
	     :settings (map nil (lambda (s)
				  (declare (type cons s))
				  (cons (first s) (symbol-value (find-symbol (conc-strings "*" (symbol-name (first s)) "*") :fomus))))
			    +settings+)
	     :parts r)
	    t)))))

