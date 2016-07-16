;; -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**************************************************************************************************
;; FOMUS
;; other.lisp
;;**************************************************************************************************

(in-package :fomus)
(compile-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type boolean *check-ranges*))
(defparameter *check-ranges* t)

(declaim (type (or null (real (0))) *input-beat-value*))
(defparameter *input-beat-value* nil)
(declaim (type (or null real) *input-offset*))
(defparameter *input-offset* nil)

;; must be before notes are transposed!
(defun check-ranges (pts)
  (declare (type list pts))
  (loop
   for p of-type partex in pts
   unless (is-percussion p)
   do (loop with i = (part-instr p)
	    for mm in (list (when (instr-minp i) (+ (instr-minp i) (or (instr-tpose i) 0))) (when (instr-maxp i) (+ (instr-maxp i) (or (instr-tpose i) 0))))
	    and co in (list #'< #'>) when mm do
	    (loop
	     for e of-type (or noteex restex) in (part-events p)
	     when (notep e)
	     do (let ((n (event-note* e)))
		  (when (funcall co n mm)
		    (format t "~&;; WARNING: Note ~S is out of range at offset ~S, part ~S" n (event-foff e) (part-name p))
		    (return))))) (print-dot)))

(defun check-useraccs (pts)
  (declare (type list pts))
  (loop for p of-type partex in pts
	unless (is-percussion p)
	do (loop with cha
		 for e of-type (or noteex restex) in (part-events p)
		 when (notep e) do (when (event-useracc e)
				     (loop with n = (event-note* e) and ch
					   for a of-type (or cons (integer -2 2)) in (event-useracc e)
					   if (if (and *quartertones* (consp a))
						  (qnotespelling n a)
						  (and (numberp a) (notespelling n a))) collect a into re else do (setf ch t cha t)
					   finally (when ch (setf (event-note e) (cons n re)))))
		 finally (when cha (format t "~&;; WARNING: Bad note spellings removed in part ~S" (part-name p))))
	(print-dot)))

(defun transpose (pts)
  (declare (type list pts))
  (loop for p of-type partex in pts
	unless (is-percussion p)
	do (let ((r (or (instr-tpose (part-instr p)) 0)))
	     (when r (loop for e of-type (or noteex restex) in (part-events p)
			   when (notep e) do
			   (if (event-useracc e)
			       (let* ((n (event-note* e))
				      (n2 (- n r)))
				 (setf (event-note e)
				       (cons n2
					     (delete-duplicates
					      (loop for a0 of-type (or cons (integer -2 2)) in (event-useracc e)
						    for a = (if (consp a0) (car a0) a0)
						    and aa = (when *quartertones* (if (consp a0) (cdr a0) 0))
						    nconc (if *quartertones*
							      (loop for (a2 . aa2) of-type ((integer -2 2) . (rational -1/2 1/2)) in
								    (mapcar #'convert-qtone +acc-qtones-double+)
								    when (and (qnotespelling n2 (cons a2 aa2))
									      (< (abs (nth-value 1 (interval (+ n aa) a (+ n2 aa2) a2))) 2))
								    collect (if (= aa2 0) a2 (cons a2 aa2)))
							      (loop for a2 of-type (integer -2 2) in +acc-double+
								    when (and (notespelling n2 a2) (< (abs (nth-value 1 (interval n a n2 a2))) 2))
								    collect a2)))
					      :test #'equal))))
			       (decf (event-note* e) r)))))
	(print-dot)))

(defun preproc-noteheads (parts)
  (declare (type list parts))
  (loop for p of-type partex in parts do
	(loop with so
	      for e of-type (or noteex restex) in (part-events p) do
	      (loop for (m a1) of-type (symbol symbol) = (popmark e :notehead) while m
		    collect (list m a1 (event-note* e)) into l
		    finally (mapc (lambda (x) (declare (type cons x)) (addmark e x)) l))
	      (loop with sy 
		    for (m a1 a2) of-type (symbol (or real symbol) (or real symbol)) = (popmark e :harmonic) while m do
		    (multiple-value-bind (ty n)
			(if (or (null a1) (find a1 '(:touched :sounding))) (values a1 a2) (values a2 a1))
		      (let ((ne (copy-event e :note (parse-usernote n)))
			    (gs (or sy (setf (event-acctie e) (setf sy (gensym))))))
			(setf (event-acctie ne) gs)
			(rmmark ne :notehead)
			(if (or (null ty) (eq ty :touched))
			    (addmark ne (list :harmonic :touched (event-note* ne)))
			    (addmark ne (list :harmonic :sounding (event-note* ne))))
			(push ne (part-events p))
			(setf so t))))
	      finally (when so (setf (part-events p) (sort (part-events p) #'sort-offdur))))
	(print-dot)))

(defun preproc-tremolos (timesigs parts)
  (declare (type list timesigs parts))
  (loop with ph = (get-timesigs timesigs parts)
	for p of-type partex in parts
	for tss = (gethash p ph) do
	(loop for e of-type (or noteex restex) in (part-events p)
	      for m = (or (popmark e :tremolo) (popmark e :tremolofirst) (popmark e :tremolosecond))
	      when m do (let* ((x (force-list m))
			       (n (second x))
			       (s (third x)))
			  (when (numberp s) (psetf n s s n))
			  (let ((r (if (eq s :notated)
				       (/ n (timesig-beat* (let ((o (event-off e))) (find-if (lambda (x) (declare (type timesig x)) (<= (timesig-off x) o)) tss))))
				       n)))
			    (addmark e (if (eq (first x) :tremolo) (list (first x) r) (list (first x) r (event-note* e)))))))
	(print-dot)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PERCUSSION

(declaim (type boolean *auto-percussion-durs*))
(defparameter *auto-percussion-durs* t)
  
(defun percussion (parts)
  (declare (type list parts))
  (loop for p of-type partex in parts
	when (is-percussion p) do
	(loop with pm = (instr-percs (part-instr p))
	      for ev of-type (or noteex restex) in (part-events p) do
	      (let ((n (event-note ev))) ; n = value of note slot
		(if (numberp n) (unless (svref +note-to-white+ (mod n 12))
				  (error "Invalid percussion note ~S in NOTE slot of note at offset ~S, part ~S" n (event-foff ev) (part-name p)))
		  (let ((c (etypecase n ; c = percussion struct
			     (symbol #|(find n *percussion* :key #'perc-sym)|# (find n pm :key #'perc-sym))
			     (perc n))))
		    (if c
			(progn 
			  (when (and (perc-staff c) (> (instr-staves (part-instr p)) 1))
			    (setf (event-staff* ev) (perc-staff c)))
			  (when (perc-voice c) (setf (event-voice* ev) (perc-voice c)))
			  (setf (event-note ev) (note-to-num (perc-note c)))
			  (addmark ev (list :percsym (note-to-num (perc-note c)) n))
			  (setf (event-marks ev) (append (perc-marks c) (event-marks ev)))
			  (when (and *auto-percussion-durs* (perc-autodur c) (not (event-grace ev))
				     (notany (lambda (x)
					       (declare (type symbol x))
					       (getmark ev x))
					     '(:tremolo :tremolofirst :tremolosecond :longtrill))) (addmark ev :autodur)))
			(if (is-note n) (setf (event-note ev) (note-to-num n))
			    (error "Unknown percussion specifier ~S in NOTE slot of note at offset ~S, part ~S" n (event-foff ev) (part-name p))))))))
	(print-dot)))

(defun autodurs-preproc (parts)
  (declare (type list parts))
  (loop for p of-type partex in parts do
	(loop with mg = (1+ (mloop for ev of-type (or noteex restex) in (part-events p) when (event-grace ev) maximize (event-grace ev)))
	      for ev of-type (or noteex restex) in (part-events p)
	      when (and *auto-percussion-durs* (getmark ev :autodur) (not (event-grace ev)))
	      do (setf (event-dur ev) (cons *default-grace-dur* mg)))
	(print-dot)))

;; voices separated
;; expands percussion :autodur durations
(defun autodurs (timesigs parts)
  (declare (type list parts))
  (loop with (mt . lb) = (let ((x 0) (x2 0))
			   (get-timesigs-aux timesigs parts
					     (lambda (p ts o1 o2)
					       (declare (ignore p o1) (type timesig-repl ts) (type (rational 0) o2))
					       (setf x (max x o2) x2 (timesig-nbeats ts))))
			   (cons x x2))
	for p of-type partex in parts do
	(loop with oo = mt
	      for ev of-type (or noteex restex) in (reverse (part-events p))
	      when (popmark ev :autodur)
	      do (setf (event-autodur ev) t (event-dur ev) (if (= oo (event-off ev)) lb (- oo (event-off ev))))
	      when (and #|(notep ev)|# (< (event-off ev) oo)) do (setf oo (event-off ev)))
	(print-dot)))

;; shift offsets/durations according to *input-beat-value* and *input-offset*
(defun fixinputbeat (parts tims mks)
  (declare (type list parts tims mks))
  (when (or *input-beat-value* *input-offset*)
    (let ((bv (or *input-beat-value* 1))
	  (io (or *input-offset* 0)))
      (loop for p of-type partex in parts do
	    (loop for e of-type (or noteex restex) in (part-events p) do
		  (setf (event-off e) (+ (/ (event-off e) bv) io))
		  (unless (event-grace e) (setf (event-dur* e) (/ (event-dur* e) bv)))))
      (loop for ti of-type timesig in tims do (setf (event-off ti) (+ (/ (event-off ti) bv) io)))
      (loop for m of-type mark in mks do (setf (event-off m) (+ (/ (event-off m) bv) io))))))

