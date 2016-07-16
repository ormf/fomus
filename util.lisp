;; -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**************************************************************************************************
;; FOMUS
;; util.lisp
;;**************************************************************************************************

;; *****COMMENTING...*****

(in-package :fomus)
(compile-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEBUGGING

;; almost all functions expected everything to be sorted--this checks for that
#+debug
(defun check-same (list str &key (key #'identity) (test #'eql))
  (unless (let ((l (mapcar key list)))
	    (if l
		(let ((fl (first l)))
		  (every (lambda (x) (funcall test x fl)) (rest l)))
		t))
    (error "CHECK-SAME failed at ~A" str)))

#+debug
(defun check-order (list str fun)
  (loop for (e1 e2) on list while e2 unless (or (funcall fun e1 e2) (not (funcall fun e2 e1))) do (error "CHECK-ORDER failed at ~A" str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FIND GHOSTVIEW

;; set to postscript viewing application
(eval-when (:load-toplevel :execute)
  (defparameter +ghostview-exe+
    #+(or darwin macos) (find-exe "open")
    #+(and (or linux unix) (not (or darwin macos)) (not cygwin)) (or (find-exe "ggv") (find-exe "kgv") (find-exe "gv") (find-exe "evince") (find-exe "display") (find-exe "ghostview") "gv")
    #+(or mswindows win32 cygwin) (or (find-exe "gsview32.exe" "Ghostgum") (find-exe "gv.exe") "gsview.exe"))
  (defparameter +acroread-exe+
    #+(or darwin macos) (find-exe "open")
    #+(and (or linux unix) (not (or darwin macos)) (not cygwin)) (or (find-exe "acroread") (find-exe "gpdf") "acroread")
    #+(or mswindows win32 cygwin) (or (find-exe "AcroRd32.exe" "Adobe") "AcroRd32.exe")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROGRESS DOTS, IMMEDIATE OUTPUT

;; print a progress dot only if some certain time interval has elapsed since the last dot
(declaim (type (integer 0) +progress-int+))
(defparameter +progress-int+ 5) ; seconds
;; after good progress, reward with a dot!
(declaim (inline print-dot))
(defun print-dot () (when (>= *verbose* 1) (progress ".")))

;; wrapper for format--some lisps buffer the output so user can't see progress
(defun out (&rest args) (apply #'format t args) (finish-output))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WHITE NOTE/CHROM NOTE CONVERSION

(declaim (type (vector (or (integer 0 6) null)) +note-to-white+)
	 (type (vector integer) +white-to-note+))
(defparameter +note-to-white+ (vector 0 nil 1 nil 2 3 nil 4 nil 5 nil 6)) ; vector of white notes in chromatic scale
(defparameter +white-to-note+ (vector 0 2 4 5 7 9 11)) ; vector of chromatic notes given white note index (0 - 6)

(declaim (type cons +acc-single+ +acc-double+ +acc-qtones-single+ +acc-qtones-double+))
(defparameter +acc-single+ '(0 -1 1)) ; all possible modifications when only using single flats and sharps
(defparameter +acc-double+ '(0 -1 1 -2 2)) ; all possible modifications when using double flats and sharps
(defparameter +acc-qtones-single+ '(0 -1 1 (0 . -1/2) (0 . 1/2) (-1 . -1/2) (1 . 1/2))) ; acc-single + quartertones
(defparameter +acc-qtones-double+ '(0 -1 1 -2 2 (0 . -1/2) (0 . 1/2) (-1 . -1/2) (1 . 1/2))) ; acc-double + quartertones

;; convert note value (mod 12) into a white-note value (mod 7)
;; expects p to actually be a valid white note (error if it isn't)
(defun notetowhite (p)
  (declare (type integer p))
  (multiple-value-bind (o n) (floor p 12)
    (+ (* o 7) #-debug (svref +note-to-white+ n) #+debug (or (svref +note-to-white+ n) (error "Error in NOTETOWHITE")))))
;; convert write-note (mod 7) value into a note value (mod 12)
(defun whitetonote (w)
  (declare (type integer w))
  (multiple-value-bind (o n) (floor w 7)
    (+ (* o 12) (svref +white-to-note+ n))))

(declaim (type (vector boolean) +nokey-quality+))
(defparameter +interval-quality+ (vector nil t t nil nil t t)) ; given diatonic interval number as index, contains nil if it can be perfect or t if it's maj/min

;; return white note spelling (0 - 6) or nil if not possible
;; note = note value
;; acc = accidental modification (-2 to 2)
(defun notespelling (note acc)
  (declare (type rational note) (type (integer -2 2) acc))
  (multiple-value-bind (o n) (floor (- note acc) 12)
    (let ((x (svref +note-to-white+ n)))
      (when x (values x o)))))
;; acc = (-2 to 2) or (-1 . -1/2), etc.
(defun qnotespelling (note acc)
  (declare (type rational note) (type (cons (integer -2 2) (rational -1/2 1/2)) acc))
  (multiple-value-bind (o n) (floor (- note (car acc) (cdr acc)) 12)
    (let ((x (when (integerp n) (svref +note-to-white+ n))))
      (when x (values x o)))))

;; converts an accidental value or quartertone to a normalized quartertone format
;; x = (-2 to 2) or (-1 . -1/2), etc.
;; returns: (-2 . 0), (-1 . 0), (-1 . -1/2), etc.
(defun convert-qtone (x)
  (declare (type (or (cons (integer -2 2) (rational -1/2 1/2)) (integer -2 2)) x))
  (if (consp x) x (cons x 0)))

;; finds the diatonic interval between two note/accidental pairs
;; return values: interval number (0 - 6), interval quality (0 = perfect, -1/1 = min./maj., -2/2... = dim./aug., etc.)
(defun interval (note1 acc1 note2 acc2)
  (declare (type rational note1 note2) (type (integer -2 2) acc1 acc2))
  (multiple-value-bind (s1 o1) (notespelling note1 acc1)
    (multiple-value-bind (s2 o2) (notespelling note2 acc2)
      (multiple-value-bind (sp1 sp2 n1 n2)
	  (let ((p1 (+ s1 (* o1 7)))
		(p2 (+ s2 (* o2 7))))
	    (if (= p1 p2)
		(if (< note1 note2)
		    (values p1 p2 note1 note2)
		    (values p2 p1 note2 note1))
		(if (< p1 p2)
		    (values p1 p2 note1 note2)
		    (values p2 p1 note2 note1))))
	(let ((b (mod (- sp2 sp1) 7)))
	  (values b
		  (let ((x (- (- n2 n1) (svref +white-to-note+ b) (* (floor (- sp2 sp1) 7) 12))))
		    (if (svref +interval-quality+ b)
			(if (>= x 0) (1+ x) x) ; maj./min.
			(cond ((> x 0) (1+ x)) ; aug./dim.
			      ((< x 0) (1- x))
			      (t 0))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTILITY

;; returns: t if num is a power of 2 (ex. 1/4, 1/2, 1, 2, 8, etc.)
(defun expof2 (num)
  (declare (type rational num))
  (loop
   for n = (if (< num 1) (/ num) num) then (/ n 2)
   when (= n 1) do (return t)
   until (< n 1)))

;; returns: list of lowest common denominators (duplicates are removed)
;; examples: (lowmult 5) = (5), (lowmult 6) = (2 3), (lowmult 10) = (2 5), (lowmult 100) = (2 5)
(defun lowmult (n)
  (declare (type rational n))
  (loop
   for i from 2
   #-clisp while #-clisp (<= i n)
   for j = #-clisp (/ n i) #+clisp (if (<= i n) (/ n i) (loop-finish))
   when (integerp j)
   collect i
   and do (loop do (setf n j j (/ n i)) while (integerp j))))

;; returns: prime numbers up to ubound excluding 1
;; (defun primes2 (ubound)
;;   (declare (type (integer 2) ubound))
;;   (loop
;;    for i from 2 to ubound
;;    when (notany (lambda (e) (declare (type (integer 2) e)) (= (mod i e) 0)) pl)
;;    collect i into pl
;;    finally (return pl)))

;; returns: odd numbers up to ubound (with exception of 2)
(defun notdivby2s (ubound)
  (declare (type (integer 2) ubound))
  (cons 2 (loop for i from 3 to ubound unless (integerp (/ i 2)) collect i)))

;; given list of conses representing (possibly overlapping) segments/distances, find all the "holes" in between these segments
;; list = list of conses (start . end)
;; o1 = starting value
;; o2 = end (can be <= o1 if don't care)
;; returns: list of conses (the "holes")
(defun get-holes (list o1 o2)
  (declare (type list list) (type real o1 o2))
  (loop
   with o = o1
   for (e1 . e2) of-type (real . real) in list
   when (< o e1) collect (cons o e1) into r
   do (setf o e2)
   finally
   (return (if (< o o2) (nconc r (list (cons o o2))) r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROPERTIES/MARKS

;; use the following functions to access/set properties and marks
(declaim (inline addprop))
(defgeneric addprop (obj prop))
(defmethod addprop ((obj meas) prop)
  (declare (type (or symbol cons) prop))
  (push prop (meas-props obj)))
(defmethod addprop ((obj part) prop)
  (declare (type (or symbol cons) prop))
  (push prop (part-props obj)))
(defmethod addprop ((obj mark) prop)
  (declare (type (or symbol cons) prop))
  (push prop (event-marks obj)))
(defmethod addprop ((obj timesig-repl) prop)
  (declare (type (or symbol cons) prop))
  (push prop (timesig-props obj)))

(declaim (inline obj-props))
(defgeneric obj-props (obj))
(defmethod obj-props ((obj meas)) (meas-props obj))
(defmethod obj-props ((obj part)) (part-props obj))
(defmethod obj-props ((obj timesig-repl)) (timesig-props obj))
(defmethod obj-props ((obj mark)) (event-marks obj))
(defun getprop (obj propid)
  (declare (type (or meas part timesig-repl mark) obj) (type (or symbol cons) propid))
  (let* ((mi (force-list propid))
	 (li (length mi)))
    (find-if (lambda (x) (declare (type (or symbol cons) x)) (let ((xx (force-list x))) (and (>= (length xx) li) (every #'equal xx mi))))
	     (obj-props obj))))
(defun getprops (obj propid)
  (declare (type (or meas part timesig-repl mark) obj) (type (or symbol cons) propid))
  (let* ((mi (force-list propid))
	 (li (length mi)))
    (loop for x of-type (or symbol cons) in (obj-props obj) 
	  when (let ((xx (force-list x))) (and (>= (length xx) li) (every #'equal xx mi))) collect x)))

(defun rmprop-aux (pr propid)
  (declare (type list pr) (type (or symbol cons) propid))
  (let* ((mi (force-list propid))
	 (li (length mi)))
    (remove-if (lambda (x) (declare (type (or symbol cons) x)) (let ((xx (force-list x))) (and (>= (length xx) li) (every #'equal xx mi)))) pr)))
(declaim (inline rmprop))
(defgeneric rmprop (obj propid))
(defmethod rmprop ((obj meas) propid)
  (declare (type (or symbol cons) propid))
  (setf (meas-props obj) (rmprop-aux (meas-props obj) propid)))
(defmethod rmprop ((obj part) propid)
  (declare (type (or symbol cons) propid))
  (setf (part-props obj) (rmprop-aux (part-props obj) propid)))
(defmethod rmprop ((obj timesig-repl) propid)
  (declare (type (or symbol cons) propid))
  (setf (timesig-props obj) (rmprop-aux (timesig-props obj) propid)))
(defmethod rmprop ((obj mark) propid)
  (declare (type (or symbol cons) propid))
  (setf (event-marks obj) (rmprop-aux (event-marks obj) propid)))

(declaim (inline popprop-aux))
(defgeneric popprop-aux (obj props))
(defmethod popprop-aux ((obj meas) props)
  (declare (type list props))
  (setf (meas-props obj) props))
(defmethod popprop-aux ((obj part) props)
  (declare (type list props))
  (setf (part-props obj) props))
(defmethod popprop-aux ((obj timesig-repl) props)
  (declare (type list props))
  (setf (timesig-props obj) props))
(defmethod popprop-aux ((obj mark) props)
  (declare (type list props))
  (setf (event-marks obj) props))
(defun popprop (obj propid)
  (declare (type (or meas part timesig-repl mark) obj) (type (or symbol cons) propid))
  (let* ((mi (force-list propid))
	 (li (length mi))
	 (f (find-if (lambda (x) (declare (type (or symbol cons) x)) (let ((xx (force-list x))) (and (>= (length xx) li) (every #'equal xx mi))))
		     (obj-props obj))))
    (popprop-aux obj (remove f (obj-props obj) :test #'equal))
    f))

;; combine marks/properties in list of objects (measures, parts, timesigs, mark objects), removing duplicates
;; returns: the new marks list
(defun combprops (objlist)
  (declare (type list objlist))
  (remove-duplicates (loop for o of-type (or meas part timesig-repl mark) in objlist nconc (copy-list (obj-props o))) :test #'equal))

;; aliasa for note/rest objects
(declaim (inline addmark getmark getmarks rmmark combmarks popmark))
(defun addmark (&rest x) (apply #'addprop x))
(defun getmark (&rest x) (apply #'getprop x))
(defun getmarks (&rest x) (apply #'getprops x))
(defun rmmark (&rest x) (apply #'rmprop x))
(defun combmarks (&rest x) (apply #'combprops x))
(defun popmark (&rest x) (apply #'popprop x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EVENTS

;; ts = a timesig objects
;; returns: number of beats in a measure
(defun timesig-nbeats (ts)
  (declare (type timesig-repl ts))
  (/ (timesig-num ts) (* (timesig-den ts) (timesig-beat* ts)))) 

;; ts = a timesig object
;; returns: metrical divisions, determined by lookup or a function that returns a reasonable default
(defun timesig-div* (ts)
  (declare (type timesig-repl ts))
  (or (force-list2all (timesig-div ts))
      (let ((nb (timesig-nbeats ts)))
	(or (lookup nb *default-meas-divs*)
	    (lookup nb +default-meas-divs+)
	    (loop with d0 = (denominator nb)
		  for d = 2 then (* d 2) #-clisp until #-clisp (> d d0)
		  for x = #-clisp (let ((bb (* nb d)))
				    (or (lookup bb *default-meas-divs*)
					(lookup bb +default-meas-divs+)))
		  #+clisp (if (> d d0) (loop-finish)
			      (let ((bb (* nb d)))
				(or (lookup bb *default-meas-divs*)
				    (lookup bb +default-meas-divs+))))
		  when x do (return (loop for y of-type list in x collect (mapcar (lambda (z) (declare (type (rational 0) z)) (/ z d)) y))))))))

(declaim (type (rational (0)) *effective-grace-dur-mul*))
(defparameter *effective-grace-dur-mul* 1/2) ; setting--multiplier for effective duration of grace notes--use this in any algorithm that needs a small durational value for grace notes

(declaim (inline event-tupfrac))
;; ev = note/rest event
;; returns: duration/time multiplier for tuplet note/rest
(defun event-tupdurmult (ev) (declare (type ex-base ev)) (if (listp (car (event-tup ev))) (cdr (event-tup ev)) (event-tup ev)))
;; returns: the tuplet fraction (portion of the whole tuplet that this event takes up)
(defun event-tupfrac (ev) (declare (type ex-base ev)) (car (event-tup ev)))

;; dur = duration
;; dmu = time/duration multipliers (a list representing the multiplier of each nested level)
;; returns: actual duration taking tuplet time modification into account
(declaim (inline event-effectdur event-writtendur* event-writtennote))
(defun effectdur (dur dmu)
  (declare (type (rational (0)) dur) (type list dmu))
  (loop
   with d = dur
   for e of-type (rational (0)) in dmu
   do (setf d (* d e))
   finally (return d)))
;; dmu = dmu argument override, needed by some functions
;; returns: effective duration of an event (taking tuplets into account)
(defun event-effectdur (ev &optional (dmu t))
  (declare (type (or noteex restex) ev) (type (or boolean list) dmu))
  (effectdur (event-dur* ev) (if (truep dmu) (event-tupdurmult ev) dmu)))
;; returns: grace note duration multiplied by effective-grace-dur-mul setting
(defun event-graceeffdur (ev)
  (declare (type dur-base ev))
  (let ((gd (event-gracedur ev)))
    (when gd (* gd *effective-grace-dur-mul*))))
;; ev = note/rest event
;; ts = timesig object
;; dmu = dmu argument override, needed by some functions
;; returns: written duration (1/4 = quarter, 1/8 = eighth, 3/8 = dotted quarter, etc.)
(defun event-writtendur (ev ts &optional (dmu t))
  (declare (type (or noteex restex) ev) (type (or timesig-repl (rational (0))) ts) (type (or boolean list) dmu))
  (* (or (event-gracedur ev) (event-effectdur ev dmu))
     (if (timesigp ts) (timesig-beat* ts) ts)))
;; figures out if dots are needed
;; w = written duration (1/4 = quarter, 1/8 = eighth, 3/8 = dotted quarter, etc.)
;; return values: base written duration (1/4 = quarter, 1/8 = eighth, 3/8 = dotted quarter, etc.), number of dots
(defun writtendur* (w)
  (declare (type (rational (0)) w))
  (if (expof2 w) (values w 0)
      (let ((w1 (* w 2/3)))
	(if (expof2 w1) (values w1 1)
	    (let ((w2 (* w 4/7)))
	      (if (expof2 w2) (values w2 2)
		  #+debug (error "Error in WRITTENDUR*")))))))
;; ev = note/rest event
;; ts = timesig object
;; return values: base written duration (1/4 = quarter, 1/8 = eighth, 3/8 = dotted quarter, etc.), number of dots
(defun event-writtendur* (ev ts) ; returns values writtendur, number-of-dots
  (declare (type (or noteex restex) ev) (type timesig-repl ts))
  (writtendur* (event-writtendur ev ts)))
;; ev = note event
;; returns: notated note (ex.: g-natural, g-sharp and g-flat all return the note value for g-natural)
(defun event-writtennote (ev)
  (declare (type noteex ev))
  (- (event-note* ev) (event-acc ev) (event-addacc ev)))
;; ev = note event (containing a chord)
;; returns: list of notated notes in chord
(defun event-writtennotes (ev)
  (declare (type noteex ev))
  (loop
   for e of-type rational in (event-notes* ev)
   and a of-type rational in (event-accs ev)
   and a2 of-type rational in (event-addaccs ev)
   collect (- e a a2)))
;; ev = notes/rest event
;; ts = timesig object
;; ext = extra number of beams to subtract from actual value (useful for a few functions)
;; returns: number of beams belonging to note (rests have 0 beams)
(defun event-nbeams (ev ts &optional (ext 0))
  (declare (type (or noteex restex) ev) (type timesig-repl ts))
  (if (notep ev) (max (- (roundint (log (event-writtendur* ev ts) 1/2)) 2 ext) 0) 0))

;; dur = given duration of entire tuplet
;; dmu = time/duration multipliers (a list representing the multiplier of each nested level)
;; ts = timesig object
;; returns: unit of tuplet (1/8 = eighth note, etc.)
(defun unitwritdur (dur dmu ts) ; ndmu = the level that applies
  (declare (type (rational (0)) dur) (type list dmu) (type timesig-repl ts))
  (/ (* (effectdur dur dmu) (timesig-beat* ts)) ; written dur w/o dots info of entire tuplet
     (numerator (first dmu))))

;; ev = note/rest event
;; returns: whether or note event represents a chord
(declaim (inline chordp))
(defun chordp (ev)
  (declare (type (or noteex restex) ev))
  (and (notep ev) (consp (event-tielt ev))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SORTING

;; all-purpose sorter
;; sorts by (more or less): offset, gracep, voice num., staff num., duration, notep/restp, notes
;; pass to sort function
(defun sort-offdur (x y)
  (declare (type (or noteex restex) x y))
  (if (= (event-off x) (event-off y))
      (let ((gx (event-grace x))
	    (gy (event-grace y)))
	(if (eql gx gy)
	    (multiple-value-bind (sx vx) (let ((z (event-voice* x))) (if (numberp z) (values (event-staff x) z) (values 0 0))) 
	      (multiple-value-bind (sy vy) (let ((z (event-voice* y))) (if (numberp z) (values (event-staff y) z) (values 0 0)))
		(if (= vx vy)
		    (if (= sx sy)
			(let ((dx (or (event-gracedur x) (event-dur* x)))
			      (dy (or (event-gracedur y) (event-dur* y))))
			  (if (= dx dy)
			      (cond ((and (restp x) (notep y)) nil)
				    ((restp y) t)
				    (t (let ((nx (remove-if #'symbolp (if (chordp x) (event-notes* x) (list (event-note* x))))) ; at this point, should only be notes
					     (ny (remove-if #'symbolp (if (chordp y) (event-notes* y) (list (event-note* y))))))
					 (loop
					  for ex of-type real in nx and ey of-type real in ny
					  when (< ex ey) do (return t)
					  finally (return (< (length nx) (length ny)))))))
			      (< dx dy)))
			(< sx sy))
		    (< vx vy))))
	    (cond ((null gy) t)
		  ((null gx) nil)
		  (t (< gx gy)))))
      (< (event-off x) (event-off y))))

;; generic property list sorter
;; use to guarantee property lists/mark lists can be compared for equality w/ equal
;; props = property list
;; returns: sorted property/marks list
(defun sort-props (props)
  (declare (type list props))
  (labels ((xx (x y)
	     (declare (type (or symbol cons) x y))
	     (flet ((ty (a)
		      (declare (type (or symbol real string list) a))
		      (etypecase a (keyword 0) (symbol 1) (string 2) (integer 3) (rational 4) (float 5) (list 6))))
	       (loop for ee1 on (force-list x) and ee2 on (force-list y)
		     for e1 of-type (or symbol real string list) = (first ee1) and e2 of-type (or symbol real string list) = (first ee2)
		     for t1 = (ty e1) and t2 = (ty e2)
		     when (/= t1 t2) do (return (< t1 t2))
		     when (listp e1) do (return (xx e1 e2))
		     else when (if (numberp e1) (/= e1 e2) (string/= e1 e2))
		     do (return (if (numberp e1) (< e1 e2) (string< e1 e2)))
		     finally (< (length ee1) (length ee2))))))
    (sort (copy-list props) #'xx)))

;; alias for marks
(declaim (inline sort-marks))
(defun sort-marks (marks) (declare (type list marks)) (sort-props marks))

;; remove all marks that aren't specific things that appear in the score
;; marks = marks list
;; returns: another marks list
(declaim (inline important-marks))
(defun important-marks (marks)
  (declare (type list marks))
  (remove-if-not (lambda (x) (find (first (force-list x)) +marks-important+)) marks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHORDS/SPLITTING

;; accepts events of same offset/duration to assemble into a chord
;; list = sorted list of events of same offset/duration
;; rests are discarded, function is destructive on mark-lists--assumes throwing away note-events to make chord-event
;; returns: note event (containing multiple notes)
(defun make-chord (list &optional voice)
  (declare (type list list) (type (or null (integer 1)) voice))
  (multiple-value-bind (n r) (split-list (sort list #'sort-offdur) #'notep)
    (if n 
	(let ((x (sort (loop ; make more efficient?
			for e of-type noteex in n
			if (chordp e) append (event-note* e) into r1 and append (event-note e) into r2 and append (event-tielt e) into r3 and append (event-tiert e) into r4
			else collect (event-note* e) into r1 and collect (event-note e) into r2 and collect (event-tielt e) into r3 and collect (event-tiert e) into r4
			finally 
			(return (mapcar (lambda (y1 y2 y3 y4) (cons (cons y1 y2) (cons y3 y4))) r1 r2 r3 r4)))
		       #'< :key #'caar)))
	  (copy-event (first n)
		      :voice (or voice (event-voice (first n)))
		      :marks (combmarks n)
		      :note (mapcar #'cdar x) 
		      :tielt (mapcar #'cadr x)
		      :tiert (mapcar #'cddr x)))
	(copy-event (first r) :marks (combmarks r)))))

;; splits an event into two events (possibly tied), given an offset split point
;; event = note/rest event
;; off = split point
;; tup/dmu = a tuplet fraction/a tuplet duration multiplier to add (used by note-splitting function to create tuplets as necessary)
;; returns: cons of two events
(defun split-event (event off &optional tup dmu)
  (declare (type (or noteex restex) event) (type (rational 0) off) (type list tup dmu))
  (let ((eo (event-endoff event))
	(o (event-off event)))
    (cond ((<= eo off) (cons (copy-event event :tup (cons tup dmu)) nil))
	  ((<= off o) (cons nil (copy-event event)))
	  (t (etypecase event 
	       (note (cons (copy-event event
				       :dur (- off o) ; shouldn't be dealing with grace note
				       :tiert (if (chordp event) (make-list (length (event-tiert event)) :initial-element (not (event-autodur event)))
						  (not (event-autodur event)))
				       :tup (cons (when tup (cons (* (first tup) (/ (- off o) (- eo o))) (rest tup))) #|tup|# dmu))
			   (if (event-autodur event)
			       (make-restex nil :off off :dur (- eo off) :voice (event-voice event) :tup (event-tup event))
			       (copy-event event :off off :dur (- eo off)
					   :tielt (if (chordp event) (make-list (length (event-tielt event)) :initial-element t) t)))))
	       (rest (cons (copy-event event :dur (- off o) :tup (cons (when tup (cons (* (first tup) (/ (- off o) (- eo o))) (rest tup))) #|tup|# dmu)
				       :marks (if (event-marks event) (cons :splitlt (event-marks event))))
			   (copy-event event :off off :dur (- eo off)
				       :marks (if (event-marks event) (cons :splitrt (event-marks event)))))))))))

;; splits events--retains tuplet information from the original event
;; event = note/rest event
;; off = split point
;; returns: cons of two events
(defun split-event2 (event off)
  (declare (type noteex event) (type (rational 0) off))
  (let ((u (car (event-tup event)))
	(d (cdr (event-tup event))))
    (let ((re (split-event event off u d)))
      (setf (event-tup (cdr re)) (cons (cons (- (first u) (first (event-tupfrac (car re)))) (rest u)) d))
      re)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; USER MARKS

;; *****COMMENT POINT*****

;; passes all events between and intersecting with startsyms and endsyms
;; events sent to fun aren't in order
(defun get-usermarks (events sym startsym contsym endsym fun name) ; extrasort distinguishes between :staff 1, :staff 2, etc.
  (declare (type list events) (type symbol sym startsym contsym endsym) (type (function ((or noteex restex) list) t) fun) (type (or string null) name))
  (loop for ((o1 . me) (o2)) of-type (((rational 0) . list) ((rational 0)))
     on (mapcar #'cdr
		(let ((xx (merge-linear
			   (loop for e of-type cons in
				(sort (loop for e of-type (or noteex restex) in (reverse events)
					 for me = (let ((o1 (popmark e endsym)) (o2 (popmark e contsym))) (or o1 o2))
					 and ms = (popmark e startsym)
					 and my = (popmark e sym)
					 when me collect (cons :e (list (event-endoff e)))
					 when ms collect (cons :s (cons (event-off e) (force-list ms)))
					 when my collect (cons :o (cons (event-off e) (cons (event-endoff e) (force-list my)))))
				      (lambda (x y)
					(declare (type cons x y))
					(let ((xx (second x)) (yy (second y)))
					  (declare (type (rational 0) xx yy))
					  (if (= xx yy)
					      (< (ecase (car x) (:e 0) (:o 1) (:s 2)) (ecase (car y) (:e 0) (:o 1) (:s 2)))
					      (< xx yy)))))
			      if (eq (car e) :o) collect (cons :s (cons (cadr e) (cdddr e))) and collect (cons :e (cddr e))
			      else collect e)
			   (lambda (x y)
			     (declare (type cons x y))
			     (when (eq (car x) (car y))
			       (if (eq (car x) :s) x y))))))
		  (unless (or (null xx) (eq (car (first xx)) :s)) (error "Missing starting mark ~S in part ~S" startsym name))
		  xx))
     by #'cddr
     do (map nil (lambda (x) (declare (type (or noteex restex) x)) (funcall fun x (rest me)))
	     (remove-if-not (lambda (e) (declare (type (or noteex restex) e)) (and (> (event-endoff e) o1) (or (null o2) (< (event-off e) o2)))) events))))

;; clean
;; deletes marks at incorrect places in tied notes/chords
;; expects measures and chords
(defun clean-ties (pts)
  (declare (type list pts))
  (loop for p of-type partex in pts
	do (loop for m of-type meas in (part-meas p)
		 do (loop
		     for e of-type (or noteex restex) in (meas-events m)
		     when (and (notep e) (or (and (event-tielt e) (and-list (force-list (event-tielt e)))) (getmark e :endtremolo)))
		     do (mapc (lambda (x) (declare (type (or symbol cons) x)) (rmmark e x)) +marks-first-tie+) 
		     when (and (notep e) (or (and (event-tiert e) (and-list (force-list (event-tiert e)))) (getmark e :starttremolo)))
		     do (mapc (lambda (x) (declare (type (or symbol cons) x)) (rmmark e x)) +marks-last-tie+)
		     when (and (restp e) (popmark e :splitrt))
		     do (mapc (lambda (x) (declare (type symbol x)) (rmmark e x)) +marks-first-rest+)
		     when (and (restp e) (popmark e :splitlt))
		     do (mapc (lambda (x) (declare (type symbol x)) (rmmark e x)) +marks-last-rest+)
		     do (loop for sp in (list +marks-spanner-voices+ +marks-spanner-staves+) do
			      (loop for (startsym contsym endsym) of-type (symbol symbol symbol) in sp
				    do (loop for n in (getmarks e startsym) do (rmmark e (list contsym (second (force-list n)))))
				    do (loop for n in (getmarks e endsym) do (rmmark e (list contsym (second (force-list n)))))))))
	(print-dot)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STAVES

(defun sep-staves (parts)
  (declare (type list parts))
  (loop
   for p of-type partex in parts
   if (part-events p)
   nconc (mapcar
	  (lambda (e) (declare (type cons e)) (copy-part p :events (sort e #'sort-offdur)))
	  (split-into-groups (part-events p) #'event-staff))
   else collect p
   do (print-dot)))

;; separates before measures exist
(defun sep-voices (parts)
  (declare (type list parts))
  (loop
   for p of-type partex in parts
   if (part-events p)
   nconc (mapcar
	  (lambda (e) (declare (type cons e)) (copy-part p :events (sort e #'sort-offdur)))
	  (split-into-groups (part-events p) #'event-voice*))
   else collect p
   do (print-dot)))

;; reassembles after measures exist
(defun assemble-parts (parts)
  (declare (type list parts))
  (loop
   for p of-type cons in (split-into-groups parts #'part-userord)
   for f of-type partex = (first p)
   when (list>1p p)
   do (setf (part-events f)
	    (if (measp (first (part-events f)))
		(apply #'mapcar
		       (lambda (&rest ms)
			 (let ((m (first ms)))
			   (declare (type meas m))
			   (setf (meas-events m) (sort (mapcan #'meas-events ms) #'sort-offdur))
			   m)) ; same part, measures are copies of each other
		       (mapcar #'part-meas p))
		(sort (mapcan #'part-events p) #'sort-offdur))
	    (part-props f) (combprops p))
   collect f
   do (print-dot)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TIMESIGS

;; divs to use in a replacement time-signature
;; to is number of beats in measure
(defun repl-divs (divs to)
  (declare (type list divs) (type (rational (0)) to))
  (delete-duplicates
   (loop for e of-type cons in divs collect
	 (loop
	  with s = 0
	  for x of-type (rational (0)) in e
	  until (>= (+ s x) to)
	  collect x into xx
	  do (incf s x)
	  finally (return (nconc xx (list (- to s))))))
   :test #'equal))

;; minimum automatic measure (before timesig change)
;; in number of beats
(declaim (type (rational (0)) *min-auto-timesig-dur*)
	 (type boolean *auto-override-timesigs*))
(defparameter *min-auto-timesig-dur* 2)
(defparameter *auto-override-timesigs* t)

;; return sorted timesigs that match up with part
(declaim (type timesig-repl *default-timesig*))
(defparameter *default-timesig* (make-timesig-repl))

(defun get-timesigs-aux (timesigs parts fun)
  (declare (type list timesigs parts) (type (function (part timesig-repl (rational 0) (rational 0)) t) fun))
  (let ((mx 0)
	(mxo (mloop for p of-type part in parts maximize (mloop for e of-type dur-base in (part-events p) maximize (event-endoff e)))))
    (flet ((ut (si p eo1 lo)
	     (declare (type timesig si) (type part p) (type (or (rational 0) null) eo1 lo))
	     (loop
	      with eo0 = (when eo1 (- eo1 (or *min-auto-timesig-dur* 0))) ; eo0 = adjusted end-offset (for auto-meas)
	      for o = (or lo 0) then and
	      #-clisp while #-clisp (if eo1 (< o eo1) (or (<= o 0) (< o mxo))) ; loop creating measures
	      for nb = #-clisp (timesig-nbeats si) #+clisp (if (if eo1 (< o eo1) (or (<= o 0) (< o mxo))) (timesig-nbeats si) (loop-finish))
	      for nd = (+ o nb)	; nd = next downbeat, loop and create measures
	      for and = (if (and eo0 (> nd eo0)) eo1 nd) ; and = actual next downbeat (>= nd)
	      for at = (if (/= nb (- and o)) ; at = actual time-signature
			   (let ((x (find (- and o) (force-list (timesig-repl si)) :key #'timesig-nbeats)))
			     (if x (copy-timesig x
						 :off o :div (or (timesig-div x) (repl-divs (timesig-div si) (- and o)))
						 :parts (timesig-partids si) :repl nil :props nil)
				 (let ((n (* (/ (- and o) (timesig-nbeats si)) (timesig-num si))))
				   (copy-timesig si
						 :off o
						 :time (cons (numerator n) (* (denominator n) (timesig-den si)))
						 :div (repl-divs (timesig-div si) (- and o))
						 :repl nil :props nil))))
			   si)
	      when (> and mx) do (setf mx and)
	      do (funcall fun p at o and) ; part, timesig, o1, o2
	      finally (return (when and (cons and at)))))) 
      (loop with dts = (make-timesigex* *default-timesig*)
	    for p in parts
	    and (lo . at) in (loop
			      for p of-type part in parts
			      collect (loop
				       with at 
				       for (ts nx) of-type (timesig (or timesig null))
				       on (let ((z (let ((x (merge-linear
							     (sort (delete-if-not (lambda (x) (or (null (timesig-partids x)) (find (part-partid p) (timesig-partids x))))
										  (copy-list timesigs))	; ts = current time sig, n = next group
								   #'< :key #'timesig-off)
							     (lambda (x y) (if (= (timesig-off x) (timesig-off y))
									       (cond ((and (null (timesig-partids x)) (timesig-partids y)) y)
										     ((and (timesig-partids x) (null (timesig-partids y))) x)
										     (t (error "Conflicting time signature/part IDs assignment at offset ~S, part ~S"
											       (timesig-foff x) (part-name p)))))))))
						     (if (or (null x) (> (timesig-off (first x)) 0))
							 (cons (copy-timesig dts :off 0 :props nil) x)
							 x))))
					    (if *auto-override-timesigs*
						(loop for (e1 e2) of-type (timesig (or timesig null)) on z
						      when (or (<= (timesig-off e1) 0)
							       (null e2)
							       (>= (- (timesig-off e2) (timesig-off e1)) (or *min-auto-timesig-dur* 0)))
						      collect e1)
						z))
				       do (let ((x (ut ts p (when nx (timesig-off nx)) (car at))))
					    (when x (setf at x))) ; (print-dot)
				       finally (return at)))
	    do (ut at p mx lo) #|(print-dot)|#))))

;; return hash-table of reverse-order timesig lists (no repeats) indexed by part objects
(defun get-timesigs (timesigs parts)
  (declare (type list timesigs parts))
  (let ((h (make-hash-table :test 'eq)))
    (get-timesigs-aux timesigs parts
		      (lambda (p ts o1 o2)
			(declare (type part p) (type timesig ts) (ignore o1 o2))
			(let ((i (gethash p h)))
			  (declare (type list i))
			  (unless (eq ts (first i))
			    (setf (gethash p h) (cons ts i))))))
    h))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISCELLANEOUS

(declaim (inline is-percussion))
(defun is-percussion (part)
  (declare (type partex part))
  (or (eq (instr-sym (part-instr part)) :percussion)
      (eq (instr-clefs (part-instr part)) :percussion)))

(defun get-dur (ev ts)
  (declare (type dur-base ev) (type timesig-repl ts))
  (let ((g (when (consp (event-dur ev)) (second (event-dur ev)))))
    (if g
	(cons (dur-to-num (first (event-dur ev)) (timesig-beat ts)) g)
	(dur-to-num (event-dur ev) (timesig-beat ts)))))

(defun beat-division (ts)
  (declare (type timesig-repl ts))
  (if (listp *beat-division*)
      (if (timesig-comp ts)
	  (second *beat-division*)
	  (first *beat-division*))
      (if (timesig-comp ts)
	  (* *beat-division* 3/2)
	  *beat-division*)))

(defun tuplet-division (tup)
  (declare (type (integer 2) tup))
  (or (lookup tup *default-tuplet-divs* #|:test #'equal|#)
      (lookup tup +default-tuplet-divs+ #|:test #'equal|#)
      (delete-duplicates
       (loop for i in '(2 1) thereis
	     (loop for tu from i to (ceiling tup 2)
		   when (expof2 tu) collect (list tu (- tup tu)) and collect (list (- tup tu) tu)))
       :test #'equal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHECK SETTINGS

(defun check-setting-types ()
  (loop for (sy ty er) in +settings+ do
	(let ((v (symbol-value (find-symbol (conc-strings "*" (symbol-name sy) "*") :fomus))))
	  (or (check-type* v ty) (error "Found ~S, expected ~A in setting ~S" v (or er ty) sy)))))

(defun check-settings ()
  (loop for d of-type cons in *default-meas-divs* do
	(loop with n of-type (rational (0)) = (first d)
	      for r of-type cons in (rest d) unless (= (apply #'+ r) n) do (error "Invalid divisions ~S in setting :DEFAULT-MEAS-DIVS" d)))
  (loop for d of-type cons in *default-tuplet-divs* do
	(loop with n of-type (integer 1) = (first d)
	      for r of-type cons in (rest d) unless (= (apply #'+ r) n) do (error "Invalid divisions ~S in setting :DEFAULT-TUPLET-DIVS" d)))
  (unless (and (if (listp *default-beat*) (and (expof2 (car *default-beat*)) (expof2 (* (cdr *default-beat*) 2/3))) (expof2 *default-beat*)))
    (error "Invalid value ~S in setting :DEFAULT-BEAT" *default-beat*))
  (when (< *min-tuplet-dur* (/ *beat-division*))
    (format t "~&;; WARNING: Value ~S of setting :MIN-TUPLET-DUR is too small for beat-division of ~S--changing to ~S"
	    *min-tuplet-dur* *beat-division* (setf *min-tuplet-dur* (/ *beat-division*))))
  (when (< *max-tuplet-dur* *min-tuplet-dur*)
    (format t "~&;; WARNING: Value ~S of setting :MAX-TUPLET-DUR is smaller than value of setting :MIN-TUPLET-DUR--changing to ~S"
	    *max-tuplet-dur* (setf *max-tuplet-dur* *min-tuplet-dur*))))

(defmacro set-instruments (&body forms)
  `(let ((*instruments*
	  (loop for e of-type (or instr cons) in *instruments*
		if (consp e) collect (apply #'copy-instr (find (first e) +instruments+ :key #'instr-sym) (rest e))
		else collect e))
	 (*percussion*
	  (loop for e of-type (or perc cons) in *percussion*
		if (consp e) collect (apply #'copy-perc (find (first e) +percussion+ :key #'perc-sym) (rest e))
		else collect e)))
    ,@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERNAL OBJECT CONSTRUCTORS

;; make functions
(declaim (inline make-eventex*))
(defgeneric make-eventex* (ev ts pa))
(defmethod make-eventex* ((ev note) ts pa)
  (declare (type timesig-repl ts) (type partex pa))
  (make-noteex ev
   :id (obj-id ev) :partid (event-partid ev)
   :off (event-off ev)
   :dur (get-dur ev ts)
   :marks (event-marks ev)
   :voice (event-voice ev)
   :note (if (is-percussion pa) (event-note ev)
	     (parse-usernote (event-note ev)))))
(defmethod make-eventex* ((ev rest) ts pa)
  (declare (type timesig-repl ts) (ignore pa))
  (make-restex ev
   :id (obj-id ev) :partid (event-partid ev)
   :off (event-off ev)
   :dur (get-dur ev ts)
   :marks (event-marks ev)
   :voice (event-voice ev)))		; rest

;; ts is reverse list of timesigs for this part
(defun make-partex* (part userord evs ts) ; destroys evs
  (declare (type part part) (type (integer 0) userord) (type list evs ts))
  (let ((pp (make-partex part
	     :name (part-name part)	; part
	     :abbrev (part-abbrev part)
	     :opts (part-opts part)
	     :instr (flet ((er (s) (error "Invalid instrument ~S in part ~S" s (part-name part))))
		      (flet ((gi (s)
			       (declare (type (or symbol (integer 0 127)) s))
			       (if (symbolp s)
				   (or (find s *instruments* :key #'instr-sym)
				       (find s +instruments+ :key #'instr-sym)
				       (er s))
				   (or (find s *instruments* :test (lambda (k i)
								     (declare (type (integer 0 127) k) (type instr i))
								     (find k (force-list (instr-midiprgch-im i)))))
				       (find s +instruments+ :test (lambda (k i)
								     (declare (type (integer 0 127) k) (type instr i))
								     (find k (force-list (instr-midiprgch-im i)))))
				       (er s)))))
			(make-instrex*
			 (typecase (part-instr part)
			   (null *default-instr*)
			   (instr (part-instr part))
			   ((or symbol number) (gi (part-instr part))) 
			   (list (let ((z (apply #'copy-instr (gi (first (part-instr part))) (rest (part-instr part)))))
				   (check-type* z +instr-type+)
				   z))
			   (otherwise (er (part-instr part))))
			 part)))
	     :props (part-props part)
	     :partid (part-partid part) :userord userord)))
    (setf (part-events pp)
	  (sort (mapcar (lambda (e) (declare (type (or note rest) e))
				(make-eventex* e (loop for s of-type timesig in ts until (<= (timesig-off s) (event-off e)) finally (return s)) pp))
			(nconc evs (part-events part)))
		#'sort-offdur))
    pp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TIMESIG FUNCTIONS

(declaim (type boolean *auto-timesig-comp*))
(defparameter *auto-timesig-comp* t)

(defun timesig-check (ts)
  (declare (type timesig-repl ts))
  (flet ((er () (error "Invalid beat ~S in BEAT slot of TIMESIG at offset ~S" (timesig-beat ts) (timesig-foff ts))))
    (when (timesig-beat ts)
      (if (timesig-comp ts)
	  (unless (= (timesig-beat ts) (/ 3 (timesig-den ts))) (er))
 	  (unless (or (= (timesig-beat ts) 0) (expof2 (timesig-beat ts))) (er)))))
  (loop with nb = (timesig-nbeats ts)
	for d of-type cons in (timesig-div ts)
	unless (= (apply #'+ d) nb) do (error "Invalid division ~S in DIV slot of TIMESIG at offset ~S" (timesig-div ts) (timesig-foff ts))))
  
(defgeneric make-timesigex* (ts))
(defmethod make-timesigex* ((ts timesig))
  (let ((nt (copy-timesig ts
			  :comp (if (eq (timesig-comp ts) 'default)
				    (when *auto-timesig-comp*
				      (let ((x (/ (car (timesig-time ts)) 3)))
					(when (and (integerp x) (> x 1)) t)))
				    (timesig-comp ts))
			  :partids (force-list (timesig-partids ts))
			  :off (roundto (timesig-off ts) (/ (beat-division ts)))
			  :div (force-list2all (timesig-div ts))
			  :time (cons (first (timesig-time ts)) (second (timesig-time ts)))
			  :repl (let ((x (mapcar #'make-timesigex* (force-list (timesig-repl ts)))))
				  (if (list1p x) (first x) x)))))
    (timesig-check nt)
    (when *old-objects* (setf (gethash nt *old-objects*) ts))
    nt))
(defmethod make-timesigex* ((ts timesig-repl))
  (let ((nt (copy-timesig-repl ts
			       :comp (if (eq (timesig-comp ts) 'default)
					 (when *auto-timesig-comp*
					   (let ((x (/ (car (timesig-time ts)) 3)))
					     (when (and (integerp x) (> x 1)) t)))
					 (timesig-comp ts))
			       :div (force-list2all (timesig-div ts))
			       :time (cons (first (timesig-time ts)) (second (timesig-time ts)))
			       :props (copy-tree (timesig-props ts)))))
    (timesig-check nt)
    nt))

(defun keysig-accs (li)
  (declare (type list li))
  (loop for (a . b) of-type ((integer 0 11) . (integer -1 1))
	in (loop for e of-type symbol in li append
		 (ecase e
		   ((:cmaj :amin) nil)
		   ((:gmaj :emin) '((6 . 1)))
		   ((:dmaj :bmin) '((6 . 1) (1 . 1)))
		   ((:amaj :f+min) '((6 . 1) (1 . 1) (8 . 1)))
		   ((:emaj :c+min) '((6 . 1) (1 . 1) (8 . 1) (3 . 1)))
		   ((:bmaj :g+min) '((6 . 1) (1 . 1) (8 . 1) (3 . 1) (10 . 1)))
		   ((:f+maj :d+min) '((6 . 1) (1 . 1) (8 . 1) (3 . 1) (10 . 1) (5 . 1)))
		   ((:c+maj :a+min) '((6 . 1) (1 . 1) (8 . 1) (3 . 1) (10 . 1) (5 . 1) (0 . 1)))
		   ((:c-maj :a-min) '((10 . -1) (3 . -1) (8 . -1) (1 . -1) (6 . -1) (11 . -1) (4 . -1)))
		   ((:g-maj :e-min) '((10 . -1) (3 . -1) (8 . -1) (1 . -1) (6 . -1) (11 . -1)))
		   ((:d-maj :b-min) '((10 . -1) (3 . -1) (8 . -1) (1 . -1) (6 . -1)))
		   ((:a-maj :fmin) '((10 . -1) (3 . -1) (8 . -1) (1 . -1)))
		   ((:e-maj :cmin) '((10 . -1) (3 . -1) (8 . -1)))
		   ((:b-maj :gmin) '((10 . -1) (3 . -1)))
		   ((:fmaj :dmin) '((10 . -1)))
		   (:c+ '((1 . 1))) (:d- '((1 . -1)))
		   (:d+ '((3 . 1))) (:e- '((3 . -1)))
		   (:f+ '((6 . 1))) (:g- '((6 . -1)))
		   (:g+ '((8 . 1))) (:a- '((8 . -1)))
		   (:a+ '((10 . 1))) (:b- '((10 . -1)))))
	nconc (loop for i from 0 below 128 by 12 collect (cons (+ i a) b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; USER UTILITIES

(defun list-fomus-settings ()
  "Info function:
Lists all of FOMUS's current default settings and values"
  (let* ((tc (+ 2 (max (1+ (mloop for x in +settings+ maximize (length (symbol-name (first x))))) 4)))
	 (tl (+ tc 1 (max (mloop for (xxx t1 t2) in +settings+ maximize (length (or t2 (princ-to-string t1)))) 4))))
    (format t "; NAME~VTTYPE~VTDEFAULT VALUE~%~%" tc tl)
    (loop for (sy t1 t2) in +settings+ unless (find sy +deprecated-repl+ :key #'car) do
	  (format t "; ~A~VT~A~VT~A~%" sy tc (or t2 t1) tl (remove-newlines (prin1-to-string (symbol-value (find-symbol (conc-strings "*" (symbol-name sy) "*") :fomus))))))))
    
(defun list-fomus-instruments ()
  "Info function:
Lists all of FOMUS's instruments and their definitions"
  (set-instruments
    (loop with li = (remove-duplicates (append *instruments* +instruments+) :key #'instr-sym :from-end t)
	  with c = (+ (mloop for e in li maximize (length (symbol-name (instr-sym e)))) 3)
	  for e in li
	  do (format t "; ~A~VT~A~%"
		     (instr-sym e) c
		     (conc-stringlist
		      (loop for (s sn) on (rest +instr-keys+)
			    collect (format nil (if sn "~A: ~S   " "~A: ~S") (string-downcase s) (slot-value e (intern (symbol-name s) :fomus)))))))))

(defun get-instr-syms ()
  "Utility function:
Returns symbol IDs for all of FOMUS's instruments"
  (set-instruments
   (mapcar #'instr-sym (remove-duplicates (append *instruments* +instruments+) :key #'instr-sym :from-end t))))

(defun list-fomus-percussion ()
  "Info function:
Lists all of FOMUS's percussion instruments and their definitions"
  (set-instruments
    (loop with li = (remove-duplicates (append *percussion* +percussion+) :key #'perc-sym :from-end t)
	  with c = (+ (mloop for e in li maximize (length (symbol-name (perc-sym e)))) 3)
	  for e in li
	  do (format t "; ~A~VT~A~%"
		     (perc-sym e) c
		     (conc-stringlist
		      (loop for (s sn) on (rest +perc-keys+)
			    collect (format nil (if sn "~A: ~S   " "~A: ~S") (string-downcase s) (slot-value e (intern (symbol-name s) :fomus)))))))))

(defun get-perc-syms ()
  "Utility function:
Returns symbol IDs for all of FOMUS's percussion instruments"
  (set-instruments
   (mapcar #'perc-sym (remove-duplicates (append *percussion* +percussion+) :key #'perc-sym :from-end t))))

(defun list-fomus-clefs ()
  "Info function:
Lists all of FOMUS's clefs"
  (loop for e in +clefs+ do (format t "; ~A~%" (symbol-name (car e)))))

(defun list-fomus-instrgroups (&key (format t))
  "Info function:
Lists all of FOMUS's instrument groups and their arrangements"
  (let ((ss (remove-duplicates (append *instr-groups* +instr-groups+) :key #'first :from-end t)))
    (if format
	(labels ((aux (li ta)
		   (let ((br (first li)))
		     (format t "~A" (case br (:group "[ ") (:grandstaff "{ ") (:choirgroup "| ") (otherwise "  ")))
		     (loop for (e en) on (rest li)
			   if (consp e) do (aux e (+ ta 2)) (if en (format t "~%;~VT" ta) (format t "~A" (case br (:group " ]") (:grandstaff " }") ((nil) " |") (otherwise ""))))
			   else do (if en
				       (format t "~A~%;~VT" e ta)
				       (format t "~A~A"
					       e (case br (:group " ]") (:grandstaff " }") (:choirgroup " |") (otherwise ""))))))))
	  (loop for (e en) on ss
		do (format t "; ~A~%~%;" (first e)) (aux e 3)
		when en do (format t "~%~%")))
	(loop for e in ss do (format t "~S~%" e)))))

(defun list-fomus-meas-divs ()
  "Info function:
Lists all of the points at which FOMUS tries to divide measures (splitting and
tying notes inside them)"
  (loop for (s . r) in (sort (copy-list (remove-duplicates (append *default-meas-divs* +default-meas-divs+) :key #'first :from-end t)) #'< :key #'first)
	do (format t "; ~A~5T~{ ~A~}~%" s r)))

(defun list-fomus-tuplet-divs ()
  "Info function:
Lists all of the points at which FOMUS tries to divide tuplets (splitting and
tying notes inside them)"
  (loop for (s . r) in (sort (copy-list (remove-duplicates (append *default-tuplet-divs* +default-tuplet-divs+) :key #'first :from-end t)) #'< :key #'first)
	do (format t "; ~A~5T~{ ~A~}~%" s r)))

(defun get-midi-instr (prog &key (default *default-instr*))
  "Utility function:
Returns an INSTR object given a MIDI program change number"
  (set-instruments
    (or (find prog *instruments* :key #'instr-midiprgch-im :test (lambda (x p) (find x (force-list p))))
	(find prog +instruments+ :key #'instr-midiprgch-im :test (lambda (x p) (find x (force-list p))))
	default)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REGISTER PLUGINS

(declaim (type cons *backendexts*))
(defparameter *backendexts*
  '((:data . "fms") (:fomus . "fms") (:raw . "fmr")
    #-fomus-nolilypond (:lilypond . "ly")
    #-fomus-nomusicxml (:musicxml . "xml") #-fomus-nomusicxml (:musicxml-finale . "xml") #-fomus-nomusicxml (:musicxml-sibelius . "xml")
    #-fomus-nocmn (:cmn . "cmn") #-fomus-nomidi (:midi . "mid")))

(defun module-package (key)
  (let ((p (symbol-name key)))
    (if (string= "FOMUS-" p :end2 6) p (conc-strings "FOMUS-" p))))

(defmacro deffomusmodule (&rest args)
  (destructuring-bind (&key type keyname initfun entryfun preload filename-ext &allow-other-keys)
      (loop for e in args collect (first e) collect (rest e))
    (declare (ignore type))
    `(progn
       ,@(when preload (list `(eval-when (:load-toplevel :compile-toplevel :execute) ,(first preload)))) ; forms for loading dependencies
       (defpackage ,(module-package (first keyname))
	 (:use "FOMUS" "COMMON-LISP")
	 (:export ,(first initfun) ,(first entryfun))
	 ,@(loop for e in args unless (find (first e) '(:type :keyname :initfun :entryfun :preload :filename-ext :import-from-fomus)) collect e))
       (eval-when (:load-toplevel)
	 (provide ,(module-package (first keyname))) 
	 ,@(when (first filename-ext)
		 `((pushnew (cons ,(first keyname) (remove #\. ,(first filename-ext) :test #'char=)) *backendexts* :test #'equal))))
       (eval-when (:load-toplevel :compile-toplevel :execute) (in-package ,(module-package (first keyname)))))))

(defstruct (module (:copier nil) (:predicate nil))
  (type nil :type symbol)
  (file "" :type string)
  (pack "" :type string)
  (initfun nil :type symbol)
  (entryfun nil :type symbol)
  (desc "" :type string)
  ;; should fomus take care of compiling/loading this module?
  (compile-p t :type boolean))

(defparameter *fomus-modules* (make-hash-table :test 'eq))
(defparameter +module-types+ '(:accidentals :voices :staves/clefs :splitrules :quantize :backend))
(defparameter +module-defaults+ `(:acc1 :voices1 :staves/clefs1 :split1 :quantize1-rmse
				  ,(remove-if (lambda (x) (declare (type symbol x)) (member x '(:fomus :raw))) (mapcar #'car *backendexts*))))

(defun compile-module-if-needed (lisp-file fasl-file keyname)
  "Compile LISP-FILE into FASL-FILE, if needed. Returns FASL-FILE or NIL, if nothing happened."
  (when (or (not (probe-file fasl-file))
	    (>= (file-write-date lisp-file) (file-write-date fasl-file)))
    (when (and (numberp *verbose*) (>= *verbose* 2)) (format t "~&;; Compiling module ~S..." keyname))
    (compile-file lisp-file :print nil :verbose nil :output-file fasl-file)))

(defun module-outname (lisp-file backend)
  "Return `compile-file-pathname' for LISP-FILE, dealing with asdf, if available
and taking care for an additional `backends' sub-directory, if it is a BACKEND.
Directories are created as needed."
  (declare (ignorable backend))
  #+asdf (let ((fasl-proto-path (ignore-errors
				  (first (asdf:output-files (make-instance 'asdf:compile-op)
							    (asdf:find-component (asdf:find-system :fomus) "package"))))))
	   (if fasl-proto-path
	       (let* ((z (change-filename fasl-proto-path :name nil :ext nil))
		      (f (change-filename fasl-proto-path :dir (if backend
								   (conc-strings z "/modules/backends/")
								   (conc-strings z "/modules/"))
					  :name (pathname-name lisp-file))))
		 ;; experienced weird problems w/ wildcards (esp. w/ cmucl)--
		 ;; if fomus was compiled then fasl-proto-path should exist anyways (should be "package.fasl" or whatever)--
		 ;; seems much simpler
		 (unless (or (probe-file fasl-proto-path) (probe-file (change-filename fasl-proto-path :ext "lisp"))) 
		   (error "FOMUS compile directory ~S doesn't exist (this is a bug)" z)) ; small sanity check
		 (ignore-errors (ensure-directories-exist f))
		 f) 
	       (compile-file-pathname lisp-file))) ;; fallback to normal...
  #-asdf (compile-file-pathname lisp-file))

;; user fun
;; also called by REGISTER-PLUGINS at fomus startup
;; for every *.lisp file found in modules/ and modules/backends 
(defun register-fomus-module (filename &key load)
  (destructuring-bind (&key type keyname initfun entryfun (documentation '("(none)")) filename-ext &allow-other-keys)
      (let ((*package* (find-package :fomus))) ; make sure the user can call this from another package
	(with-open-file (f filename :direction :input)
	  (let ((d (read f)))
	    (unless (string= (symbol-name (first d)) "DEFFOMUSMODULE") (error "DEFFOMUSMODULE declaration not found"))
	    (loop for e in (rest d) collect (first e) collect (rest e)))))
    ;; make sure all the right values are stored so error doesn't happen later
    (unless (member (first type) +module-types+) (error "~S is not a valid module type" (first type)))
    (let ((x (first keyname))) (check-type x keyword))
    (check-type filename (or pathname string))
    (let ((x (first initfun))) (check-type x symbol))
    (let ((x (first entryfun))) (check-type x symbol))
    (let ((x (first documentation))) (check-type x string))
    (when (and (first filename-ext) (not (eq (first type) :backend))) (error "Non-backend shouldn't declare a filename extension"))
    (let ((pk (module-package (first keyname)))
	  (cf (module-outname filename (eq (first type) :backend))))
      (compile-module-if-needed filename cf (first keyname)) ; make sure it compiles
      (setf (gethash (first keyname) *fomus-modules*) (make-module :type (first type) :file filename :pack pk
								   :initfun (first initfun) :entryfun (first entryfun) :desc (first documentation))))
    (when load (load-fomus-module (first keyname))))
  t)

;; user fun
(defun list-fomus-modules (&rest type)
  (let ((ty (or type +module-types+)))
    (loop for l in (sort (split-into-groups (loop for h being each hash-key in *fomus-modules* using (hash-value v)
						  when (member (module-type v) ty) collect (cons h v)) (lambda (x) (module-type (cdr x))))
			 #'< :key (lambda (x) (position (module-type (cdar x)) +module-types+)))
	  for nx = nil then t
	  do (format t "~:[~;~%~];; ----Type: ~67,1,,'-A~%~{~%; Key: :~A   File: ~A~%; ~A~%~}" nx (symbol-name (module-type (cdr (first l))))
		     (nconc
		      (loop with bc = (eq (module-type (cdr (first l))) :backend)
			    for e in (force-list (lookup (module-type (cdr (first l))) (pairlis +module-types+ +module-defaults+)))
			    and fi = nil then t
			    nconc (list e "(internal)" (format nil "FOMUS ~:[default ~;~]internal ~:[~;backend ~]function" (or fi bc) bc)))
		      (loop for e in (sort l #'string< :key (lambda (x) (symbol-name (car x))))
			    collect (symbol-name (car e)) collect (module-file (cdr e))
			    collect (commentify (module-desc (cdr e)) 1)))))))

;; user fun
(defun load-fomus-module (keyname)
  (flet ((module-provided-p (module)
	   (find (module-pack module) *modules* :test #'string=)))
    (let* ((module (or (gethash keyname *fomus-modules*)
		       (error "Module ~S is not registered or does not exist" keyname)))
	   (fasl-path (module-outname (module-file module) (eq (module-type module) :backend))))
      (when (or (and (module-compile-p module) ; default is T
		     (compile-module-if-needed (module-file module) fasl-path keyname)) 
		(not (module-provided-p module)))
	(when (and (numberp *verbose*) (>= *verbose* 2)) (format t "~&;; Loading module ~S..." keyname))
	(load fasl-path :verbose nil :print nil)
	(when (module-initfun module)
	  (funcall (find-symbol (symbol-name (module-initfun module)) (find-package (module-pack module))))))
      t)))

(declaim (inline call-module))
(defun call-module (keyname err &rest args) ; assume it's been loaded
  (let ((pl (gethash keyname *fomus-modules*)))
    (if pl (apply (find-symbol (symbol-name (module-entryfun pl)) (find-package (module-pack pl))) args)
	(apply #'error err))))
