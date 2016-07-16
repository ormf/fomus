;; -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**************************************************************************************************
;; FOMUS
;; misc.lisp
;;**************************************************************************************************

;; ***** UPDATED COMMENTS *****

(in-package :fomus)
(compile-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL

;; normalize print variables for printing to output file
(defmacro setprints (&body forms)
  `(let ((*print-array* t) (*print-base* 10) (*print-case* :downcase) (*print-circle* nil) (*print-gensym* t) (*print-length* nil)
	 (*print-level* nil) (*print-pretty* t) (*print-radix* nil) (*print-readably* nil) (*print-escape* t))
     ,@forms))

;; cmu and sbcl prints out an annoying warning referring to some internal operation sometimes--this gets rid of it
#+(or cmu sbcl)
(defmacro muffwarn (&body forms)
  `(handler-bind ((style-warning (lambda (x) (declare (ignore x)) (muffle-warning))))
    ,@forms))

;; catches errors and prints them with lispy ';;' marks without dying
(defmacro catcherr (&body forms)
  `(handler-case (progn ,@forms)
    (error (co)
     (fresh-line)
     (format t ";;; *****ERROR*****~%")
     (loop with s = (make-string-input-stream (string-trim '(#\newline #\space) (princ-to-string co)))
      for l = (read-line s nil 'eof)
      until (eq l 'eof)
      do (format t ";; ~A~%" l))
     nil)))

;; concatenate strings and lists of strings (i hate typing the word concatenate)
(declaim (inline conc-strings conc-stringlist))
(defun conc-strings (&rest strings)
  (apply #'concatenate 'string strings))
(defun conc-stringlist (list)
  (declare (type list list))
  (apply #'concatenate 'string list))

;; replaces only the parts of the filename given as keywords
;; tries to deal with implementation/playform-specific quirks involving trailing '/' characters
(declaim (inline change-filename))
(defun change-filename (filename &key (dir (pathname-directory filename)) (name (pathname-name filename)) (ext (pathname-type filename)))
  (declare (type (or pathname string list) filename name ext) (type (or pathname string list) dir))
  (namestring (make-pathname :device (pathname-device filename)
			     :directory
			     (if (or (stringp dir) (pathnamep dir))
				 (pathname-directory (conc-strings dir "/"))
			       dir)
			     :name name :type ext)))

;; tests for t
(declaim (inline truep))
(defun truep (x) (eq x t))

;; gets last element
(declaim (inline last-element))
(defun last-element (list)
  (declare (type list list))
  (car (last list)))

;; installs reader macro #Z
;; (when (get-dispatch-macro-character #\# #\Z) ; some Lisps return an error function, so don't bother printing warning
;;   (format t ";; WARNING: Reinstalling dispatch macro #Z~%"))
(set-dispatch-macro-character
 #\# #\Z
 (lambda (s c n)
   (declare (type stream s) (ignore c n))
   (let ((r (read s t nil t)))
     (apply #'make-instance r))))

;; generate methods to print fomus classes and structs
(declaim (type boolean *prepend-fm*))
(defparameter *prepend-fm* nil)
(defmacro defprint-class (class &rest slots)
  (let ((sl (loop
	     for i in slots
	     unless (and (listp i) (null (second i)))
	     collect (if (listp i) (second i) (intern (string i) 'keyword))
	     collect (list 'slot-value 'x (list 'quote (if (listp i) (first i) i))))))
    `(defmethod print-object ((x ,class) s)
      (declare (type stream s))
      (princ "#Z" s)
      (if *prepend-fm*
	  (format s "(~A:~A~{ ~S~})"
		  (if (eq *print-case* :downcase) "fm" "FM")
		  ,(list 'quote class)
		  ,(cons 'list sl))
	  (prin1 ,(nconc (list 'list (list 'quote class)) sl)
		 s)))))
(defmacro defprint-struct (class &rest slots)
  (let ((sl (loop
	     for i in slots
	     unless (and (listp i) (null (second i)))
	     collect (if (listp i) (second i) (intern (string i) 'keyword))
	     collect (list (if (listp i) (first i) i) 'x))))
    `(defmethod print-object ((x ,class) s)
      (declare (type stream s))
      (princ "#S" s)
      (if *prepend-fm*
	  (format s "(~A:~A~{ ~S~})"
		  (if (eq *print-case* :downcase) "fm" "FM")
		  ,(list 'quote class)
		  ,(cons 'list sl))
	  (prin1 ,(nconc (list 'list (list 'quote class)) sl)
		 s)))))

;; if list = an atom, forces it into a list
;; if list = a list, returns the list
(declaim (inline force-list force-newlist))
(defun force-list (list)
  (if (listp list) list (list list)))
;; returns a new list regardless
(defun force-newlist (list)
  (if (listp list) (copy-list list) (list list)))
;; returns doubly nested list, expecting that only some of the inner items might be lists
(defun force-list2some (list)
  (let ((x (force-list list)))
    (if (or (null x) (some #'consp x)) x
	(list x))))
;; returns doubly nested list, expecting that all the inner items might be lists
(defun force-list2all (list)
  (let ((x (force-list list)))
    (if (or (null x) (every #'consp x)) x
	(list x))))

;; objs = list
;; places = list of lists
;; prepends objs onto corresponding "places"
(defmacro cons-list (objs places)
  `(mapcar #'cons ,objs ,places))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DIRECTORIES, FILENAMES, IMPLEMENTATION/PLATFORM SPECIFIC THINGS

;; wrapper for namestring function, accepts a NIL filname
(declaim (inline namestring*))
(defun namestring* (filename) (when filename (namestring filename)))

;; special function for Allegro to run external command (uses older function for compatibility--might have to be updated at some point)
#+allegro
(defun run-allegro-cmd (cmd &optional (wait t) (hide t))
  (ignore-errors
    (multiple-value-bind (ostr istr p) (excl:run-shell-command
					#-(or mswindows win32) cmd
					#+(or mswindows win32) (if (typep cmd 'string) cmd
								   (conc-stringlist (loop for e across cmd and i = nil then t when i collect e and collect " ")))
					:input :stream :output :stream :error-output :stream :wait nil :show-window (if hide :hide :normal))
      (declare (ignore istr))
      (values (if wait (sys:os-wait nil p) 0) ostr))))

;; wrappers for probe-file and directory ignoring errors
(declaim (inline probe-file* directory*))
(defun probe-file* (fn) (ignore-errors (probe-file fn)))
(defun directory* (di &rest re) (ignore-errors (apply #'directory #+clisp (conc-strings di "/") #-clisp di re)))

;; looks for executable in likely places depending on operating system
;; returns a full pathname if found
(defun find-exe (filename &optional subdir)
  (declare (ignorable subdir))
  (namestring*
   (or #+(or linux (or darwin macos) unix cygwin) (probe-file* (change-filename filename :dir "/usr/local/bin"))
       #+(or linux (or darwin macos) unix cygwin) (probe-file* (change-filename filename :dir "/usr/bin"))
       #+(or linux (or darwin macos) unix cygwin) (probe-file* (change-filename filename :dir "/bin"))
       #+(or linux (or darwin macos) unix cygwin) (probe-file* (change-filename filename :dir "/usr/X11R6/bin"))
       #+(or darwin macos) (probe-file* (change-filename filename :dir "/sw/bin"))
       #+(or darwin macos) (probe-file* (change-filename filename :dir "/Applications"))
       #+(or darwin macos)
       (when subdir (find-if #'probe-file* (mapcar (lambda (x) (change-filename filename :dir (namestring x)))
						   (directory* (format nil "/Applications/~A" subdir) #+openmcl :directories #+openmcl t))))
       #+(or darwin macos)
       (when subdir (find-if #'probe-file* (mapcar (lambda (x) (change-filename filename :dir (namestring x)))
						   (directory* (format nil "/Applications/~A/*" subdir) #+openmcl :directories #+openmcl t))))
       #+(or darwin macos)
       (when subdir (find-if #'probe-file* (mapcar (lambda (x) (change-filename filename :dir (namestring x)))
						   (directory* (format nil "/Applications/~A/*/*" subdir) #+openmcl :directories #+openmcl t))))
       #+(or darwin macos)
       (when subdir (find-if #'probe-file* (mapcar (lambda (x) (change-filename filename :dir (namestring x)))
						   (directory* (format nil "/Applications/~A/*/*/*" subdir) #+openmcl :directories #+openmcl t))))
       #+(or mswindows win32) (probe-file* (change-filename filename :dir "/Program Files"))
       #+(or mswindows win32)
       (when subdir (find-if #'probe-file* (mapcar (lambda (x) (change-filename filename :dir (namestring x)))
						   (directory* (format nil "/Program Files/~A" subdir) #+openmcl :directories #+openmcl t))))
       #+(or mswindows win32)
       (when subdir (find-if #'probe-file* (mapcar (lambda (x) (change-filename filename :dir (namestring x)))
						   (directory* (format nil "/Program Files/~A/*" subdir) #+openmcl :directories #+openmcl t))))
       #+(or mswindows win32)
       (when subdir (find-if #'probe-file* (mapcar (lambda (x) (change-filename filename :dir (namestring x)))
						   (directory* (format nil "/Program Files/~A/*/*" subdir) #+openmcl :directories #+openmcl t))))
       #+(or mswindows win32)
       (when subdir (find-if #'probe-file* (mapcar (lambda (x) (change-filename filename :dir (namestring x)))
						   (directory* (format nil "/Program Files/~A/*/*/*" subdir) #+openmcl :directories #+openmcl t))))
       #+(or mswindows win32 cygwin) (probe-file* (change-filename filename :dir "/cygdrive/c/Program Files"))
       #+(or mswindows win32 cygwin)
       (when subdir (find-if #'probe-file* (mapcar (lambda (x) (change-filename filename :dir (namestring x)))
						   (directory* (format nil "/cygdrive/c/Program Files/~A" subdir) #+openmcl :directories #+openmcl t))))
       #+(or mswindows win32 cygwin)
       (when subdir (find-if #'probe-file* (mapcar (lambda (x) (change-filename filename :dir (namestring x)))
						   (directory* (format nil "/cygdrive/c/Program Files/~A/*" subdir) #+openmcl :directories #+openmcl t))))
       #+(or mswindows win32 cygwin)
       (when subdir (find-if #'probe-file* (mapcar (lambda (x) (change-filename filename :dir (namestring x)))
						   (directory* (format nil "/cygdrive/c/Program Files/~A/*/*" subdir) #+openmcl :directories #+openmcl t))))
       #+(or mswindows win32 cygwin)
       (when subdir (find-if #'probe-file* (mapcar (lambda (x) (change-filename filename :dir (namestring x)))
						   (directory* (format nil "/cygdrive/c/Program Files/~A/*/*/*" subdir) #+openmcl :directories #+openmcl t))))
       #+(or mswindows win32) (probe-file* (change-filename filename :dir "/cygwin/usr/local/bin"))
       #+(or mswindows win32) (probe-file* (change-filename filename :dir "/cygwin/usr/bin"))
       #+(or mswindows win32) (probe-file* (change-filename filename :dir "/cygwin/bin"))
       #+(or mswindows win32) (probe-file* (change-filename filename :dir "/cygwin/usr/X11R6/bin")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HEAP

;; the following functions implement a heap
(defstruct (heap (:constructor make-heap-aux) (:predicate heapp))
  (fun #'+ :type (function (t t) t))
  (arr #() :type (array t)))

(defun percdown (hp n)
  (declare (type heap hp) (type (integer 0) n))
  (let ((c1 (+ (ash n 1) 1))
	(c2 (+ (ash n 1) 2)))
    (let ((s (fill-pointer (heap-arr hp))))
      (if (< c2 s)
	  (let ((an (aref (heap-arr hp) n))
		(ac1 (aref (heap-arr hp) c1))
		(ac2 (aref (heap-arr hp) c2)))
	    (if (or (funcall (heap-fun hp) ac1 an)
		    (funcall (heap-fun hp) ac2 an))
		(if (funcall (heap-fun hp) ac1 ac2)
		    (progn (psetf (aref (heap-arr hp) c1) (aref (heap-arr hp) n) (aref (heap-arr hp) n) (aref (heap-arr hp) c1))
			   (percdown hp c1))
		    (progn (psetf (aref (heap-arr hp) c2) (aref (heap-arr hp) n) (aref (heap-arr hp) n) (aref (heap-arr hp) c2))
			   (percdown hp c2)))))
	  (if (< c1 s)
	      (if (funcall (heap-fun hp) (aref (heap-arr hp) c1) (aref (heap-arr hp) n))
		  (progn (psetf (aref (heap-arr hp) c1) (aref (heap-arr hp) n) (aref (heap-arr hp) n) (aref (heap-arr hp) c1))
			 (percdown hp c1))))))))
(defun percup (hp n)
  (declare (type heap hp) (type (integer 0) n))
  (if (> n 0)
      (let ((p (ash (- n (if (oddp n) 1 2)) -1)))
	(if (funcall (heap-fun hp) (aref (heap-arr hp) n) (aref (heap-arr hp) p))
	    (progn (psetf (aref (heap-arr hp) n) (aref (heap-arr hp) p) (aref (heap-arr hp) p) (aref (heap-arr hp) n))
		   (percup hp p))))))
(defun heap-ins (obj heap)
  (declare (type heap heap))
  (percup heap (vector-push-extend obj (heap-arr heap)))
  obj)
(defun heap-rem (heap)
  (declare (type heap heap))
  (if (> (fill-pointer (heap-arr heap)) 0)
      (let ((r (aref (heap-arr heap) 0)))
	(setf (aref (heap-arr heap) 0) (aref (heap-arr heap) (decf (fill-pointer (heap-arr heap)))))
	(percdown heap 0)
	r)))
(declaim (inline heap-peek heap-size heap-empty-p heap-clear))
(defun heap-peek (heap)
  (declare (type heap heap))
  (if (> (fill-pointer (heap-arr heap)) 0) (aref (heap-arr heap) 0)))
(defun heap-size (heap)
  (declare (type heap heap))
  (fill-pointer (heap-arr heap)))
(defun heap-empty-p (heap)
  (declare (type heap heap))
  (= (fill-pointer (heap-arr heap)) 0))
(defun heap-clear (heap)
  (declare (type heap heap))
  (setf (fill-pointer (heap-arr heap)) 0)
  heap)
(defun make-heap (fun &key initial-contents initial-size)
  (declare (type (function (t t) t) fun) (type list initial-contents) (type (or (integer 0) null) initial-size))
  (let ((hp (make-heap-aux :fun fun :arr (make-array (if initial-size (max (length initial-contents) initial-size) (length initial-contents)) :adjustable t :fill-pointer 0))))
    (loop for e in initial-contents do (heap-ins e hp))
    hp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MORE GENERAL

;; tests for list of length > 1
;; list = a list
(declaim (inline list>1p list1p))
(defun list>1p (list)
  (declare (type list list))
  (cdr list))
;; tests for list of length = 1
(defun list1p (list)
  (declare (type list list))
  (and list (null (cdr list))))

;; utility functions for various operations over a list of numbers
(declaim (inline or-list and-list min-list max-list))
(defun or-list (list)
  (declare (type list list))
  (loop for e in list thereis e)) ; if list = nil, returns nil
(defun and-list (list)
  (declare (type list list))
  (loop for e in list always e)) ; if list = nil, returns t
(defun min-list (list)
  (declare (type list list))
  (when list (mloop for e of-type real in list minimize e)))
(defun max-list (list)
  (declare (type list list))
  (when list (mloop for e of-type real in list maximize e)))
(defun ave-list (nums)
  (declare (type list nums))
  (loop for e of-type real in nums and n from 1 sum e into s finally (return (/ s n))))

;; utility math functions
(declaim (inline roundint diff roundto))
(defun diff (a b)
  (declare (type real a b))
  (abs (- a b)))
(defun roundint (i)
  (declare (type real i))
  (nth-value 0 (truncate (if (< i 0) (- i 1/2) (+ i 1/2)))))
;; round to given precision value
(defun roundto (i prec)
  (declare (type real i prec))
  (* (roundint (/ i prec)) prec))

;; nconcs obj and place and sets place to the result (modifies both obj and place)
(defmacro prenconc (obj place) 
  `(setf ,place (nconc ,obj ,place)))
;; appends obj and place and sets place to the result (just modifies place)
(defmacro prepend (obj place) 
  `(setf ,place (append ,obj ,place)))
;; performs prepend over a list of objs and list of places, setting places to the returned list
(defmacro prepend-lists (objs places)
  `(setf ,places (mapcar #'append ,objs ,places)))

;; split list into groups (results are unsorted)
;; key = function that returns a group id for each element
;; test = group id equality test
;; returns: list of lists (each inner list is a group)
(defun split-into-groups (list key &key (test 'eql)) ; groupfun should return objects unique to ea. group
  (declare (type list list) (type (function (t) t) key) (type symbol test))
  (let ((h (make-hash-table :test test)))
    (loop for e in list do
	  (let ((k (funcall key e)))
	    (setf (gethash k h) (cons e (gethash k h)))))
    (loop for e being each hash-value in h collect e)))

;; split a list into several lists given one or more test functions
;; funs = 1 or more predicate functions that return t if list item belongs in list 1, list 2, etc..
;; returns: (number of funs + 1) lists returned as separate values (last value represents list of no matches)
(defun split-list (list &rest funs)
  (declare (type list list))
  (loop
   with r = (loop repeat (1+ (length funs)) collect (cons nil nil))
   with re = (copy-list r)
   for e in list
   do (loop for a on r for f in funs
	    when (funcall f e) do (setf (car a) (setf (cdar a) (list e))) (return)
	    finally (setf (car a) (setf (cdar a) (list e))))
   finally (return (values-list (mapcar #'rest re)))))

;; splits a list into several lists according to vals
;; vals = list of values to compare list items to and sort against
;; key = function to get comparison val from list item
;; test = test function to compare key val again vals list
;; returns: list of lists grouped according to vals
(defun split-list* (list vals &key (key #'identity) (test #'eql))
  (declare (type list list vals) (type (function (t) t) key) (type (function (t t) t) test))
  (loop
   with r = (loop repeat (1+ (length vals)) collect (cons nil nil))
   with re = (copy-list r)
   for e in list
   do (loop with k = (funcall key e)
	    for a on r for v in vals
	    when (funcall test k v) do (setf (car a) (setf (cdar a) (list e))) (return)
	    finally (setf (car a) (setf (cdar a) (list e))))
   finally (return (mapcar #'rest re))))

;; some loop macros for finding argmaxes and mins
(defmacro loop-return-firstmin (form for var &rest loop)
  (let ((mx (gensym)) (ev (gensym)) (rt (gensym)))
    `(loop with ,mx and ,ev and ,rt
      ,for ,var ,@loop
      do (setq ,ev ,form)
      until (and ,mx (> ,ev ,mx))
      when (or (null ,mx) (< ,ev ,mx))
      do (setq ,mx ,ev ,rt ,var)
      finally (return ,rt))))
(defmacro loop-return-lastmin (form for var &rest loop)
  (let ((mx (gensym)) (ev (gensym)) (rt (gensym)))
    `(loop with ,mx and ,ev and ,rt
      ,for ,var ,@loop
      do (setq ,ev ,form)
      until (and ,mx (> ,ev ,mx))
      when (or (null ,mx) (<= ,ev ,mx))
      do (setq ,mx ,ev ,rt ,var)
      finally (return ,rt))))
(defmacro loop-return-argmax (form for var &rest loop) ; if several argmaxes are =, return first one
  (let ((mx (gensym)) (ev (gensym)) (rt (gensym)))
    `(loop with ,mx and ,ev and ,rt
      ,for ,var ,@loop
      do (setq ,ev ,form)		; ev = arg eval
      when (or (null ,mx) (> ,ev ,mx))
      do (setq ,mx ,ev ,rt ,var)
      finally (return ,rt))))
(defmacro loop-return-argmin (form for var &rest loop)
  (let ((mx (gensym)) (ev (gensym)) (rt (gensym)))
    `(loop with ,mx and ,ev and ,rt
      ,for ,var ,@loop
      do (setq ,ev ,form)		; ev = arg eval
      when (or (null ,mx) (< ,ev ,mx))
      do (setq ,mx ,ev ,rt ,var)
      finally (return ,rt))))
(defmacro loop-return-argmins (form for var &rest loop)
  (let ((mx (gensym)) (ev (gensym)) (rt (gensym)))
    `(loop with ,mx and ,ev and ,rt
      ,for ,var ,@loop
      do (setq ,ev ,form)		; ev = arg eval
      when (or (null ,mx) (< ,ev ,mx))
      do (setq ,mx ,ev ,rt (list ,var))
      else when (= ,ev ,mx)
      do (push ,var ,rt)
      finally (return ,rt))))

;; does an assoc on a list and returns the cdr of the found item
#-cmu (declaim (inline lookup))
(defun lookup (item list &rest keys)
  (declare (type list list))
  (cdr (apply #'assoc item list keys)))

;; merges consecutive items in a list into single items, depending on return value of fun
;; fun = function accepting two consecutive items, returning a merged item or nil if they aren't to be merged
;; returns: the new list
(defun merge-linear (list fun)
  (declare (type list list) (type (function (t t) t) fun))
  (if (cdr list)
      (loop
       with e1 = (first list)
       for (e2 . re) on (rest list)
       for m = (funcall fun e1 e2)
       if m do (setf e1 m)
       else collect e1 into r and do (setf e1 e2)
       unless re collect e1 into r and do (return r))
      list))
;; (defun merge-linear (list fun)
;;   (declare (type list list) (type (function (t t) t) fun))
;;   (when list
;;     (loop
;;      with e1 = (first list)
;;      #-clisp while #-clisp list
;;      for e2 in #-clisp (rest list) #+clisp (when list (rest list))
;;      for m = (funcall fun e1 e2)
;;      if m do (setf e1 m)
;;      else collect e1 into r and do (setf e1 e2)
;;      finally (return (nconc r (list e1))))))

;; merges all items in a list into single items, depending on return value of fun
;; fun = function accepting two items, returning a merged item or nil if they aren't to be merged
;; call-rev = t if fun is also to be called with items in reverse order (up to two calls are made with the same two items)
;; returns: the new list
(defun merge-all (list fun &key (call-rev t))
  (declare (type list list) (type (function (t t) t) fun) (type boolean call-rev))
  (if (rest list)
      (loop
       with x = (mapcar #'list list) with l of-type cons = (last-element x) and c of-type cons = (first x)
       while (and (not (eq c l)) (rest x)) do
       (setf x (loop for (e . er) of-type (cons . list) on (rest x)
		     for m = (if call-rev (or (funcall fun (car c) (car e)) (funcall fun (car e) (car c)))
				 (funcall fun (car c) (car e)))
		     if m do (let ((ms (list m))) (setf c ms l ms)) else collect e into rt
		     when (or (not er) (eq e l)) nconc er into rt and collect c into rt and do (return rt)))
       (setf c (first x))		; c = object being combined
       finally (return (mapcar #'car x)))
      list))
;; (defun merge-all (list fun &key (call-rev t))
;;   (declare (type list list) (type (function (t t) t) fun) (type boolean call-rev))
;;   (when list
;;     (loop
;;        with x = (copy-list list) with l = (last-element x)
;;        until (eq (first x) l)
;;        do (let* ((y (first x))
;; 		 (z (delete-if (lambda (e)
;; 				 (let ((s (if call-rev (or (funcall fun y e) (funcall fun e y))
;; 					      (funcall fun y e))))
;; 				   (when s (setf y s l s))))
;; 			       (rest x))))
;; 	    (setf x (nconc z (list y))))
;;        finally (return x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROGRESS PRINTING

;; print a string if a certain amount of time has elapsed (set by track-progress function)
(declaim (type (integer 0) *print-prog-time* *print-prog-secs*))
(declaim (special *print-prog-time* *print-prog-secs*))
(defun progress (string)
  (declare (type string string))
  (when (>= (get-internal-run-time) *print-prog-time*)
    (when (> *verbose* 0)
      (format t string)
      (finish-output))
    (setf *print-prog-time* (+ (get-internal-run-time) (* *print-prog-secs* internal-time-units-per-second)))))
;; initialize progress printer
;; secs = seconds between each printing progress
(defmacro track-progress (secs &body forms)
  `(let ((*print-prog-time* (+ (get-internal-run-time) (* ,secs internal-time-units-per-second)))
	 (*print-prog-secs* ,secs))
    ,@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SEARCH ENGINE

;; the following implement a BFS algorithm with an optional limited heap
(declaim (type (integer -1) *a*-id*))
(declaim (special *a*-id*))
(defstruct (a*-node (:copier nil) (:predicate nil))
  (id (incf *a*-id*) :type (integer 0) :read-only t)
  data score (val t :type boolean)) ; val = valid

;; init-nodes = list of initial search nodes
;; scorefun = function that returns a score value when passed a node
;; expandfun = function that returns list of child nodes when passed a parent node
;; solutfun = functino that returns t if solution is found
;; heaplim = number of highest-scoring nodes to keep track of (or nil if no limit is desired)
;; scoregreaterfun = comparison function for scores (ex. set to #'< if lowest score if best)
;; remscoregreaterfun = comparison function for scores used when removing lowest ones from heap
;; retdefault = return value if no init-nodes are provided (if engine can't return anything)
;; returns: the best node!
(defun bfs*-engine (init-nodes scorefun expandfun solutfun &key heaplim (scoregreaterfun #'>) (remscoregreaterfun #'<) retdefault)
  (declare (type (function (t) t) scorefun solutfun) (type (function (t) list) expandfun)
	   (type (or null (integer 0)) heaplim) (type (function (t t) t) scoregreaterfun remscoregreaterfun))
  (let ((*a*-id* -1)
	(hs (length init-nodes)))
    (declare (type (integer 0) hs))
    (flet ((pp (h) (loop for n = (heap-rem h) always n until (a*-node-val n)
			 finally (decf hs) (setf (a*-node-val n) nil) (return n))))
      (let ((ic (mapcar (lambda (e) (make-a*-node :data e :score (funcall scorefun e))) init-nodes)))
	(let ((h (make-heap (lambda (x y)
			      (or (funcall scoregreaterfun (a*-node-score x) (a*-node-score y))
				  (unless (funcall scoregreaterfun (a*-node-score y) (a*-node-score x))
				    (< (a*-node-id x) (a*-node-id y)))))
			    :initial-contents ic :initial-size (* heaplim 2)))
	      (rh (when heaplim (make-heap (lambda (x y)
					     (or (funcall remscoregreaterfun (a*-node-score x) (a*-node-score y))
						 (unless (funcall remscoregreaterfun (a*-node-score y) (a*-node-score x))
						   (> (a*-node-id x) (a*-node-id y)))))
					   :initial-contents ic :initial-size (* heaplim 2)))))
	  (loop
	   for n = (pp h)
	   unless n do (return retdefault)
	   do (if (funcall solutfun (a*-node-data n))
		  (return (a*-node-data n))
		  ;;(if (<= hs 0) (return (a*-node-data n)) (ii n))
		  (mapcar (lambda (e)
			    (if heaplim
				(let ((o (make-a*-node :data e :score (funcall scorefun e))))
				  (heap-ins o h) (heap-ins o rh))
				(heap-ins (make-a*-node :data e :score (funcall scorefun e)) h))
			    (incf hs))
			  (funcall expandfun (a*-node-data n))))
	   when heaplim do (loop while (> hs heaplim) do (pp rh))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TYPE CHECKING

;; slightly more complicated type checking that checks for unique values, values within structs and classes, keyword-argument pairs, 
;; allows things to be wrapped into let statements, customizable error strings, etc..
;; if satisfies could pass arguments, this could all just be implemented with deftypes				    
(defun check-type* (obj type &optional er un lt)
  (declare (type (or null string) er) (type list un lt))
  (flet ((get-error (x)
	   (apply #'format nil (typecase (first x) (symbol (symbol-value (first x))) (otherwise (first x)))
		  (mapcar (lambda (z)
			    (if (truep z) obj
				(cond ((functionp z) (funcall z obj))
				      ((and (listp z) (eq (first z) 'function)) (funcall (second z) obj))
				      (t z))))
			  (rest x)))))
    (typecase type
      (cons (let ((ty (mapcar (lambda (x) (let ((z (find x lt :key #'car))) (if z (cdr z) x))) (rest type))))
	      (destructuring-bind (&optional fi se th &rest xxx) ty
		(declare (ignore xxx))
		(case (first type)
		  (cons* (and (consp obj) (check-type* (car obj) fi er un lt) (check-type* (cdr obj) se er un lt)))
		  (cons-of* (and (consp obj) (check-type* (car obj) fi er un lt) (check-type* (cdr obj) fi er un lt)))
		  (list* (and (consp obj) (= (length obj) (length ty)) (loop for o in obj and y in ty always (check-type* o y er un lt))))
		  (list-of* (and (consp obj) (loop for o in obj always (check-type* o fi er un lt))))
		  (list-of-unique* (and (listp obj)
					(loop for o2 on obj for o1 = (car o2) never (find o1 (rest o2) :test #'equal))
					(loop for o in obj always (check-type* o fi er un lt))))
		  (vector* (and (vectorp obj) (= (length obj) (length ty)) (loop for i from 0 below (length obj) and y in ty always (check-type* (svref obj i) y er un lt))))
		  (vector-of* (and (vectorp obj) (loop for i from 0 below (length obj) always (check-type* (svref obj i) fi er un lt))))
		  (array-of* (and (arrayp obj) (loop for i from 0 below (array-total-size obj) always (check-type* (row-major-aref obj i) fi er un lt))))
		  (struct* (and (typep obj fi) (loop for (s y) in (rest ty) always (check-type* (funcall s obj) y er un lt))))
		  (class* (and (typep obj fi) (loop for (s y) in (rest ty) always (check-type* (slot-value obj s) y er un lt))))
		  (key-arg-pairs* (and (listp obj) (evenp (length obj)) (loop for k in obj by #'cddr always (and (keywordp k) (find k ty)))))
		  (type* (check-type* obj (symbol-value fi) er un lt))
		  (check* (if (check-type* obj fi er un lt) t
			      (let ((x (get-error (rest ty)))) (if er (error er x) (error x)))))
		  (or* (loop for y in ty for cp = (mapcar (lambda (u) (cons (car u) (cdr u))) un) for re = (check-type* obj y er cp lt) until re
			     finally (when re (mapcar (lambda (u c) (setf (cdr u) (cdr c))) un cp) (return t))))
		  (and* (loop for y in ty for cp = (mapcar (lambda (u) (cons (car u) (cdr u))) un) for re = (check-type* obj y er cp lt) while re
			      finally (when re (mapcar (lambda (u c) (setf (cdr u) (cdr c))) un cp) (return t))))
		  (length* (funcall fi (length obj) se))
		  (with-unique* (check-type* obj se er (nconc (mapcar #'list fi) un) lt))
		  (unique* (let ((x (assoc fi un))
				 (o (if th se obj)))
			     (unless (find o (cdr x) :test #'equal)
			       (push o (cdr x))
			       (check-type* obj (or th se) er un lt))))
		  (let* (mapcar (lambda (x) (push (cons (first x) (second x)) lt)) fi) (check-type* obj se er un lt))
		  (error* (let ((x (get-error ty))) (if er (error er x) (error x))))
		  (with-error* (if (or (stringp (first fi)) (check-type* obj (first fi) er un lt))
				   (let ((x (get-error (if (stringp (car fi)) fi (rest fi)))))
				     (check-type* obj se (if er (format nil er x) x) un lt))
				   (check-type* obj se er un lt)))
		  (if* (if (check-type* obj fi er un lt) (check-type* obj se er un lt) (check-type* obj th er un lt)))
		  (when* (if (funcall fi obj) (check-type* obj se er un lt) t))
		  (unless* (if (funcall fi obj) t (check-type* obj se er un lt)))
		  (otherwise (typep obj type))))))
      (symbol (case type
		(key-arg-pairs* (and (listp obj) (evenp (length obj)) (loop for k in obj by #'cddr always (keywordp k))))
		(otherwise (typep obj type))))
      (function (funcall type obj))
      (otherwise (typep obj type)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MORE GENERAL

;; removes newlines and double spaces from a string
(defun remove-newlines (str &optional (and2xs t))
  (declare (type string str))
  (loop with c = 0
	for p = #\space then x
	for x0 across str
	for x = (if (char= x0 #\newline) #\space x0)
	unless (and and2xs (char= p #\space) (char= x #\space))
	collect x into r and do (incf c)
	finally (return (make-array c :element-type 'character :initial-contents r))))

;; the return string needs to be run through FORMAT
(defun commentify (str in)
  (declare (type string str) (type (integer 1) in))
  (loop with in = (format nil "~%~A " (make-string in :initial-element #\;))
	and sr = (string-trim '(#\newline) str)
	for p = (position #\newline sr :start (if p (1+ p) 0))
	while p do (setf sr (conc-strings (subseq sr 0 p) in (subseq sr (1+ p))))
	finally (return sr)))

;; cartesian distance
(declaim (inline distance))
(defun distance (x y)
  (declare (type real x y))
  (sqrt (+ (* x x) (* y y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pathname compatibility - deliberately borrowed from cl-fad

;; SAVE THIS (don't need at the moment, the directory function causes silly errors in some Lisps)

;; (declaim (inline component-present-p #|directory-pathname-p|#))
;; (defun component-present-p (value)
;;   "Helper function for DIRECTORY-PATHNAME-P which checks whether VALUE
;; is neither NIL nor the keyword :UNSPECIFIC."
;;   (and value (not (eql value :unspecific))))

;; (defun directory-pathname-p (pathspec)
;;   "Returns NIL if PATHSPEC \(a pathname designator) does not designate
;; a directory, PATHSPEC otherwise.  It is irrelevant whether file or
;; directory designated by PATHSPEC does actually exist."
;;   (and 
;;    (not (component-present-p (pathname-name pathspec)))
;;    (not (component-present-p (pathname-type pathspec)))
;;    pathspec))

;; ;; pathspec/dirname won't have wildcards in it
;; (defun pathname-as-directory (pathspec)
;;   "Converts the non-wild pathname designator PATHSPEC to directory
;; form."
;;   (let ((pathname (pathname pathspec)))
;;     (when (wild-pathname-p pathname)
;;       (error "Can't reliably convert wild pathnames."))
;;     (cond ((not (directory-pathname-p pathspec))
;;            (make-pathname :directory (append (or (pathname-directory pathname)
;;                                                  (list :relative))
;;                                              (list (file-namestring pathname)))
;;                           :name nil
;;                           :type nil
;;                           :defaults pathname))
;;           (t pathname))))

;; (defun directory-wildcard (dirname)
;;   "Returns a wild pathname designator that designates all files within
;; the directory named by the non-wild pathname designator DIRNAME."
;;   (when (wild-pathname-p dirname)
;;     (error "Can only make wildcard directories from non-wildcard directories."))
;;   (make-pathname :name #-:cormanlisp :wild #+:cormanlisp "*"
;;                  :type #-(or :clisp :cormanlisp) :wild
;; 		 #+:clisp nil
;; 		 #+:cormanlisp "*"
;;                  :defaults (pathname-as-directory dirname)))

