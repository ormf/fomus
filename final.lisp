;; -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**************************************************************************************************
;; FOMUS
;; final.lisp
;;**************************************************************************************************

(in-package :fomus)
(compile-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +initfilename+ (namestring (merge-pathnames ".fomus" (user-homedir-pathname))))

;; initfile
(defun load-initfile (&optional (filename +initfilename+) (info t))
  "FOMUS init function:
Reloads the \"~/.fomus\" initialization file"
  (with-open-file (f filename :direction :input :if-does-not-exist nil)
    (when f
      (when info (out ";; Loading initialization file ~S...~%" filename))
      (loop
       with nt0
       for x = (read f nil 'eof)
       #-clisp until #-clisp (eq x 'eof)
       for y = #-clisp (read f nil 'eof) #+clisp (if (eq x 'eof) (loop-finish) (read f nil 'eof))
       when (eq y 'eof) do (error "KEYWORD/ARGUMENT-PAIRS expected in initialization file")
       do (setf nt0 (find-symbol (conc-strings "*" (symbol-name x) "*") :fomus))
       if nt0 do (unless (ignore-errors (eval `(progn (setf ,(find-symbol (conc-strings "*" (symbol-name x) "*") :fomus) ,y) t)))
		   (format t ";; WARNING: Error setting ~S to value ~S in initialization file~%" x y))
       else do (format t ";; WARNING: Unknown setting ~S in initialization file~%" x)
       finally
       (return t)))))

(defparameter +fomus-dir+ #+asdf (or (ignore-errors (asdf:component-pathname (asdf:find-system :fomus))) *load-truename*) #-asdf *load-truename*)
(defun register-fomus-modules (&optional (info t))
  (map nil
       (lambda (file)
	 (multiple-value-bind (value error)
	     (ignore-errors (register-fomus-module file))
	   (when (and (null value) info)
	     (format t ";; NOTE: Can't compile/register module file ~S~%;    (~A)~%" (namestring file) (commentify (format nil "~A" error) 1)))))
       (nconc (directory (merge-pathnames "modules/*.lisp" +fomus-dir+))
	      (directory (merge-pathnames "modules/backends/*.lisp" +fomus-dir+))))
  (when info (format t "~&"))
  t)

(eval-when (:load-toplevel :execute)
  (export (mapcar (lambda (x) (find-symbol (conc-strings "*" (symbol-name (first x)) "*") :fomus)) +settings+) :fomus))

(eval-when (:load-toplevel :execute) (provide :fomus))

;; feature
(eval-when (:load-toplevel :execute)
  (pushnew :fomus *features*))

;; print load greeting
(eval-when (:load-toplevel :execute)
  (when (>= *verbose* 1) (format t "~&~%;; ~A v~A.~A.~A~%~A~%"
				 +title+
				 (first +version+) (second +version+) (third +version+)
				 (conc-stringlist (loop for e in +banner+ collect (format nil ";; ~A~%" e))))))

(eval-when (:load-toplevel :execute)
  (find-cm) (find-cmn))

(eval-when (:load-toplevel :execute)
  (unless (find-symbol "+FOMUS-INSTALL+" :common-lisp-user)
    (load-initfile)
    #-fomus-noautoreg (register-fomus-modules nil)))

(defun fomus-exe (initfile opts basename quality verbosity &rest filename)
  (let ((*package* (find-package "FOMUS")))
    (catcherr
      (load-initfile initfile nil)
      (register-fomus-modules nil)
      (let* ((v (when (find #\w opts) t))
	     (o (nconc (when (string/= quality "") (list :quality (ignore-errors (read-from-string quality))))
		       (when (string/= basename "") (list :filename basename))
		       (when (string/= verbosity "") (list :verbose (ignore-errors (read-from-string verbosity))))
		       (let ((x (nconc
				 (when (find #\l opts) (list (list :lilypond :view v)))
				 (when (find #\c opts) (list (list :cmn :view v)))
				 (when (find #\m opts) (list (list :fomus)))
				 (cond ((find #\f opts) (list (list :musicxml-finale)))
				       ((find #\s opts) (list (list :musicxml-sibelius)))
				       ((find #\x opts) (list (list :musicxml)))))))
			 (when x (cons :output x))))))
	(fomus-text (if (list1p filename) (first filename) filename) o #'fomus-textexec))))
  (fresh-line)
  (finish-output)
  #+cmu (ext:quit) #+sbcl (sb-ext:quit) #+openmcl (ccl:quit) #+clisp (ext:quit))

