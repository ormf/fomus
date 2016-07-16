;; -*- lisp -*-

;; NOTEPRO--Backend Module
;; David Psenicka

;;   Outputs a NotePro file for use with M4C

;; Options:



(deffomusmodule
    (:keyname :notepro) (:type :backend) (:entryfun do-notepro) (:filename-ext "np")
    (:import-from #:fomus #:force-list #:*verbose* #:out)
    (:documentation "(Unfinished) Outputs a NotePro file for use with M4C"))

;; timesig is given in the form of a cons
;; barline is the barline property (:double, etc.) for the measure if present (applies to the BEGINNING of the measure)
(defstruct (timedump (:copier nil) (:predicate timedumpp))
  off endoff dur timesig barline)
  
(defun do-timedump (parts filename options process view)
  (declare (ignorable parts filename options process view))
  (out ";; ERROR: NotePro backend doesn't work yet~%")
  #|(when (>= *verbose* 1) (out ";; Saving NotePro file ~S...~%" filename))|#)

;; (destructuring-bind (&key callback partids &allow-other-keys) options
;;     (if callback
;; 	(loop for e in (or (force-list partids) (list (part-partid (first parts))))
;; 	      for p = (find e parts :key #'part-partid)
;; 	      do (funcall callback
;; 			  (loop with bl for m in (part-meas p) 
;; 				collect (make-timedump :off (meas-off m) :endoff (meas-endoff m)
;; 						       :dur (- (meas-endoff m) (meas-off m)) :timesig (timesig-time (meas-timesig m))
;; 						       :barline bl)
;; 				do (setf bl (second (getprop m :barline))))
;; 			  filename))
;; 	(format t ";; ERROR: Need a user callback function for timedump~%")))
