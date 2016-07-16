;; -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**************************************************************************************************
;; FOMUS
;; backend_xml.lisp
;;**************************************************************************************************

(in-package :fomus)
(compile-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +xml-comment+ "~A v~A.~A.~A")

(defparameter +xml-head+ '("<?xml version=\"1.0\" standalone=\"no\"?>"
			   "<!DOCTYPE score-partwise PUBLIC \"-//Recordare//DTD MusicXML 1.1 Partwise//EN\" \"http://www.musicxml.org/dtds/partwise.dtd\">"))

(defparameter +xml-num-note+ (vector "C" nil "D" nil "E" "F" nil "G" nil "A" nil "B"))
(defparameter +xml-num-perc-note+ (vector "B" nil "C" nil "D" "E" nil "F" nil "G" nil "A"))
(defparameter +xml-num-durtype+
  '((1/256 . "256th") (1/128  . "128th") (1/64 . "64th") (1/32 . "32nd") (1/16 . "16th") (1/8 . "eighth")
    (1/4 . "quarter") (1/2 . "half") (1 . "whole") (2 . "breve") (4 . "long")))
(defparameter +xml-num-acctype+
  (vector (vector nil "flat-flat")
	  (vector "three-quarters-flat" "flat" "quarter-flat")
	  (vector "quarter-flat" "natural" "quarter-sharp")
	  (vector "quarter-sharp" "sharp" "three-quarters-sharp")
	  (vector nil "sharp-sharp")))

(defparameter +xml-dynamics+
  '((:pppppp . "pppppp") (:ppppp . "ppppp") (:pppp . "pppp") (:ppp . "ppp") (:pp . "pp") (:p . "p") (:mp . "mp")
    (:mf . "mf") (:f . "f") (:ff . "ff") (:fff . "fff") (:ffff . "ffff") (:fffff . "fffff") (:ffffff . "ffffff")
    (:fp . "fp") (:sf . "sf") (:sff . ("sff")) (:sp . ("sp")) (:spp . ("spp")) (:sfz . "sfz") (:rfz . "rfz")))

(defparameter +xml-barlines+
  '((:single . "regular") (:double . "light-light") (:final . "light-heavy")
    (:repeatleft . "light-heavy") (:repeatright . "heavy-light") (:repeatleftright . "heavy-heavy")
    (:invisible . "none")))

(defparameter +xml-clefs+
  '((:subbass-8dn . ("F" 5 -1)) (:bass-8dn . ("F" 4 -1)) (:c-baritone-8dn . ("C" 5 -1)) (:f-baritone-8dn . ("F" 3 -1)) (:tenor-8dn . ("C" 4 -1))
    (:subbass . ("F" 5)) (:alto-8dn . ("C" 3 -1)) (:bass . ("F" 4)) (:mezzosoprano-8dn . ("C" 2 -1)) (:c-baritone . ("C" 5)) (:f-baritone . ("F" 3))
    (:soprano-8dn . ("C" 1 -1)) (:tenor . ("C" 4)) (:subbass-8up . ("F" 5 1)) (:treble-8dn . ("G" 2 -1)) (:alto . ("C" 3)) (:bass-8up . ("F" 4 1)) 
    (:mezzosoprano . ("C" 2)) (:c-baritone-8up . ("C" 5 1)) (:f-baritone-8up . ("F" 3 1)) (:soprano . ("C" 1)) (:tenor-8up . ("C" 4 1))
    (:treble . ("G" 2)) (:alto-8up . ("C" 3 1)) (:mezzosoprano-8up . ("C" 2 1)) (:soprano-8up . ("C" 1 1)) (:treble-8up . ("G" 2 1))
    (:percussion . ("percussion"))))

(defparameter +xml-noteheads+
  '((:harmonic . "diamond") (:diamond . "diamond") (:x . "x") (:xcircle . "circle-x") (:triangle . "triangle") (:slash . "slash")))

(defparameter +xml-ornaments+
  '((:trill . "trill-mark") (:startlongtrill- . "trill-mark") (:prall . "inverted-mordent") (:mordent . "mordent")))
(defparameter +xml-technicals+ ;; add: 2x-tongue, 3x-tongue, snap-pizz,
  '((:open . "open-string") (:thumb . "thumb-position") (:stopped . "stopped")
    (:leftheel . ("heel" ("placement" "below"))) (:rightheel . ("heel" ("placement" "above")))
    (:lefttoe . ("toe" ("placement" "below"))) (:righttoe . ("toe" ("placement" "above")))
    (:upbow . "up-bow") (:downbow . "down-bow")))
(defparameter +xml-articulations+
  '((:accent . "accent") (:marcato . "strong-accent") (:staccatissimo . "staccatissimo")
    (:staccato . "staccato") (:tenuto . "tenuto") (:portato . "detached-legato") ((:breath :after) . "breath-mark")))
(defparameter +xml-words+
  '((:pizz . "pizz.") (:arco . "arco")))
(defparameter +xml-keysigs+
  '((:cmaj . ("0" . "major")) (:amin . ("0" . "minor"))
    (:gmaj . ("1" . "major")) (:emin . ("1" . "minor"))
    (:dmaj . ("2" . "major")) (:bmin . ("2" . "minor"))
    (:amaj . ("3" . "major")) (:f+min . ("3" . "minor"))
    (:emaj . ("4" . "major")) (:c+min . ("4" . "minor")) 
    (:bmaj . ("5" . "major")) (:g+min . ("5" . "minor")) (:c-maj . ("-7" . "major")) (:a-min . ("-7" . "minor"))
    (:f+maj . ("6" . "major")) (:d+min . ("6" . "minor")) (:g-maj . ("-6" . "major")) (:e-min . ("-6" . "minor"))
    (:c+maj . ("7" . "major")) (:a+min . ("7" . "minor")) (:d-maj . ("-5" . "major")) (:b-min . ("-5" . "minor"))
    (:a-maj . ("-4" . "major")) (:fmin . ("-4" . "minor"))
    (:e-maj . ("-3" . "major")) (:cmin . ("-3" . "minor"))
    (:b-maj . ("-2" . "major")) (:gmin . ("-2" . "minor"))
    (:fmaj . ("-1" . "major")) (:dmin . ("-1" . "minor"))))

(defparameter +xml-textstyle+
  '(("font-style" "italic")))
(defparameter +xml-dyntextstyle+ ;; font-family font-style font-size font-weight, xx-small, x-small, small, medium, large, x-large, xx-large
  '(("font-family" "music") ("font-style" "italic") ("font-size" "xx-large") ("font-weight" "bold"))) ; *** find appropriate pt. size for various apps
(defparameter +xml-textnotestyle+
  '(("font-style" "italic")))
(defparameter +xml-texttempostyle+
  '(("font-size" "x-large") ("font-weight" "bold")))

(defparameter +xml-numlvls+ 6)

(defun write-xml (cont str &optional (ind 0))
  (destructuring-bind (ta ar0 &rest re) cont
    (let ((ar (conc-stringlist (loop for (a va) in (force-list2all ar0) collect (format nil " ~A=\"~A\"" a va)))))
      (if re
	  (if (consp (first re))
	      (progn (format str "~V,0T<~A~A>~%" ind ta ar)
		     (loop for e in re do (write-xml e str (+ ind 4)))
		     (format str "~V,0T</~A>~%" ind ta))
	      (format str "~V,0T<~A~A>~A</~A>~%" ind ta ar
		      (loop with x = (format nil "~A" (first re))
			    for (cf . cr) in '((#\& . "&amp;") (#\< . "&lt;") (#\> . "&gt;"))
			    do (setf x (loop with y = x
					     for p = (position cf y) while p
					     collect (subseq y 0 p) into z
					     collect cr into z
					     do (setf y (subseq y (1+ p)))
					     finally (return (conc-stringlist (nconc z (list y))))))
			    finally (return x))
		      ta))
	  (format str "~V,0T<~A~A/>~%" ind ta ar)))))

(defparameter *xml-1note-tremolo-kludge* nil)
(defparameter *xml-multinote-tremolo-kludge* nil)
(defparameter *xml-harmonic-kludge* nil)
(defparameter *xml-partgroups-kludge* nil)
;; (defparameter *xml-heel-kludge* nil)
;; (defparameter *xml-toe-kludge* nil)

;;; Currently, this function does not compile on sbcl. I could not yet
;;; investigate why. --KS
#-sbcl
(defun save-xml (parts header filename options #|process view|#)
  (when (>= *verbose* 1) (out ";; Saving MusicXML file ~S...~%" filename))
  (destructuring-bind (&key (xml-1note-tremolo-kludge *xml-1note-tremolo-kludge*)
			    (xml-multinote-tremolo-kludge *xml-multinote-tremolo-kludge*)
			    (xml-harmonic-kludge *xml-harmonic-kludge*)
			    (xml-partgroups-kludge *xml-partgroups-kludge*) &allow-other-keys) options
    (with-open-file (f filename :direction :output :if-exists :supersede)
      (loop for e in +xml-head+ do (format f "~A~%" e))
      (format f "<!-- ~A -->~%" header)
      (flet ((msrt (x y) (cond
			   ((find (cdr x) +marks-withacc+) nil) ; cadr 8/18/06
			   ((find (cdr y) +marks-withacc+) t) ; cadr 8/18/06
			   (t t)))
	     (ssrt (x y) (let ((x2 (second x)) (y2 (second y))) ; caddr 8/18/06
			   (cond ((and (numberp x2) (numberp y2)) (< x2 y2))
				 (x2 t))))
	     (getnum (n li) (loop for i from 1 to +xml-numlvls+	;; n is (lvl . voice)
				  unless (find i (car li) :key #'car) do (push (cons i n) (car li)) (return i)))
	     (remnum (n li) (prog1
				(car (find n (car li) :key #'cdr :test #'equal))
			      (setf (car li) (delete n (car li) :key #'cdr :test #'equal)))))
	(let* ((dv0 (mloop for p in parts maximize
			   (mloop for m in (part-meas p) maximize
				  (mloop with ts = (meas-timesig m)
					 for v in (meas-voices m) maximize
					 (mloop for e in v
						maximize (denominator (* (event-writtendur e ts) 4))))))) 
	       (dv (* dv0 4)))
	  (write-xml
	   `("score-partwise" nil
	     ,@(when *title* `(("work" nil ("work-title" nil ,*title*))))
	     ("identification" nil
	      ,@(when *composer* `(("creator" ("type" "composer") ,*composer*)))
	      ("encoding" nil
	       ("encoding-date" nil ,(apply #'format nil "~A-~A-~A"
					    (multiple-value-bind (xxx1 xxx2 xxx3 d m y) (get-decoded-time)
					      (declare (ignore xxx1 xxx2 xxx3))
					      (list m d y))))
	       ("software" nil ,header)))
	     ("part-list" nil
	      ,.(loop
		 for p in parts and pn from 1
		 for s = (getprops p :startgroup) and e = (getprops p :endgroup)
		 when (and s (not xml-partgroups-kludge))
		 nconc (loop for x in (sort s #'< :key #'second) when (> (second x) 0) collect
			     `("part-group" (("type" "start") ("number" ,(second x)))
			       ,@(case (third x)
				       (:group '(("group-symbol" nil "bracket")))
				       (:grandstaff '(("group-symbol" nil "brace"))))
			       ("group-barline" nil "yes")))
		 collect `("score-part" ("id" ,(format nil "P~A" pn))
			   ("part-name" nil ,(or (part-name p) ""))
			   ,@(when (part-abbrev p) `(("part-abbreviation" nil ,(part-abbrev p)))))
		 when (and e (not xml-partgroups-kludge)) nconc (loop for x in (sort e #'> :key #'second) when (> (second x) 0) collect
								      `("part-group" (("type" "stop") ("number" ,(second x)))))))
	     ,.(loop for p in parts and pn from 1 for pc = (is-percussion p) and ns = (instr-staves (part-instr p)) collect
		`("part" ("id" ,(format nil "P~A" pn))
		  ,.(loop with slrlvl = (cons nil nil) and wlvl = (cons nil nil) and olvl = (cons nil nil) and tlvl = (cons nil nil)
		     for m in (part-meas p) for mn from 1 collect
		     `("measure" ("number" ,mn)
		       ,@(let ((ss (getprop m :startsig))
			       (kk (getprop m :startkey))
			       (cc (getprops m :clef)))
			      (when (or (= mn 1) ss cc)
				`(("attributes" nil
				   ,@(when (= mn 1) `(("divisions" nil ,dv0)))
				   ,@(when kk (let ((x (lookup (second (getprop (meas-timesig m) :keysig)) +xml-keysigs+)))
						(when x `(("key" nil
							   ("fifths" nil (car x))
							   ("mode" nil (cdr x)))))))
				   ,@(when ss `(("time" ,(when (eq *timesig-style* :common)
							       (cond ((equal (timesig-time (meas-timesig m)) '(4 . 4)) '("symbol" "common"))
								     ((equal (timesig-time (meas-timesig m)) '(2 . 4)) '("symbol" "cut"))))
						 ("beats" nil ,(timesig-num (meas-timesig m)))
						 ("beat-type" nil ,(timesig-den (meas-timesig m))))))
				   ,@(when (and (= mn 1) (> ns 1)) `(("staves" nil ,ns)))
				   ,.(loop for c in (sort cc #'< :key #'third) for (s l o) = (lookup (second c) +xml-clefs+) collect
				      `("clef" ,(when (> ns 1) `("number" ,(third c)))
					("sign" nil ,s)
					,@(when l `(("line" nil ,l)))
					,@(when o `(("clef-octave-change" nil ,o)))))))))
		       ,.(loop with nv = (length (meas-voices m)) and ts = (meas-timesig m)
			  for v in (meas-voices m) and vn from 0
			  for b = (getprop m :barline) and fi = nil then t
			  when fi collect `("backup" nil ("duration" nil ,(* (- (meas-endoff m) (meas-off m)) (timesig-beat* ts) dv)))
			  nconc (loop
				 with tv
				 for e in v and mfi = t then nil nconc
				 (loop with ch = (chordp e)
				       for fi = t then nil
				       and no in (if (notep e) (if ch (event-writtennotes e) (list (event-writtennote e))) '(nil))
				       and o in (if (notep e) (if ch (event-notes* e) (list (event-note* e))) '(nil))
				       and ac in (if (notep e) (if ch (event-accs e) (list (event-acc e))) '(nil))
				       and aac in (if (notep e) (if ch (event-addaccs e) (list (event-addacc e))) '(nil))
				       and tr in (if (notep e) (if (consp (event-tiert e)) (event-tiert e) (list (event-tiert e))) '(nil))
				       and tl in (if (notep e) (if (consp (event-tielt e)) (event-tielt e) (list (event-tielt e))) '(nil))
				       do (prenconc (sort (mapcar #'rest (getmarks e :starttup)) #'> :key #'first) tv)
				       nconc (when (and fi (not mfi))
					       (loop for c in (getmarks e :clef) 
						     for (s l o) = (lookup (second c) +xml-clefs+)
						     unless (fourth c) collect
						     `("attributes" nil
						       ("clef" ,(when (> ns 1) `("number" ,(event-staff e)))
							("sign" nil ,s)
							,@(when l `(("line" nil ,l)))
							,@(when o `(("clef-octave-change" nil ,o)))))))
				       nconc (when fi (loop for x in (getmarks e :startwedge>) collect
							    `("direction" ("placement" ,(if (eq (third x) :up) "above" "below"))
							      ("direction-type" nil
							       ("wedge" (("type" "diminuendo") ("number" ,(getnum (cons (second x) (event-voice* e)) wlvl)))))
							      ,@(when (> ns 1) `(("staff" nil ,(event-staff e)))))))
				       nconc (when fi (loop for x in (getmarks e :startwedge<) collect
							    `("direction" ("placement" ,(if (eq (third x) :up) "above" "below"))
							      ("direction-type" nil
							       ("wedge" (("type" "crescendo") ("number" ,(getnum (cons (second x) (event-voice* e)) wlvl)))))
							      ,@(when (> ns 1) `(("staff" nil ,(event-staff e)))))))
				       nconc (when fi (loop for xxx in (getmarks e :start8up-) collect
							    `("direction" nil
							      ("direction-type" nil
							       ("octave-shift" (("type" "down") ("number" ,(getnum (event-staff e) olvl))))))))				     
				       nconc (when fi (loop for xxx in (getmarks e :start8down-) collect
							    `("direction" nil
							      ("direction-type" nil
							       ("octave-shift" (("type" "up") ("number" ,(getnum (event-staff e) olvl))))))))
				       nconc (when fi (loop for x in (getmarks e :starttext-) collect
							    `("direction" ("placement" ,(ecase (third x) (:up "above") (:down "below")))
							      ("direction-type" nil ("words" ,+xml-textstyle+ ,(fourth x))))
							    collect
							    `("direction" ("placement" ,(if (eq (third x) :up) "above" "below"))
							      ("direction-type" nil
							       ("dashes" (("type" "start") ("number" ,(getnum (cons (second x) (event-voice* e)) tlvl))))))))
				       nconc (when fi (loop for (m . i) in +xml-words+ when (getmark e m) collect
							    (cons i m) into re
							    finally (when re (return (loop for i in
											   (mapcar #'car
												   (sort (delete-duplicates
													  re :key #'cdr :test #'equal)
													 #'msrt))
											   collect
											   `("direction" ("placement" "above")
											     ("direction-type" nil
											      ("words" ,+xml-textnotestyle+ ,i))
											     ,@(when (> ns 1) `(("staff" nil ,(event-staff e))))))))))
				       nconc (when fi
					       (loop for x in (nconc (getmarks e :text) (getmarks e :textdyn) (getmarks e :textnote) (getmarks e :texttempo)) collect
						     `("direction" ("placement" ,(ecase (second x) (:up "above") (:down "below")))
						       ("direction-type" nil
							("words" ,(ecase (first x)
									 (:text +xml-textstyle+) (:textdyn +xml-dyntextstyle+)
									 (:textnote +xml-textnotestyle+) (:texttempo +xml-texttempostyle+))
							 ,(third x))))))
				       nconc (when (and fi xml-multinote-tremolo-kludge)
					       (let ((m (or (getmark e :tremolo) (getmark e :starttremolo))))
						 (when m (list `("direction" ("placement" "above")
								 ("direction-type" nil
								  ("words" nil ,(conc-stringlist
										 (nconc (loop repeat
											      (- (roundint (log (third m) 1/2)) 2)
											      collect "/")
											(when (eq (first m) :starttremolo)
											  '("-")))))))))))
				       nconc (when fi (loop for i in (loop for a in +xml-dynamics+ nconc (mapcar #'force-list (getmarks e (car a))))
							    for x = (lookup (first i) +xml-dynamics+)
							    when (listp x)
							    collect `("direction" ("placement" ,(if (>= vn (floor nv 2)) "below" "above"))
								      ("direction-type" nil
								       ("words" ,+xml-dyntextstyle+ ,(car x))))))
				       collect `("note" nil
						 ,@(when (event-grace e) `(("grace" ("slash" ,(if (< (event-grace e) 0) "yes" "no")))))
						 ,@(unless fi `(("chord" nil)))
						 ,@(when (and (notep e) (not pc))
							 `(("pitch" nil
							    ("step" nil ,(svref +xml-num-note+ (mod no 12)))
							    ,@(unless (and (= ac 0) (= aac 0))
								      `(("alter" nil
									 ,(let ((a (+ ac aac)))
									       (if (integerp a) a (format nil "~,1F" a))))))
							    ("octave" nil ,(1- (floor no 12))))))
						 ,@(when (and (notep e) pc)
							 `(("unpitched" nil
							    ("display-step" nil ,(svref +xml-num-perc-note+ (mod no 12)))
							    ("display-octave" nil ,(floor (1- no) 12)))))
						 ,@(when (restp e) '(("rest" nil)))
						 ,@(unless (event-grace e) `(("duration" nil ,(* (if (getmark e :measrest)
												     (/ (timesig-num ts) (timesig-den ts))
												     (event-writtendur e ts))
												 dv))))
						 ,@(when tl '(("tie" ("type" "stop"))))
						 ,@(when tr '(("tie" ("type" "start"))))
						 ,@(when (> nv 1) `(("voice" nil ,(event-voice* e))))
						 ,@(unless (getmark e :measrest) `(("type" nil ,(lookup (event-writtendur* e ts) +xml-num-durtype+))))
						 ,.(unless (getmark e :measrest) (loop repeat (nth-value 1 (event-writtendur* e ts)) collect '("dot" nil)))
						 ,@(let ((ca (getmark e (list :cautacc o))))
							(when (and (notep e) (not pc) (not tl)
								   (not (getmark e (list :hideacc o)))
								   (or (getmark e (list :showacc o)) (/= ac 0) (/= aac 0) ca))
							  `(("accidental" ,(when ca '("cautionary" "yes"))
							     ,(svref (svref +xml-num-acctype+ (+ ac 2)) (1+ (* aac 2)))))))
						 ,@(when (event-tup e)
							 (let ((tt (apply #'* (force-list (event-tup e)))))
							   `(("time-modification" nil
							      ("actual-notes" nil ,(numerator tt))
							      ("normal-notes" nil ,(denominator tt))
							      ,@(let ((ty (fourth (first tv))))
								     (multiple-value-bind (c1 c2) (event-writtendur* e ts)
								       (unless (equal (cons c1 c2) ty)
									 `(("normal-type" nil ,(lookup (car ty) +xml-num-durtype+))
									   ,.(when (> (cdr ty) 0)
									       (loop repeat (cdr ty) collect '("normal-dot" nil)))))))))))
						 ,@(let ((nh (or (when (and xml-harmonic-kludge (getmark e (list :harmonic :touched o))) :harmonic)
								 (let ((x (getmark e :notehead)))
								   (when (and x (= (third x) o)) (second x))))))
							(when nh `(("notehead" ,(when (eq nh :harmonic) '("filled" "no")) ,(lookup nh +xml-noteheads+)))))
						 ,@(when (> ns 1) `(("staff" nil ,(event-staff e))))
						 ,.(when (notep e)
						     (let ((bc (min (event-beamlt e) (event-beamrt e))))
						       (nconc
							(loop for i from 1 to bc collect `("beam" ("number" ,i) "continue"))
							(loop for i from (1+ bc) to (event-beamlt e) collect `("beam" ("number" ,i) "end"))
							(loop for i from (1+ bc) to (event-beamrt e) collect `("beam" ("number" ,i) "begin")))))
						 ,@(let ((ntr (when tr '(("tied" ("type" "start")))))
							 (ntl (when tl '(("tied" ("type" "stop")))))
							 (etup (when fi (loop for u in (sort (mapcar #'rest (getmarks e :endtup)) #'> :key #'first) collect
									      `("tuplet" (("type" "stop") ("number" ,(first u)))))))
							 (stup (when fi (loop for u in (sort (mapcar #'rest (getmarks e :starttup)) #'< :key #'first) collect
									      `("tuplet" (("type" "start") ("number" ,(first u))
											  ("bracket" ,(if (third u) "yes" "no"))
											  ,@(when (eq *tuplet-style* :ratio) '(("show-number" "both"))))
										("tuplet-actual" nil ("tuplet-number" nil ,(caadr u)))
										,@(when (eq *tuplet-style* :ratio)
											`(("tuplet-normal" nil ("tuplet-number" nil ,(cdadr u)))))))))
							 (orn (when fi (let ((z (nconc (mapcan #'car
											       (sort (delete-duplicates
												      (loop for (m . i) in +xml-ornaments+
													    for x = (getmark e m)
													    when x collect
													    (cons (nconc
														   (list `(,i nil))
														   (let* ((f (force-list x))
															  (a (if (eq (first f) :startlongtrill-)
																 (fifth f)
																 (third f))))
														     (when a
														       (list `("accidental-mark" nil
															       ,(ecase a
																       (-2 "flat-flat")
																       (-3/2 "three-quarters-flat")
																       (-1 "flat")
																       (-1/2 "quarter-flat")
																       (0 "natural")
																       (1/2 "quarter-sharp")
																       (1 "sharp")
																       (3/2 "three-quarters-sharp")
																       (2 "sharp-sharp")))))))
														  m))
												      :key #'cdr :test #'equal)
												     #'msrt))
										       (when (getmark e :startlongtrill-)
											 (list '("wavy-line" ("type" "start"))))
										       (when (getmark e :endlongtrill-)
											 (list '("wavy-line" ("type" "stop"))))
										       (unless xml-1note-tremolo-kludge
											 (let ((m (getmark e :tremolo)))
											   (when m (list `("tremolo" nil
													   ,(- (roundint (log (third m) 1/2)) 2)))))))))
									 (when z `(("ornaments" nil ,@z))))))
							 (tch (let ((z (nconc (when fi (mapcar #'car
											       (sort (delete-duplicates
												      (loop for (m . i) in +xml-technicals+
													    when (getmark e m) collect
													    (cons (if (listp i) i (list i nil)) m))
												      :key #'cdr :test #'equal)
												     #'msrt))) 
									      (when (and (not xml-harmonic-kludge) (getmark e (list :harmonic :touched o)))
										(list '("harmonic" nil ("artificial" nil) ("touching-pitch" nil))))
									      (when (and (not xml-harmonic-kludge) (getmark e (list :harmonic :sounding o)))
										(list `("harmonic" nil
											(,(if (getmark e '(:harmonic :touched)) "artificial" "natural") nil)
											("sounding-pitch" nil))))
									      (when (getmark e (list :harmonic :sounding o))
										(list '("harmonic" nil ("natural" nil) ("sounding-pitch" nil))))
									      (when (and (not xml-harmonic-kludge)
											 (not (getmark e (list :harmonic :touched o)))
											 (not (getmark e (list :harmonic :sounding o)))
											 (or (getmark e (list :harmonic :touched))
											     (getmark e (list :harmonic :sounding))))
										(list `("harmonic" nil
											(,(if (getmark e '(:harmonic :touched)) "artificial" "natural") nil)
											("base-pitch" nil)))))))
								(when z `(("technical" nil ,@z)))))
							 (art (when fi (loop for (m . i) in +xml-articulations+ when (getmark e m) collect
									     (cons `(,i nil) m) into re
									     finally (when re (return `(("articulations" nil
													 ,@(mapcar #'car
														   (sort (delete-duplicates
															  re :key #'cdr :test #'equal)
															 #'msrt)))))))))
							 (dyn (when fi (loop for i in
									   (loop for a in +xml-dynamics+ nconc (mapcar #'force-list (getmarks e (car a))))
									   for x = (lookup (first i) +xml-dynamics+)
									   unless (listp x) collect `(,x nil) into re
									   finally (when re (return `(("dynamics" nil ,@re)))))))
						       (gls (when fi (nconc (when (getmark e '(:glissando :before)) (list '("glissando" ("type" "stop"))))
									    (when (getmark e '(:glissando :after)) (list '("glissando" ("type" "start"))))
									    (when (getmark e '(:portamento :before)) (list '("slide" ("type" "stop"))))
									    (when (getmark e '(:portamento :after)) (list '("slide" ("type" "start")))))))
						       (fer (when (and fi (getmark e :fermata)) '(("fermata" nil))))
						       (arp (when fi (cond ((getmark e '(:arpeggio :up)) '(("arpeggiate" ("direction" "up"))))
									   ((getmark e '(:arpeggio :down)) '(("arpeggiate" ("direction" "down"))))
									   ((getmark e :arpeggio) '(("arpeggiate" nil))))))
						       (eslr (when fi (loop for m in (sort (delete-duplicates
											    (nconc (getmarks e :endslur-) (mapcar #'force-list (getmarks e :endgraceslur-)))
											    :key #'cdr :test #'equal) #'ssrt)
									    collect `("slur" (("type" "stop") ("number" ,(remnum (cons (second m) (event-voice* e))
																 slrlvl)))))))
						       (sslr (when fi (loop for m in (sort (delete-duplicates
											    (nconc (getmarks e :startslur-) (mapcar #'force-list
																    (getmarks e :startgraceslur-)))
											    :key #'cdr :test #'equal) #'ssrt)
									    collect `("slur" (("type" "start") ("number" ,(getnum (cons (second m) (event-voice* e))
																  slrlvl))
											      ,@(when (eq (third m) :dotted)
												      '(("line-type" "dotted")))))))))
						      (when (or ntr ntl etup stup eslr sslr orn tch art dyn gls fer arp)
							`(("notations" nil ,@ntl ,@ntr ,@etup ,@stup ,@eslr ,@sslr
							   ,@orn ,@tch ,@art ,@dyn ,@gls ,@arp ,@fer)))))
				       nconc (when fi (loop for x in (getmarks e :endwedge<) collect
							    `("direction" nil ;;("placement" ,(if (eq (third x) :up) "above" "below"))
							      ("direction-type" nil
							       ("wedge" (("type" "stop") ("number" ,(remnum (cons (second x) (event-voice* e)) wlvl))))))))
				       nconc (when fi (loop for x in (getmarks e :endwedge>) collect
							    `("direction" nil ;;("placement" ,(if (eq (third x) :up) "above" "below"))
							      ("direction-type" nil
							       ("wedge" (("type" "stop") ("number" ,(remnum (cons (second x) (event-voice* e)) wlvl))))))))
				       nconc (when fi (loop for xxx in (getmarks e :end8up-) collect
							    `("direction" nil
							      ("direction-type" nil
							       ("octave-shift" (("type" "stop") ("number" ,(remnum (event-staff e) olvl))))))))
				       nconc (when fi (loop for xxx in (getmarks e :end8down-) collect
							    `("direction" nil
							      ("direction-type" nil
							       ("octave-shift" (("type" "stop") ("number" ,(remnum (event-staff e) olvl)))))))) 
				       nconc (when fi (loop for x in (getmarks e :endtext-) collect
							    `("direction" nil
							      ("direction-type" nil
							       ("dashes" (("type" "stop") ("number" ,(remnum (cons (second x) (event-voice* e)) tlvl))))))))
				       do (let ((ns (mapcar #'rest (getmarks e '(:endtup)))))
					    (setf tv (delete-if (lambda (x) (find (first x) ns)) tv)))))
			  when b collect `("barline" nil
					   ("bar-style" nil ,(lookup (second b) +xml-barlines+))
					   ,@(when (find (second b) '(:repeatleft :repeatleftright)) '(("repeat" ("direction" "backward"))))
					   ,@(when (find (second b) '(:repeatright :repeatleftright)) '(("repeat" ("direction" "forward")))))))))))
	   f))))))

(defmacro save-xmlsibelius (parts header filename options #|process view|#)
  `(let ((*xml-1note-tremolo-kludge* t)
	 (*xml-multinote-tremolo-kludge* t)
	 (*xml-harmonic-kludge* t)
	 (*xml-partgroups-kludge* t))
    (format t ";; WARNING: There are serious issues importing MusicXML tuplet rhythms into Sibelius~%")
    (save-xml ,parts ,header ,filename ,options)))
(defmacro save-xmlfinale (parts header filename options #|process view|#)
  `(let ((*xml-1note-tremolo-kludge* t)
	 (*xml-multinote-tremolo-kludge* t)
	 (*xml-harmonic-kludge* t)
	 (*xml-partgroups-kludge* nil))
    (save-xml ,parts ,header ,filename ,options)))

