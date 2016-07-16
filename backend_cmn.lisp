;; -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**************************************************************************************************
;; FOMUS
;; backend_cmn.lisp
;;**************************************************************************************************

(in-package :fomus)
(compile-settings)

#+sbcl (eval-when (:compile-toplevel :load-toplevel :execute) (require :sb-posix))

(eval-when (:load-toplevel :execute)
  (defparameter *cmn-view-exe* +ghostview-exe+))
(defparameter *cmn-view-opts* #-(or darwin macos) nil #+(or darwin macos) '("/Applications/Preview.app"))

(defparameter +cmn-comment+ ";; -*- lisp -*-~%;; CMN score file~%;; ~A v~A.~A.~A~%~%")

(defparameter +cmn-num-note+ (vector "C" nil "D" nil "E" "F" nil "G" nil "A" nil "B"))
(defparameter +cmn-num-acc+ (vector 'double-flat 'flat 'natural 'sharp 'double-sharp))
(defparameter +cmn-num-accq+ (vector (vector nil 'double-flat) (vector 'flat-down 'flat 'natural-down) (vector 'natural-down 'natural 'natural-up)
				     (vector 'natural-up 'sharp 'sharp-up) (vector nil 'double-sharp)))

(defparameter +cmn-barlines+ '((nil . bar) (:single . bar) (:double . interior-double-bar) (:final . double-bar)
			       (:repeatleft . end-repeat-bar) (:repeatright . begin-repeat-bar) (:repeatleftright . begin-and-end-repeat-bar)
			       (:invisible . (bar invisible))))

(defparameter +cmn-durations+ '((1/16 . 64th) (3/32 . 64th.)
				(1/8 . 32nd) (3/16 . 32nd.)
				(1/4 . s) (3/8 . s.) (7/16 . s..)
				(1/6 . ts) 
				(1/2 . e) (3/4 . e.) (7/8 . e..)
				(1/3 . te) 
				(1 . q) (3/2 . q.) (7/4 . q..)
				(2/3 . tq) 
				(2 . h) (3 . h.) (7/2 . h..)
				(4/3 . th)
				(4 . w) (6 . w.)
				(8 . dw)))
(defparameter +cmn-restdurs+ '((1/32 . one-twenty-eighth-rest)
			       (1/16 . sixty-fourth-rest)
			       (1/8 . thirty-second-rest)
			       (1/4 . sixteenth-rest) (3/8 . dotted-sixteenth-rest)
			       (1/2 . eighth-rest) (3/4 . dotted-eighth-rest)
			       (1 . quarter-rest) (3/2 . dotted-quarter-rest) 
			       (2 . half-rest) (3 . dotted-half-rest)
			       (4 . whole-rest) (6 . dotted-whole-rest)
			       (8 . double-whole-rest)))
(defparameter +cmn-restdursex+ '((1/128 . 'draw-128th-rest)
				 (1/64 . 'draw-64th-rest)
				 (1/32 . 'draw-32nd-rest)
				 (1/16 . 'draw-16th-rest)
				 (1/8 . 'draw-8th-rest)
				 (1/4 . 'draw-quarter-rest) 
				 (1/2 . 'draw-half-rest) 
				 (1 . 'draw-whole-rest)  
				 (2 . 'draw-double-whole-rest)))

(defparameter +cmn-clefs+ '((:subbass-8dn . sub-bass) (:bass-8dn . double-bass) (:c-baritone-8dn . baritone-c) (:f-baritone-8dn . baritone-f) (:tenor-8dn . tenor)
			    (:subbass . sub-bass) (:alto-8dn . alto) (:bass . bass) (:mezzosoprano-8dn . mezzo-soprano) (:c-baritone . baritone-c) (:f-baritone . baritone-f)
			    (:soprano-8dn . soprano) (:tenor . tenor) (:subbass-8up . sub-bass) (:treble-8dn . tenor-treble) (:alto . alto) (:bass-8up . bass)
			    (:mezzosoprano . mezzo-soprano) (:c-baritone-8up . baritone-c) (:f-baritone-8up . baritone-f) (:soprano . soprano) (:tenor-8up . tenor)
			    (:treble . treble) (:alto-8up . alto) (:mezzosoprano-8up . mezzo-soprano) (:soprano-8up . soprano) (:treble-8up . double-treble)
			    (:percussion . percussion)))

(defparameter +cmn-textstyleexc+ '((font-name "Times-Bold") (font-scaler 1/2)))
(defparameter +cmn-textstyle+ '((font-name "Times-Italic") (font-scaler 1/2)))
(defparameter +cmn-textnotestyle+ '((font-name "Times-Italic") (font-scaler 1/2)))
(defparameter +cmn-texttempostyle+ '((font-name "Times-Bold") (font-scaler 1/2)))

(defparameter +cmn-up+ '(y (lambda (ma no sc &optional ju) (declare (ignore ma sc ju)) (+ (staff-y0 no) 1))))
(defparameter +cmn-down+ '(y (lambda (ma no sc &optional ju) (declare (ignore ma sc ju)) (- (staff-y0 no) 1))))

(defparameter +cmn-options+ '((automatic-rests nil) (implicit-accidental-duration 1) (implicit-accidental-style :new-style)
			      (automatic-beams nil) (automatic-octave-signs nil) (automatic-ties nil) (automatic-bars nil)
			      (automatic-beat-subdivision-numbers nil) (automatic-naturals nil)))
(defparameter +cmn-changeableopts+ '((all-output-in-one-file t) (size 24) (text-connecting-pattern '(5 10))))

;; left out: (:leftheel . ...) (:rightheel . ...) (:lefttoe . ...) (:righttoe . ...)|
(defparameter +cmn-marks+
  `((:accent . accent) (:marcato . marcato) (:staccatissimo . (text "!" ,@+cmn-textstyleexc+ ,+cmn-up+)) (:staccato . staccato) (:tenuto . tenuto)
    (:portato . (text "|." ,@+cmn-textstyleexc+ ,+cmn-up+)) (:upbow . up-bow) (:downbow . down-bow)
    (:thumb . thumb) (:open . open-note) (:stopped . stopped-note) ((:breath :after) . breath-mark) (:fermata . fermata)
    (:leftheel . (text "l heel" ,@+cmn-textstyleexc+ ,+cmn-up+)) (:rightheel . (text "r heel" ,@+cmn-textstyleexc+ ,+cmn-up+))
    (:lefttoe . (text "l toe" ,@+cmn-textstyleexc+ ,+cmn-up+)) (:righttoe . (text "r toe" ,@+cmn-textstyleexc+ ,+cmn-up+))))

(defparameter +cmn-noteheads+
  '((:harmonic . (note-head :artificial-harmonic)) (:diamond . (note-head :diamond-1)) (:x . (note-head :x)) (:xcircle . (note-head :circled-x))
    (:triangle . (note-head :triangle)) (:slash . (note-head :slash))
    (:touched . (note-head :artificial-harmonic)) (:sounding . open-note)))

;; (:arpeggio . ...) (:pizz . ...) (:arco . ...)
;; ((:glissando :after) . ...) ((:portamento :after) . ...) <-- begin/end marks, use setf gliss- and -gliss

(defparameter +cmn-dynamics+
  '((:pppppp . (dynamic "pppppp")) (:ppppp . (dynamic "ppppp")) (:pppp . pppp) (:ppp . ppp) (:pp . pp) (:p . p) (:mp . mp)
    (:ffffff . (dynamic "ffffff")) (:fffff . (dynamic "fffff")) (:ffff . ffff) (:fff . fff) (:ff . ff) (:f . f) (:mf . mf)
    (:sff . sff) (:spp . spp) (:sf . sf) (:sp . sp) (:fp . fp) (:rfz . rfz) (:sfz . sfz)))

(defparameter +cmn-trmarks+
  '((:prall . inverted-mordent) (:trill . trill) (:mordent . mordent) (:startlongtrill- . (trill))))

(defparameter +cmn-keysigs+
  '((:cmaj . c-major) (:amin . a-minor)
    (:gmaj . g-major) (:emin . e-minor)
    (:dmaj . d-major) (:bmin . b-minor)
    (:amaj . a-major) (:f+min . fs-minor)
    (:emaj . e-major) (:c+min . cs-minor) 
    (:bmaj . b-major) (:g+min . gs-minor) (:c-maj . cf-major) (:a-min . af-minor)
    (:f+maj . fs-major) (:d+min . ds-minor) (:g-maj . gf-major) (:e-min . ef-minor)
    (:c+maj . cs-major) (:a+min . as-minor) (:d-maj . df-major) (:b-min . bf-minor)
    (:a-maj . af-major) (:fmin . f-minor)
    (:e-maj . ef-major) (:cmin . c-minor)
    (:b-maj . bf-major) (:gmin . g-minor)
    (:fmaj . f-major) (:dmin . d-minor)))

(defun internalize (x)
  (typecase x
    (keyword x)
    (symbol (intern (symbol-name x)))
    (list (mapcar #'internalize x))
    (otherwise x)))

(defparameter +cmn-out-ext+ "eps")

(defun view-cmn (filename options view)
  (when (not *cmn-exists*) ;; for viewing only
    (format t ";; ERROR: Common Music Notation required for CMN compiling/viewing~%")
    (return-from view-cmn))
  (when (>= *verbose* 1) (out (if view ";; Compiling/opening ~S for viewing...~%" ";; Compiling ~S...~%") filename))
  (destructuring-bind (&key view-exe view-opts out-ext &allow-other-keys) options
    (flet ((er (str)
	     (format t ";; ERROR: Error ~A CMN file~%" str)
	     (return-from view-cmn)))
      (ignore-errors (delete-file (change-filename filename :ext (or out-ext +cmn-out-ext+))))
      (#+cmu unix:unix-chdir #+sbcl sb-posix:chdir #+openmcl ccl:cwd #+allegro excl:chdir #+lispworks hcl:change-directory #+clisp ext:cd
	     (change-filename filename :name nil :ext nil))
      (if (ignore-errors (load filename))
	  (progn
	    (unless (probe-file (change-filename filename :ext (or out-ext +cmn-out-ext+))) (er "compiling"))
	    (when view
	      (unless #+(or cmu sbcl openmcl) (ignore-errors
						(#+cmu extensions:run-program #+sbcl sb-ext:run-program #+openmcl ccl:run-program
						       (or view-exe *cmn-view-exe*)
						       (append (or view-opts *cmn-view-opts*)
							       (list (change-filename filename :ext (or out-ext +cmn-out-ext+))))
						       :wait nil))
		      #+clisp (eql (ignore-errors
				     (ext:run-program
				      (or view-exe *cmn-view-exe*)
				      :arguments
				      (append (or view-opts *cmn-view-opts*)
					      (list (change-filename filename :ext (or out-ext +cmn-out-ext+))))
				      :output nil
				      :wait nil)) 0)
		      #+lispworks (ignore-errors
				    (system:call-system (format nil "~A~{ ~A~}" (or view-exe *cmn-view-exe*)
								(append (or view-opts *cmn-view-opts*)
									(list (change-filename filename :ext (or out-ext +cmn-out-ext+))))
								:wait nil)))
		      #+allegro (eql (run-allegro-cmd
				      (apply #'vector (cons (or view-exe *cmn-view-exe*)
							    (cons (or view-exe *cmn-view-exe*)
								  (append (or view-opts *cmn-view-opts*)
									  (list (change-filename filename :ext (or out-ext +cmn-out-ext+))))))) nil nil)
				     0)
		      (er "viewing"))))
	(er "compiling")))))

;; multinote trems???
(defun save-cmn (parts header filename options process view)
  (when (>= *verbose* 1) (out ";; Saving CMN file ~S...~%" filename))
  (with-open-file (f filename :direction :output :if-exists :supersede)
    (destructuring-bind (&key score-attr out-ext &allow-other-keys) options
      (format f "~A" header)
      (let ((de 0) (phash (make-hash-table :test 'equal)))
	(flet ((cmndur (val m) (* val (timesig-beat* (meas-timesig m)) 4))
	       (cmnnote (wnum acc1 acc2 dur hide show caut #|grace|# #|harmt harms|# tl)	;; wdur is actual dur * beat * 4
		 (let ((acc (unless (or hide tl) (if *quartertones* (svref (svref +cmn-num-accq+ (+ acc1 2)) (1+ (* acc2 2))) (svref +cmn-num-acc+ (+ acc1 2))))))
		   (when (and acc caut) (setf acc (list acc 'in-parentheses)))
		   (when (and (equal acc 'natural) (not show)) (setf acc nil))
		   (nconc ;;(when (and grace (< grace 0)) (list 'grace-note))
		    ;;(when (and grace (>= grace 0)) (list 'appoggiatura))
		    (list (intern (conc-strings (svref +cmn-num-note+ (mod wnum 12))
						(case acc (flat "F") (natural "N") (sharp "S") (otherwise ""))
						(format nil "~D" (1- (truncate wnum 12))))))
		    (when dur (list (or (lookup dur +cmn-durations+) (list 'rq dur))))
		    (unless (member acc '(nil flat natural sharp)) (list acc)))))
	       (cmnname (p)
		 (incf de)
		 (intern
		  (conc-strings
		   (string-upcase
                    (conc-stringlist (when (part-name p)
                                       (loop for x across (part-name p)
                                          when (alpha-char-p x)
                                          collect (string x)))))
		   "-"
		   (string (code-char (+ 64 de)))))))
	  (let ((bv -1) (gv -1) (pv -1) (sv -1) (ouv -1) (odv -1) (w<v -1) (w>v -1) (tv -1) (rv -1) (iv -1) (uv -1) (rfun nil))
	    (let ((cmp (loop for p in parts nconc
			     (destructuring-bind (&key (cmn-partname (cmnname p)) &allow-other-keys) (part-opts p)
			       (loop with nvce = (mloop for e in (part-meas p) maximize (length (meas-voices e)))
				     and bbb = (make-hash-table :test 'eq)
				     and ggg = (make-hash-table :test 'eq)
				     and ppp = (make-hash-table :test 'eq)
				     and sss = (make-hash-table :test 'eq)
				     and ouuu = (make-hash-table :test 'eq)
				     and oddd = (make-hash-table :test 'eq)
				     and w<<< = (make-hash-table :test 'eq)
				     and w>>> = (make-hash-table :test 'eq)
				     and ttt = (make-hash-table :test 'eq)
				     and rrr = (make-hash-table :test 'eq)
				     and iii = (make-hash-table :test 'equal)
				     and uuu = (make-hash-table :test 'equal)
				     for vi from 0 below nvce nconc ; loop through voices
				     (loop with pna = (if (> nvce 1) (format nil "~A~D" cmn-partname (1+ vi)) (format nil "~A" cmn-partname)) ;; name-let voice
					   and ns = (instr-staves (part-instr p)) ; number of staves
					   for si from 1 to ns
					   for ipna = (intern (if (> ns 1)
								  (if (> nvce 1)
								      (format nil "~A~D" pna si)
								      (format nil "~A1~D" pna si))
								  pna))
					   do (setf (gethash p phash) (nconc (gethash p phash) (list ipna)))
					   collect
					   `(,ipna
					     (staff bar
					      ,@(when (and (<= si 1) (part-name p)) (list (list 'staff-name (part-name p))))
					      ,@(when (> vi 0)
						      (list (list 'tied-to (intern (if (> ns 1)
										       (format nil "~A1~D" cmn-partname si)
										       (if (> nvce 1) (format nil "~A1" cmn-partname) (format nil "~A" cmn-partname))))))) 
					      ,(lookup (second (find si (getprops p :clef) :key #'third)) +cmn-clefs+)
					      ,@(loop with o = 0 and st = 1 and gg = (cons nil nil) and pg = (cons nil nil) and sg = (make-hash-table) and wvy and oug and odg
						      and w>g = (make-hash-table) and w<g = (make-hash-table) and tg = (make-hash-table) and rg and ug = (make-hash-table)
						      and pre and tipl and gra1 and gra2 and tra = 0
						      for (m nxm) on (part-meas p) 
						      and stoff = 0 then (+ stoff lmdur)
						      for lmdur = (cmndur (- (meas-endoff m) (meas-off m)) m)
						      when (getprop m :startkey) nconc (let ((x (lookup (second (getprop (meas-timesig m) :keysig)) +cmn-keysigs+)))
											 (when x (list `(key ,x))))
						      when (getprop m :startsig) collect `(meter ,(timesig-num (meas-timesig m)) ,(timesig-den (meas-timesig m)))
						      nconc
						      (loop
						       with bb and ee ;;for (pre e nxe) on (cons nil (nth vi (meas-events m))) ;;while e
						       for (e nxe0) on (nth vi (meas-events m))
						       for nxe = (or nxe0 (when nxm (first (nth vi (meas-events nxm)))))
						       for co = (+ stoff (cmndur (- (event-off e) (meas-off m)) m))
						       ;; 						       and (wr . do) = (multiple-value-bind (wr0 do0) (event-writtendur* e (meas-timesig m)) (cons wr0 do0))
						       ;; 						       for enb0 = (if (and (or (getmark e :starttremolo) (getmark e :endtremolo))
						       ;; 									  ;;(< (cmndur (event-dur* e) m) 1) ;; correct, i think
						       ;; 									  (or (< (cmndur (event-dur* e) m) 1/2)
						       ;; 									      (> (nth-value 1 (event-writtendur* e (meas-timesig m))) 0)))
						       ;; 								     (event-nbeams e (meas-timesig m) 1) (event-nbeams e (meas-timesig m)))
						       for enb = (if (or (getmark e :starttremolo) (getmark e :endtremolo))
								     (event-nbeams e (meas-timesig m) 1) (event-nbeams e (meas-timesig m)))
						       for l = (and (notep e) (or (> (event-beamlt e) 0) #|(getmark e :endtremolo)|# (and (> enb 0) (getmark e :endtremolo))))
						       and r = (and (notep e) (or (> (event-beamrt e) 0) #|(getmark e :starttremolo)|# (and (> enb 0) (getmark e :starttremolo))))
						       and tipr = (or (and (chordp e) (some #'truep (event-tiert e)) (some #'null (event-tiert e)))
								      (and nxe (chordp nxe) (some #'truep (event-tielt nxe)) (some #'null (event-tielt nxe))))
						       do (setf st (or (third (getmark e '(:staff :voice))) st))
						       unless (or l (event-grace e)) do
						       (when ee (setf (car ee) '-beam ee nil)) ;;(event-off e)
						       (when r (setf bb e))
						       when (getmark e '(:glissando :before)) do (setf (car gg) (cdr gg))
						       when (getmark e '(:portamento :before)) do (setf (car pg) (cdr pg))
						       when (getmark e '(:glissando :after)) do (setf (cdr gg) e)
						       when (getmark e '(:portamento :after)) do (setf (cdr pg) e)
						       when (getmark e :start8up-) do (setf oug e tra -12)
						       when (getmark e :start8down-) do (setf odg e tra 12)
						       when (getmark e :starttremolo-) do (setf rg e)
						       when (and wvy (getmark e :endlongtrill-)) do (setf (second wvy) (+ co (cmndur (event-dur e) m)))
						       do
						       (loop for (xxx lvl) in (getmarks e :startslur-) do (setf (gethash lvl sg) e))
						       (loop for (xxx lvl) in (getmarks e :startwedge<) do (setf (gethash lvl w<g) e))
						       (loop for (xxx lvl) in (getmarks e :startwedge>) do (setf (gethash lvl w>g) e))
						       (loop for (xxx lvl) in (getmarks e :starttup) do (setf (gethash lvl ug) e))
						       when (= st si) nconc
						       (let ((not (if (getmark e :measrest)
								      (if (event-inv e) '(whole-measure-rest invisible) 'whole-measure-rest)
								      (multiple-value-bind (wr do) (event-writtendur* e (meas-timesig m))
									(let ((cd (cmndur (if (event-grace e) (event-gracedur e) (event-dur* e)) m))
									      (hq `(head-quarters ,(let ((wr0 (if (or (getmark e :starttremolo) (getmark e :endtremolo))
														  (* wr 2) wr)))
													(cond ((>= wr0 1) 4) ((>= wr0 1/2) 3) (t 1))))))
									  (flet ((doti (ee no)
										   (let ((h (gethash (cons ee no) iii)))
										     (list (if h
											       `(end-tie (svref tvect ,h))
											       `(setf (svref tvect ,(setf (gethash (cons ee no) iii) (incf iv))) (begin-tie)))))))
									    (nconc (if (restp e)
										       (let ((x (lookup cd +cmn-restdurs+)))
											 (if x (list x)
											     (progn (setf rfun t) (list 'restex (lookup wr +cmn-restdursex+) `(rq ,cd)))))
										       (let ((tcd (if (or (getmark e :starttremolo) (getmark e :endtremolo)) ;; correct
												      (* cd 2) cd)))
											 (if (chordp e)
											     (cons 'chord
												   (nconc
												    (loop
												     for n in (event-notes* e)
												     and w in (event-writtennotes e)
												     and a in (event-accs e)
												     and a2 in (event-addaccs e)
												     and tr in (event-tiert e)
												     and tl in (event-tielt e)
												     ;;for ha = (getmark e (list :harmonic :touched n))
												     ;;and hs = (getmark e (list :harmonic :sounding n))
												     for cl = (nconc
													       (cmnnote (+ w tra) a a2 nil 
															(getmark e (list :hideacc n))
															(getmark e (list :showacc n))
															(getmark e (list :cautacc n))
															#|(event-grace e)|#
															tl
															#|(getmark e (list :harmonic :touched n))|#
															#|(getmark e (list :harmonic :sounding n))|#)
													       (list hq)
													       (when (and tipl tl) (doti pre n))
													       (when (and tipr tr) (doti e n))
													       (loop for (xxx ty no) in (append (getmarks e :notehead)
																		(getmarks e :harmonic))
														     when (= no n)
														     collect (lookup ty +cmn-noteheads+)))
												     if (list1p cl) nconc cl else collect cl)
												    (list (or (lookup tcd +cmn-durations+) `(rq ,tcd)))))
											     (cmnnote (+ (event-writtennote e) tra)
												      (event-acc e) (event-addacc e)
												      tcd 
												      (getmark e (list :hideacc (event-note* e)))
												      (getmark e (list :showacc (event-note* e)))
												      (getmark e (list :cautacc (event-note* e)))
												      #|(event-grace e)|#
												      (event-tielt e)
												      #|(getmark e (list :harmonic :touched (event-writtennote e)))|#
												      #|(getmark e (list :harmonic :sounding (event-writtennote e)))|#))))
										   (list `(dots ,do)) ;; SPELLING
										   (when (and (notep e) (not (chordp e))) (list hq)) ;; SPELLING
										   (when (notep e) (list `(flags ,enb))) ;; SPELLING
										   (loop for (xxx lvl) in (nconc (getmarks e :starttup) (getmarks e :endtup)) collect
											 (let* ((h0 (gethash lvl ug)) ; h0 is event-obj
												(h (gethash (cons h0 lvl) uuu)))
											   (if h
											       `(-beat-subdivision (svref uvect ,h))
											       (let ((f0 (getmark h0 (list :starttup lvl))))
												 `(setf (svref uvect ,(setf (gethash (cons h0 lvl) uuu) (incf uv)))
												   (beat-subdivision-
												    (subdivision
												     ,(let ((f (third f0)))
													   (if (eq *tuplet-style* :ratio)
													       (format nil "~A:~A" (car f) (cdr f))
													       (format nil "~A" (car f)))))
												    ,@(unless (fourth f0) (list '(bracketed nil)))))))))
										   (when (and (notep e) (not (and tipl (chordp e)))
											      (if (chordp e) (some #'truep (event-tielt e)) (event-tielt e)))
										     (doti pre (unless (chordp e) (event-note* e))))
										   (when (and (notep e) (not (and tipr (chordp e)))
											      (if (chordp e) (some #'truep (event-tiert e)) (event-tiert e)))
										     (doti e (unless (chordp e) (event-note* e))))
										   (unless (or (chordp e) (restp e))
										     (loop with n = (event-note* e)
											   for (xxx ty no) in (append (getmarks e :notehead)
														      (getmarks e :harmonic))
											   when (= no n)
											   collect (lookup ty +cmn-noteheads+)))
										   (when (and (> co o) (not (event-grace e))) (list `(onset ,co)))
										   (when (and (or l r) (not (event-grace e)))
										     (let ((h (gethash bb bbb)))
										       (list (if h
												 (setf ee (list '-beam- `(svref bvect ,h))) ;; -beam- will be resetfed
												 `(setf (svref bvect ,(setf (gethash bb bbb) (incf bv))) (beam-))))))
										   (loop for i in
											 (sort (delete-duplicates
												(loop for (a1 . a2) in +cmn-marks+
												      nconc (mapcar (lambda (x) (cons a2 (force-list x))) (getmarks e a1)))
												:key #'cdr :test #'equal)
											       (lambda (x y) (cond
													       ((find (cadr x) +marks-withacc+) nil)
													       ((find (cadr y) +marks-withacc+) t)
													       (t (let ((x2 (caddr x)) (y2 (caddr y)))
														    (cond ((and (numberp x2) (numberp y2)) (< x2 y2))
															  (x2 t)))))))
											 collect (car i))
										   (loop for i in
											 (delete-duplicates
											  (loop for (a1 . a2) in +cmn-trmarks+
												nconc (mapcar (lambda (x)
														(let ((f (force-list x)))
														  (cons a2 (if (eq (first f) :startlongtrill-) (fifth f) (third f)))))
													      (getmarks e a1)))
											  :key #'cdr :test #'equal)
											 collect
											 `(,(if (listp (car i)) (caar i) (car i))
											   ,@(when (cdr i)
												   (list `(ornament-sign
													   (,(ecase (cdr i)
														    (-2 'double-flat)
														    (-3/2 'flat-down)
														    (-1 'flat)
														    (-1/2 'natural-down)
														    (0 'natural)
														    (1/2 'natural-up)
														    (1 'sharp)
														    (3/2 'sharp-up)
														    (2 'double-sharp))
													    (scale 1/2 1/2)))))
											   ,@(when (listp (car i))
												   (list '(wavy-line t)
													 (setf wvy (list 'wavy-time nil))))))
										   (when (or (getmark e :start8up-) (getmark e :end8up-))
										     (let ((h (gethash oug ouuu)))
										       (list (if h
												 `(end-octave-up (svref ouvect ,h))
												 `(setf (svref ouvect ,(setf (gethash oug ouuu) (incf ouv))) (begin-octave-up))))))
										   (when (or (getmark e :start8down-) (getmark e :end8down-))
										     (let ((h (gethash odg oddd)))
										       (list (if h
												 `(end-octave-down (svref odvect ,h))
												 `(setf (svref odvect ,(setf (gethash odg oddd) (incf odv))) (begin-octave-down))))))
										   (let ((x (getmark e :tremolo)))
										     (when x (list `(tremolo (tremolo-slashes ,(- (roundint (log (third x) 1/2)) 2 #|enb|#))))))
										   (let ((x (or (getmark e :endtremolo) (getmark e :starttremolo))))
										     (when x (list (let ((h (gethash rg rrr)))
												     (if h
													 `(end-tremolo (svref rvect ,h))
													 `(setf (svref rvect ,(setf (gethash rg rrr) (incf rv)))
													   (begin-tremolo (tremolo-beams ,(- (roundint (log (third x) 1/2)) 2 enb)))))))))
										   (cond ((getmark e '(:arpeggio :up)) (list '(arpeggio arrow-up)))
											 ((getmark e '(:arpeggio :down)) (list '(arpeggio arrow-down)))
											 ((getmark e :arpeggio) (list 'arpeggio)))
										   (loop for i in
											 (loop for a in +cmn-dynamics+ nconc (mapcar #'force-list (getmarks e (car a))))
											 collect (lookup (first i) +cmn-dynamics+))
										   (loop
										    for (xxx lvl) in (nconc (getmarks e :startwedge>) (getmarks e :endwedge>))
										    nconc (let ((h (gethash (gethash lvl w>g) w>>>)))
											    (list (if h
												      `(-diminuendo (svref wvect> ,h))
												      `(setf (svref wvect> ,(setf (gethash (gethash lvl w>g) w>>>) (incf w>v))) (diminuendo-))))))
										   (loop
										    for (xxx lvl) in (nconc (getmarks e :startwedge<) (getmarks e :endwedge<))
										    nconc (let ((h (gethash (gethash lvl w<g) w<<<)))
											    (list (if h
												      `(-crescendo (svref wvect< ,h))
												      `(setf (svref wvect< ,(setf (gethash (gethash lvl w<g) w<<<) (incf w<v))) (crescendo-))))))
										   (loop for x in (nconc (getmarks e :text) (getmarks e :textdyn) (getmarks e :textnote) (getmarks e :texttempo)) collect
											 (if (eq (first x) :textdyn)
											     `(dynamic ,(third x))
											     `(text ,(third x)
											       ,@(case (first x) (:text +cmn-textstyle+) (:textnote +cmn-textnotestyle+) (:texttempo +cmn-texttempostyle+))
											       ,(ecase (second x) (:up +cmn-up+) (:down +cmn-down+)))))
										   (loop for (m lvl dir txt) in (nconc (getmarks e :starttext-) (getmarks e :endtext-)) nconc
											 (let ((h (gethash (gethash lvl tg) ttt)))
											   (list (if h
												     `(-text (svref xvect ,h) ,.(when (eq m :starttext-) (list txt))
												       ,(case dir (:up +cmn-up+) (:down +cmn-down+)))
												     `(setf (svref xvect ,(setf (gethash (gethash lvl tg) ttt) (incf tv)))
												       (text- ,.(when (eq m :starttext-) (list txt))
													,(case dir (:up +cmn-up+) (:down +cmn-down+))))))))
										   (loop
										    for (xxx lvl) in (nconc (getmarks e :startslur-) (getmarks e :endslur-))
										    nconc (let ((h (gethash (gethash lvl sg) sss)))
											    (list (if h
												      `(-slur (svref svect ,h))
												      `(setf (svref svect ,(setf (gethash (gethash lvl sg) sss) (incf sv))) (slur-))))))
										   (loop
										    for (xxx wh) in (getmarks e :glissando)
										    nconc (let* ((gg0 (ecase wh (:before (car gg)) (:after (cdr gg))))
												 (h (gethash gg0 ggg)))
											    (list (if h
												      `(-glissando (svref gvect ,h))
												      `(setf (svref gvect ,(setf (gethash gg0 ggg) (incf gv))) (glissando-))))))
										   (loop
										    for (xxx wh) in (getmarks e :portamento)
										    nconc (let* ((pg0 (ecase wh (:before (car pg)) (:after (cdr pg))))
												 (h (gethash pg0 ppp)))
											    (list (if h
												      `(-portamento (svref pvect ,h))
												      `(setf (svref pvect ,(setf (gethash pg0 ppp) (incf pv))) (portamento-))))))
										   (when (and (restp e) (event-inv e)) (list 'invisible))
										   (when (and gra1 (not (event-grace e)))
										     (prog1 (list `(grace-note ,@(nreverse gra1))) (setf gra1 nil)))
										   (when (and gra2 (not (event-grace e)))
										     (prog1 (list `(grace-note (slashed nil) ,@(nreverse gra2))) (setf gra2 nil)))))))))) ;; support rests
							 (if (event-grace e) (progn (if (< (event-grace e) 0) (push not gra1) (push not gra2)) nil) (list not)))
						       and do (unless (event-grace e) (setf o (+ co (cmndur (event-dur* e) m))))
						       do (setf pre e tipl tipr)
						       when (getmark e :end8up-) do (setf tra 0)
						       when (getmark e :end8down-) do (setf tra 0)						       
						       finally (when ee (setf (car ee) '-beam)))
						      collect (let ((b (getprop m :barline)))
								(if (>= o (+ stoff lmdur))
								    (lookup (second b) +cmn-barlines+)
								    (list (lookup (second b) +cmn-barlines+)
									  `(onset ,(setf o (+ stoff lmdur)))))))))))))))
	      (setprints
	       (prin1 (internalize '(in-package cmn)) f)
	       (fresh-line f)
	       (prin1
		(internalize
		 `(cmn ,@(remove-duplicates (append +cmn-options+ score-attr +cmn-changeableopts+
						    (list (list 'output-file (change-filename filename :ext (or out-ext +cmn-out-ext+)))))
					    :key (lambda (x) (if (consp x) (first x) x)) :from-end t)
		   ,(let ((x `(let* ,(nconc
				      (if (>= bv 0) (list `(bvect (make-array ,(1+ bv)))))
				      (if (>= gv 0) (list `(gvect (make-array ,(1+ gv)))))
				      (if (>= pv 0) (list `(pvect (make-array ,(1+ pv)))))
				      (if (>= iv 0) (list `(tvect (make-array ,(1+ iv)))))
				      (if (>= sv 0) (list `(svect (make-array ,(1+ sv)))))
				      (if (>= ouv 0) (list `(ouvect (make-array ,(1+ ouv)))))
				      (if (>= odv 0) (list `(odvect (make-array ,(1+ odv)))))
				      (if (>= tv 0) (list `(xvect (make-array ,(1+ tv)))))
				      (if (>= uv 0) (list `(uvect (make-array ,(1+ uv)))))
				      (if (>= rv 0) (list `(rvect (make-array ,(1+ rv)))))
				      (if (>= w<v 0) (list `(wvect< (make-array ,(1+ w<v)))))
				      (if (>= w>v 0) (list `(wvect> (make-array ,(1+ w>v)))))
				      cmp)
			       (system ,.(mapcan (lambda (e) (gethash e phash)) parts))
			       ;; 			      ,@(labels ((pfn (pps &optional (grp 0))
			       ;; 					      (loop for e = (pop pps) ; e = part
			       ;; 						    while e
			       ;; 						    for gr = (delete-if (lambda (x) (< (second x) grp)) (getprops e :startgroup)) ; startgroups = grp or greater
			       ;; 						    if gr nconc (let* ((gg (first (sort gr #'< :key #'second)))
			       ;; 								       (gl (second gg))	; gl = level
			       ;; 								       (ps (pfn (loop for i = e then (pop pps) collect i until (getprop i (list :endgroup gl))) (1+ gl))))
			       ;; 								  (case (third gg)
			       ;; 								    ((:group :choirgroup) (list (append '(system bracket) ps)))
			       ;; 								    (:grandstaff (list (append '(system brace) ps)))
			       ;; 								    (otherwise (list (append '(system) ps)))))
			       ;; 						    else nconc (gethash e phash))))
			       ;; 					(pfn parts))
			       )))
			 (if rfun `(flet ((restex (draw &rest args) (let ((r (apply (quote rest) args))) (setf (draw-func r) draw) r))) ,x) x))))
		f)
	       (fresh-line f))))))))
  (when process (view-cmn filename options view)))
