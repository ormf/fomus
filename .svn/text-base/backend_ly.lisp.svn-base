;; -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**************************************************************************************************
;; FOMUS
;; backend_ly.lisp
;;**************************************************************************************************

(in-package :fomus)
(compile-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LILYPOND VIEWER

#+sbcl (eval-when (:compile-toplevel :load-toplevel :execute) (require :sb-posix))

(eval-when (:load-toplevel :execute)
  (defparameter *lilypond-exe*
    (or #+(or darwin macos) (find-exe "lilypond" "LilyPond.app/Contents/Resources/bin")
	#+(or darwin macos) (find-exe "lilypond.sh" "LilyPond.app")
	#+(or mswindows win32) (find-exe "lilypond.exe" "LilyPond")
	#-(or mswindows win32) (find-exe "lilypond" "LilyPond") 
	#+(or darwin macos) "lilypond.sh"
	#+(or mswindows win32) "lilypond.exe"
	#-(or mswindows win32) "lilypond"))
  (defparameter *lilypond-view-exe* #-(or mswindows win32) +ghostview-exe+ #+(or mswindows win32) +acroread-exe+))

(defparameter *lilypond-opts* #-(or (or darwin macos) mswindows win32) '("--ps") #+(or (or darwin macos) mswindows win32) '("--pdf"))
(defparameter *lilypond-out-ext* #-(or (or darwin macos) mswindows win32) "ps" #+(or (or darwin macos) mswindows win32) "pdf")
(defparameter *lilypond-view-opts* #-(or darwin macos) nil #+(or darwin macos) '("/Applications/Preview.app"))

(defun view-lilypond (filename options view)
  (when (>= *verbose* 1) (out (if view ";; Compiling/opening ~S for viewing...~%" ";; Compiling ~S...~%") filename))
  (destructuring-bind (&key exe view-exe opts view-opts out-ext &allow-other-keys) options
    (flet ((er (str)
	     (format t ";; ERROR: Error ~A LilyPond file~%" str)
	     (return-from view-lilypond)))
      (ignore-errors (delete-file (change-filename filename :ext (or out-ext *lilypond-out-ext*))))
      (#+cmu unix:unix-chdir #+sbcl sb-posix:chdir #+openmcl ccl:cwd #+allegro excl:chdir #+lispworks hcl:change-directory #+clisp ext:cd
	     (change-filename filename :name nil :ext nil))
      (if #+(or cmu sbcl openmcl) (ignore-errors
				    (#+cmu extensions:run-program #+sbcl sb-ext:run-program #+openmcl ccl:run-program
					   (or exe *lilypond-exe*)
					   (append (or opts *lilypond-opts*) (list filename))
					   :wait t))
	  #+clisp (eql (ignore-errors
			 (ext:run-program
			  (or exe *lilypond-exe*)
			  :arguments (append (or opts *lilypond-opts*) (list filename))
			  :output nil
			  :wait t))
		       0)
	  #+lispworks (ignore-errors
			(system:call-system (format nil "~A~{ ~A~}" 
						    (or exe *lilypond-exe*)
						    (append (or opts *lilypond-opts*) (list filename))
						    :wait t)))
	  #+allegro (eql (run-allegro-cmd (apply #'vector (cons (or exe *lilypond-exe*)
								(cons (or exe *lilypond-exe*)
								      (append (or opts *lilypond-opts*) (list filename)))))) 0)
	  (progn
	    (unless (probe-file (change-filename filename :ext (or out-ext *lilypond-out-ext*))) (er "compiling"))
	    (ignore-errors (delete-file (change-filename filename :ext "log")))
	    (unless (string= (or out-ext *lilypond-out-ext*) "tex") (ignore-errors (delete-file (change-filename filename :ext "tex"))))
	    (unless (string= (or out-ext *lilypond-out-ext*) "dvi") (ignore-errors (delete-file (change-filename filename :ext "dvi"))))
	    (unless (string= (or out-ext *lilypond-out-ext*) "ps") (ignore-errors (delete-file (change-filename filename :ext "ps")))) 
	    (unless (string= (or out-ext *lilypond-out-ext*) "pdf") (ignore-errors (delete-file (change-filename filename :ext "pdf"))))
	    (when view
	      (unless #+(or cmu sbcl openmcl) (ignore-errors
						(#+cmu extensions:run-program #+sbcl sb-ext:run-program #+openmcl ccl:run-program
						       (or view-exe *lilypond-view-exe*)
						       (append (or view-opts *lilypond-view-opts*)
							       (list (change-filename filename :ext (or out-ext *lilypond-out-ext*))))
						       :wait nil))
		      #+clisp (eql (ignore-errors
				     (ext:run-program
				      (or view-exe *lilypond-view-exe*)
				      :arguments (append (or view-opts *lilypond-view-opts*)
							 (list (change-filename filename :ext (or out-ext *lilypond-out-ext*))))
				      :output nil
				      :wait nil))
				   0)
		      #+lispworks (ignore-errors
				    (system:call-system (format nil "~A~{ ~A~}" 
								(or view-exe *lilypond-view-exe*)
								(append (or view-opts *lilypond-view-opts*)
									(list (change-filename filename :ext (or out-ext *lilypond-out-ext*))))
								:wait nil)))
		      #+allegro (eql (run-allegro-cmd
				      (apply #'vector (cons (or view-exe *lilypond-view-exe*)
							    (cons (or view-exe *lilypond-view-exe*)
								  (append (or view-opts *lilypond-view-opts*)
									  (list (change-filename filename :ext (or out-ext *lilypond-out-ext*))))))) nil nil)
				     0)
		      (er "viewing"))))
	  (er "compiling")))))

(defparameter *lilypond-version* nil)
(defparameter *lilyvers* t)
(defparameter +lilypond-defaultver+ '("2.10" . 210))
(defun lilypond-version (options usrv)
  (let ((v (or usrv *lilypond-version*)))
    (if v
	(let ((ve (ignore-errors
		    (+ (* 100 (parse-integer v :junk-allowed t))
		       (parse-integer v :start (1+ (position #\. v)) :junk-allowed t)))))
	  (if (and (>= ve 204) (<= ve 209)) ve
	      (progn (format t ";; WARNING: Unknown LilyPond version--assuming version ~A~%" (car +lilypond-defaultver+)) (cdr +lilypond-defaultver+))))
	(if (truep *lilyvers*)
	    (setf *lilyvers*
		  (destructuring-bind (&key exe &allow-other-keys) options
		    (flet ((er () (format t ";; WARNING: Can't determine LilyPond version--assuming version ~A~%" (car +lilypond-defaultver+)) (cdr +lilypond-defaultver+)))
		      (let ((os #+(or cmu sbcl openmcl lispworks) (make-string-output-stream)
				#+allegro (nth-value 1 (run-allegro-cmd (vector (or exe *lilypond-exe*) (or exe *lilypond-exe*) "-v")))
				#+clisp (ignore-errors
					  (ext:run-program
					   (or exe *lilypond-exe*)
					   :arguments (list "-v") :wait t :output :stream))))
			#+(or cmu sbcl openmcl) (ignore-errors (#+cmu extensions:run-program #+sbcl sb-ext:run-program #+openmcl ccl:run-program
								      (or exe *lilypond-exe*)
								      (list "-v") :wait t :output os))
			#+lispworks (ignore-errors
				      (system:call-system-showing-output
				       (format nil "~A~{ ~A~}"
					       (or exe *lilypond-exe*)
					       (list "-v"))
				       :prefix "" :show-cmd nil
				       :output-stream os
				       :wait t))
			(if (streamp os)
			    (let* ((out #+(or cmu sbcl openmcl lispworks) (get-output-stream-string os) #+(or clisp allegro) (ignore-errors (read-line os)))
				   (p (search "LilyPond " out)))
			      (if p (multiple-value-bind (n1 np) (parse-integer out :start (+ p 9) :junk-allowed t)
				      (+ (* n1 100) (parse-integer out :start (1+ np) :junk-allowed t)))
				  (er)))
			    (er))))))
	    *lilyvers*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LILYPOND BACKEND

(defparameter +lilypond-comment+ "% LilyPond score file~%% ~A v~A.~A.~A~%~%")

(defparameter +lilypond-vers+ "\\version \"~A.~A\"~%")
(defparameter +lilypond-defs+
  '("octUp = #(set-octavation 1)"
    "octReset = #(set-octavation 0)"
    "octDown = #(set-octavation -1)"
    "markAccIn = \\once \\override TextScript #'script-priority = #-100"
    "markOrnIn = \\once \\override Script #'script-priority = #-100"))
(defparameter +lilypond-defs-24+
  '("beamL = #(def-music-function (loc num) (number?) #{\\set stemLeftBeamCount = #$num #})"
    "beamR = #(def-music-function (loc num) (number?) #{\\set stemRightBeamCount = #$num #})"
    "beamLR = #(def-music-function (loc numl numr) (number? number?) #{\\set stemLeftBeamCount = #$numl \\set stemRightBeamCount = #$numr #})" 
    "textSpan = #(def-music-function (loc dir str) (number? string?) #{\\override TextSpanner #'direction = #$dir \\override TextSpanner #'edge-text = #(cons $str \"\") #})" 
    "noteHead = #(def-music-function (loc sty) (symbol?) #{\\once \\override NoteHead #'style = #$sty #})"
    ))
(defparameter +lilypond-defs-26+
  '("beamL = #(def-music-function (par loc num) (number?) #{\\set stemLeftBeamCount = #$num #})"
    "beamR = #(def-music-function (par loc num) (number?) #{\\set stemRightBeamCount = #$num #})"
    "beamLR = #(def-music-function (par loc numl numr) (number? number?) #{\\set stemLeftBeamCount = #$numl \\set stemRightBeamCount = #$numr #})" 
    "textSpan = #(def-music-function (par loc dir str) (number? string?) #{\\override TextSpanner #'direction = #$dir \\override TextSpanner #'edge-text = #(cons $str \"\") #})" 
    "noteHead = #(def-music-function (par loc sty) (symbol?) #{\\once \\override NoteHead #'style = #$sty #})"
    ))
(defparameter +lilypond-defs-28+
  '("beamL = #(define-music-function (par loc num) (number?) #{\\set stemLeftBeamCount = #$num #})"
    "beamR = #(define-music-function (par loc num) (number?) #{\\set stemRightBeamCount = #$num #})"
    "beamLR = #(define-music-function (par loc numl numr) (number? number?) #{\\set stemLeftBeamCount = #$numl \\set stemRightBeamCount = #$numr #})" 
    "textSpan = #(define-music-function (par loc dir str) (number? string?) #{\\override TextSpanner #'direction = #$dir \\override TextSpanner #'edge-text = #(cons $str \"\") #})" 
    "noteHead = #(define-music-function (par loc sty) (symbol?) #{\\once \\override NoteHead #'style = #$sty #})"
    ))

(defparameter +lilypond-num-note+ (vector "c" nil "d" nil "e" "f" nil "g" nil "a" nil "b"))
(defparameter +lilypond-num-acc+ (vector "eses" "es" "" "is" "isis"))
(defparameter +lilypond-num-accq+ (vector (vector nil "eses") (vector "eseh" "es" "eh") (vector "eh" "" "ih") (vector "ih" "is" "isih") (vector nil "isis")))
(defparameter +lilypond-barlines+ '((:single . "|") (:double . "||") (:final . "|.") (:repeatleft . ":|") (:repeatright . "|:") (:repeatleftright . ":|:") (:invisible . "")))

(defparameter +lilypond-marks+
  '((:accent . "->") (:marcato . "-^") (:staccatissimo . "-|") (:staccato . "-.") (:tenuto . "--") (:portato . "-_") (:upbow . "\\upbow") (:downbow . "\\downbow")
    (:thumb . "\\thumb") (:leftheel . "\\lheel") (:rightheel . "\\rheel") (:lefttoe . "\\ltoe") (:righttoe . "\\rtoe") (:open . "\\open")
    (:stopped . "-+") (:pizz . "^\"pizz.\"") (:arco . "^\"arco\"") ((:breath :after) . " \\breathe") ((:glissando :after) . "\\glissando")
    ((:portamento :after) . "\\glissando") ((:fermata :short) . "\\shortfermata") (:fermata . "\\fermata")
    ((:fermata :long) . "\\longfermata") ((:fermata :verylong) . "\\verylongfermata")))

(defparameter +lilypond-trmarks+
  '((:trill . "\\trill") (:startlongtrill- . "\\startTrillSpan") (:prall . "\\prall") (:mordent . "\\mordent")))

(defparameter *lilypond-text-markup* "\\markup{\\italic{~A}}")
(defparameter *lilypond-textdyn-markup* "\\markup{\\dynamic{\\italic{\\bold{~A}}}}")
(defparameter *lilypond-texttempo-markup* "\\markup{\\bold{\\huge{~A}}}")
(defparameter *lilypond-textnote-markup* "\\markup{\\italic{~A}}")
(defparameter *lilypond-textacc-markup* "\\markup{\\tiny{~A}}") ;; not user defined yet

(defparameter +lilypond-dyns+
  `((:pppppp . ,(list (format nil *lilypond-textdyn-markup* "pppppp"))) (:ppppp . ,(list (format nil *lilypond-textdyn-markup* "ppppp")))
    (:pppp . "\\pppp") (:ppp . "\\ppp") (:pp . "\\pp") (:p . "\\p") (:mp . "\\mp")
    (:mf . "\\mf") (:f . "\\f") (:ff . "\\ff") (:fff . "\\fff") (:ffff . "\\ffff")
    (:fffff . ,(list (format nil *lilypond-textdyn-markup* "fffff"))) (:ffffff . ,(list (format nil *lilypond-textdyn-markup* "ffffff")))
    (:fp . "\\fp") (:sf . "\\sf") (:sff . "\\sff") (:sp . "\\sp") (:spp . "\\spp") (:sfz . "\\sfz") (:rfz . "\\rfz")))

(defparameter +lilypond-noteheads+
  '((:harmonic . "harmonic") (:diamond . "diamond") (:x . "cross") (:xcircle . "xcircle") (:triangle . "triangle") (:slash . "slash")))

(defparameter +lilypond-clefs+
  '((:subbass-8dn . "\"subbass_8\"") (:bass-8dn . "\"bass_8\"") (:c-baritone-8dn . "\"baritone_8\"") (:f-baritone-8dn . "\"varbaritone_8\"") (:tenor-8dn . "\"tenor_8\"")
    (:subbass . "subbass") (:alto-8dn . "\"alto_8\"") (:bass . "bass") (:mezzosoprano-8dn . "\"mezzosoprano_8\"") (:c-baritone . "baritone") (:f-baritone . "varbaritone")
    (:soprano-8dn . "\"soprano_8\"") (:tenor . "tenor") (:subbass-8up . "\"subbass^8\"") (:treble-8dn . "\"treble_8\"") (:alto . "alto") (:bass-8up . "\"bass^8\"")
    (:mezzosoprano . "mezzosoprano") (:c-baritone-8up . "\"baritone^8\"") (:f-baritone-8up . "\"varbaritone^8\"") (:soprano . "soprano") (:tenor-8up . "\"tenor^8\"")
    (:treble . "treble") (:alto-8up . "\"alto^8\"") (:mezzosoprano-8up . "\"mezzosoprano^8\"") (:soprano-8up . "\"soprano^8\"") (:treble-8up . "\"treble^8\"")
    (:percussion . "percussion")))

(defparameter +lilypond-keysigs+
  '((:cmaj . "c \\major") (:amin . "a \\minor")
    (:gmaj . "g \\major") (:emin . "e \\minor")
    (:dmaj . "d \\major") (:bmin . "b \\minor")
    (:amaj . "a \\major") (:f+min . "fis \\minor")
    (:emaj . "e \\major") (:c+min . "cis \\minor") 
    (:bmaj . "b \\major") (:g+min . "gis \\minor") (:c-maj . "ces \\major") (:a-min . "aes \\minor")
    (:f+maj . "fis \\major") (:d+min . "dis \\minor") (:g-maj . "ges \\major") (:e-min . "ees \\minor")
    (:c+maj . "cis \\major") (:a+min . "ais \\minor") (:d-maj . "des \\major") (:b-min . "bes \\minor")
    (:a-maj . "aes \\major") (:fmin . "f \\minor")
    (:e-maj . "ees \\major") (:cmin . "c \\minor")
    (:b-maj . "bes \\major") (:gmin . "g \\minor")
    (:fmaj . "f \\major") (:dmin . "d \\minor")))

(defparameter *lilypond-filehead* nil)
(defparameter *lilypond-scorehead* nil)

(defun lilypond-string-escape (string)
  (if (position #\# string)
      (concatenate 'string "\"" string "\"")
      string))

(defun save-lilypond (parts header filename options process view)
  (when (>= *verbose* 1) (out ";; Saving LilyPond file ~S...~%" filename))
  (with-open-file (f filename :direction :output :if-exists :supersede)
    (destructuring-bind (&key filehead scorehead text-markup textdyn-markup texttempo-markup textnote-markup textacc-markup version &allow-other-keys) options
      (let ((ve (lilypond-version options version)))
	(format f "~A" header)
	(format f +lilypond-vers+ (floor ve 100) (mod ve 100))
	(let ((fh (or filehead *lilypond-filehead*)))
	  (when fh (loop for e in (force-list fh) do (format f "~A~%" e) finally (format f "~%")))) ;; user header
	(loop for e in (append +lilypond-defs+
			       (cond
				 ((>= ve 207) +lilypond-defs-28+)
				 ((>= ve 205) +lilypond-defs-26+)
				 (t +lilypond-defs-24+)))
	      do (format f "~A~%" e) finally (format f "~%")) ;; definitions
	(let ((de 0) (nms nil) (twrn nil))
	  (flet ((lynote (wnum acc1 acc2 caut show)
		   (let ((rs (let ((g (- (truncate wnum 12) 4)))
			       (cond ((< g 0) (make-string (- g) :initial-element #\,))
				     ((> g 0) (make-string g :initial-element #\'))
				     (t "")))))
		     (if *quartertones*
			 (conc-strings
			  (svref +lilypond-num-note+ (mod wnum 12))
			  (svref (svref +lilypond-num-accq+ (+ acc1 2)) (1+ (* acc2 2)))
			  rs
			  (when caut "?")
			  (when show "!"))
			 (conc-strings
			  (svref +lilypond-num-note+ (mod wnum 12))
			  (svref +lilypond-num-acc+ (+ acc1 2))
			  rs
			  (when caut "?")
			  (when show "!")))))
		 (lyname (p)
                   (incf de)
                   (conc-strings
                    (string-downcase
                     (conc-stringlist (when (part-name p)
                                        (loop for x across (part-name p)
                                           when (alpha-char-p x)
                                           collect (string x)))))
                    (conc-strings "Prt" (if (> de 26) (make-string 2 :initial-element (code-char (+ 64 (- de 26)))) (string (code-char (+ 64 de)))))))
		 (lyclef (c)
		   (lookup c +lilypond-clefs+)))
	    (loop
	     for p in parts 
	     do (destructuring-bind (&key (lily-partname (lyname p))
					  lily-parthead	;; extra header information for part (list of strings)
					  &allow-other-keys) (part-opts p) 
		  (let ((ns (instr-staves (part-instr p))))
		    (push lily-partname nms)
		    (format f "~A = {~%" lily-partname)
		    (when (part-name p) (format f (if (> ve 209) "  \\set Staff.instrumentName = ~S~%" "  \\set Staff.instrument = ~S~%") (part-name p)))
		    (when (part-abbrev p) (format f (if (> ve 209) "  \\set Staff.shortInstrumentName = ~S~%" "  \\set Staff.instr = ~S~%") (part-abbrev p)))
		    (when (or (null *timesig-style*) (eq *timesig-style* :fraction))
		      (if (> ns 1)
			  (loop for s from 1 to ns do
				(format f "  \\change Staff = ~A \\override Staff.TimeSignature #'style = #'()~%" (code-char (+ 64 s))))
			  (format f "  \\override Staff.TimeSignature #'style = #'()~%")))
		    (if (>= ve 209)
			(when (eq *tuplet-style* :ratio) (format f "  \\override TupletNumber #'text = #tuplet-number::calc-fraction-text~%"))
			(when (eq *tuplet-style* :ratio) (format f "  \\set tupletNumberFormatFunction = #fraction-tuplet-formatter~%")))
		    (format f "  \\autoBeamOff~%")
		    (if *acc-throughout-meas*
			(format f "  #(set-accidental-style 'default)~%")
			(format f "  #(set-accidental-style 'forget)~%"))
		    (if (> ns 1)
			(loop for (xxx cl s) in (sort (getprops p :clef) #'< :key #'third) do
			      (format f "  ~A\\clef ~A~%" (format nil "\\change Staff = ~A " (code-char (+ 64 s))) (lyclef cl)))
			(format f "  \\clef ~A~%" (lyclef (second (getprop p :clef)))))
		    (loop for e in lily-parthead do (format f "  ~A~%" e))
		    (format f "~%") 
		    (loop with nvce = (mloop for e in (part-meas p) maximize (length (meas-voices e)))
			  initially (format f "  << ")
			  for vce from 0 below nvce do
			  (format f "{")
			  (loop
			   with cdi = :u
			   for (m . nxm) on (part-meas p) and mn from 1
			   for ts = (meas-timesig m) do
			   (when (getprop m :startkey)
			     (let ((x (lookup (second (getprop ts :keysig)) +lilypond-keysigs+)))
			       (when x (if (> ns 1)
					   (loop for s from 1 to ns do
						 (format f "\\change Staff = ~A \\key ~A " (code-char (+ 64 s)) x))
					   (format f "\\key ~A " x)))))
			   (when (getprop m :startsig) (format f "\\time ~A/~A " (timesig-num ts) (timesig-den ts)))
			   (loop
			    for (pre e nxe) on (cons nil (or (nth vce (meas-voices m))
							     (list (make-restex nil :inv t :off (meas-off m) :dur (- (meas-endoff m) (meas-off m)) :marks '(:measrest)))))
			    while e
			    do (let ((fm (getmark e :measrest))
				     (trf (and (>= (or (nth-value 1 (event-writtendur* e ts)) 0) 2) (< ve 209))))
				 (when (getmark e '(:starttext- 2)) (setf twrn t))
				 (format f "~A "
					 (conc-strings
					  (let ((m (getmark e '(:voice :ord1324))))
					    (if (and m (null (fourth m)))
						(case (third m)
						  (1 (setf cdi :u) "\\voiceOne ")
						  (2 (setf cdi :d) "\\voiceTwo ")
						  (3 (setf cdi :u) "\\voiceThree ")
						  (4 (setf cdi :d) "\\voiceFour ")
						  (otherwise (setf cdi :u) "\\oneVoice "))
						""))
					  (let ((m (getmark e '(:staff :voice))))
					    (if (and m (> ns 1) (null (fourth m))) (format nil "\\change Staff = ~A " (code-char (+ 64 (third m)))) ""))
					  (let ((c (getmark e :clef)))
					    (if (and c (null (fourth c))) (format nil "\\clef ~A " (lyclef (second c)))
						""))
					  (cond ((or (getmark e :start8up-) (getmark e :8up)) "\\octUp ")
						((or (getmark e :start8down-) (getmark e :8down)) "\\octDown "))
					  (let ((m (getmark e '(:starttext- 1)))) ; can't have more than one at once
					    (if m (format nil "\\textSpan #~A #\"~A \" " (ecase (third m) (:up 1) (:down -1) (:nopos 0)) ;; test this nopos!?
							  (fourth m)) ""))
					  (let ((uu (sort (getmarks e :starttup) #'< :key #'second)))
					    (conc-stringlist
					     (loop for u in uu for r = (third u)
						   collect (format nil "\\times ~A/~A {" (cdr r) (car r)))))
					  (let ((z (getmark e :notehead)))
					    (if z (let ((y (lookup (second z) +lilypond-noteheads+)))
						    (if y (format nil "\\noteHead #'~A " y) ""))
						""))
					  (let ((g (event-grace e)))
					    (if g
						(let ((g1 (getmark e :startgrace))
						      (gs (getmark e :startgraceslur-)))
						  (cond ((and g1 (getmark e :endgrace))
							 (if (and gs (not (or (getmark e :slur-) (getmark e :startslur-))))
							     (if (< g 0) "\\acciaccatura " "\\appoggiatura ") "\\grace "))
							(g1 (if (and gs (not (or (getmark e :slur-) (getmark e :startslur-))))
								(if (< g 0) "\\acciaccatura {" "\\appoggiatura {") "\\grace {"))))
						""))
					  (cond ((getmark e '(:arpeggio :up)) "\\arpeggioUp ")
						((getmark e '(:arpeggio :down)) "\\arpeggioDown ")
						((getmark e :arpeggio) "\\arpeggioNeutral ")
						(t ""))
					  (if (some (lambda (aa) (let ((x (getmarks e (car aa))))
								   (some (lambda (z) (and z (listp z) (if (member (first z) +marks-long+) (fifth z) (third z)))) x)))
						    +lilypond-trmarks+)
					      (if (eq cdi :d) "\\markAccIn " "\\markOrnIn ")
					      "")
					  (let ((l (and (notep e) (notep pre) (> (event-beamlt e) 0) (/= (min (event-nbeams e ts) (event-nbeams pre ts)) (event-beamlt e))))
						(r (and (notep e) (notep nxe) (> (event-beamrt e) 0) (/= (min (event-nbeams e ts) (event-nbeams nxe ts)) (event-beamrt e)))))
					    (cond ((and l r) (format nil "\\beamLR #~A #~A " (event-beamlt e) (event-beamrt e)))
						  (l (format nil "\\beamL #~A " (event-beamlt e)))
						  (r (format nil "\\beamR #~A " (event-beamrt e)))
						  (t "")))
					  (let ((m (or (getmark e :tremolo) (getmark e :starttremolo)))) ;; (format nil "\\markup{~A}" (conc-stringlist (loop repeat (second m) collect "/")))
					    (if (and m (not trf))
						(format nil "\\repeat \"tremolo\" ~A ~A" (second m)
							(if (eq (first m) :tremolo) "" "{"))
						"")) ; stuff before 
					  (if (notep e)
					      (if (chordp e)
						  (format nil "<~A>" (conc-stringlist
								      (loop
								       for (n nn) on (event-notes* e)
								       and w in (event-writtennotes e)
								       and a in (event-accs e)
								       and a2 in (event-addaccs e)
								       for ha = (getmark e (list :harmonic :touched n))
								       and hs = (getmark e (list :harmonic :sounding n))
								       collect (lynote w a a2 (getmark e (list :cautacc n)) (getmark e (list :showacc n)))
								       when ha collect "\\harmonic"
								       when hs collect "^\\flageolet"
								       when nn collect " ")))
						  (lynote (event-writtennote e) (event-acc e) (event-addacc e) ; 8/9/06
							  (getmark e (list :cautacc (event-note* e)))
							  (getmark e (list :showacc (event-note* e))))
						  #|(conc-strings ; 8/9/06
						   (lynote (event-writtennote e) (event-acc e) (event-addacc e)
							   (getmark e (list :cautacc (event-note* e)))
							   (getmark e (list :showacc (event-note* e))))
						   (let ((ha (getmark e :harmonic)))
						     (when ha (ecase (second ha) (:touched "\\harmonic") (:sounding "^\\flageolet")))))|#)
					      (if fm (if (event-inv e) "\\skip " "R") (if (event-inv e) "s" "r")))
					  (if fm (format nil "1*~A/~A" (timesig-num ts) (timesig-den ts))
					      (multiple-value-bind (wd ds) (let ((m (or (getmark e :tremolo)
											(getmark e :starttremolo)
											(getmark e :endtremolo))))
									     (if (and m (not trf))
										 (values (third m) 0)
										 (event-writtendur* e ts)))
						(let ((du (case wd
							    (2 "\\breve")
							    (4 "\\longa")
							    (otherwise (/ wd)))))
						  (ecase ds
						    (0 (format nil "~A" du))
						    (1 (format nil "~A." du))
						    (2 (format nil "~A.." du))))))
					  (if (chordp e) "" ; 8/9/06
					      (let ((ha (getmark e :harmonic)))
						(or (when ha (ecase (second ha) (:touched "\\harmonic") (:sounding "^\\flageolet"))) "")))
					  (if (getmark e :arpeggio) "\\arpeggio" "")
					  (conc-stringlist
					   (loop
					    for xxx in (delete-if (lambda (x) (/= (second x) 2)) (getmarks e :startslur-))
					    collect "\\("))
					  (conc-stringlist
					   (loop
					    for xxx in (delete-if (lambda (x) (/= (second x) 1)) (getmarks e :startslur-))
					    collect "("))
					  (conc-stringlist
					   (loop
					    for xxx in (delete-if (lambda (x) (/= (second x) 1)) (getmarks e :endslur-))
					    collect ")"))
					  (conc-stringlist
					   (loop
					    for xxx in (delete-if (lambda (x) (/= (second x) 2)) (getmarks e :endslur-))
					    collect "\\)"))
					  (if (and (notep e) (= (event-beamlt e) 0) (> (event-beamrt e) 0)) "[" "")
					  (if (and (notep e) (> (event-beamlt e) 0) (= (event-beamrt e) 0)) "]" "")
					  (if (and (notep e) (or-list (force-list (event-tiert e)))) "\~" "")
					  (conc-stringlist
					   (loop for i in
						 (sort (delete-duplicates
							(loop for (a1 . a2) in +lilypond-marks+
							      nconc (mapcar (lambda (x) (cons a2 (force-list x))) (getmarks e a1)))
							:key #'cdr :test #'equal)
						       (lambda (x y) (cond
								       ((find (cadr x) +marks-withacc+) nil)
								       ((find (cadr y) +marks-withacc+) t)
								       (t (let ((x2 (caddr x)) (y2 (caddr y)))
									    (cond ((and (numberp x2) (numberp y2)) (< x2 y2))
										  (x2 t)))))))
						 collect (car i)))
					  (conc-stringlist
					   (loop for i in
						 (delete-duplicates
						  (loop for (a1 . a2) in +lilypond-trmarks+
							nconc (mapcar (lambda (x) (let ((f (force-list x)))
										    (cons a2 (if (member (first f) +marks-long+) (fifth f) (third f)))))
								      (getmarks e a1)))
						  :key #'cdr :test #'equal)
						 when (eq cdi :u) collect "^" and collect (car i)
						 when (cdr i) collect (ecase cdi (:u "^") (:d "_")) and
						 collect (format nil (or textacc-markup *lilypond-textacc-markup*)
								 (ecase (cdr i)
								   (-2 "\\doubleflat")
								   (-3/2 "\\sesquiflat")
								   (-1 "\\flat")
								   (-1/2 "\\semiflat")
								   (0 "\\natural")
								   (1/2 "\\semisharp")
								   (1 "\\sharp")
								   (3/2 "\\sesquisharp")
								   (2 "\\doublesharp")))
						 when (eq cdi :d) collect "_" and collect (car i)))
					  (cond ((or (getmark e :endwedge<) (getmark e :endwedge>)) "\\!")
						(t ""))
					  (conc-stringlist
					   (loop for i in
						 (loop for a in +lilypond-dyns+ nconc (mapcar #'force-list (getmarks e (car a))))
						 for l = (lookup (first i) +lilypond-dyns+)
						 if (listp l) collect (conc-strings (if (<= nvce 1) "_" (ecase vce ((0 2) "^") ((1 3) "_"))) (car l)) else collect l))
					  (cond ((getmark e :startwedge<) "\\< ")
						((getmark e :startwedge>) "\\> ") 
						(t "")) 
					  (conc-stringlist
					   (loop for x in '(:text :textdyn :texttempo :textnote)
						 and m in (list (or text-markup *lilypond-text-markup*)
								(or textdyn-markup *lilypond-textdyn-markup*)
								(or texttempo-markup *lilypond-texttempo-markup*)
								(or textnote-markup *lilypond-textnote-markup*))
						 nconc (loop for (xxx di str) in (getmarks e x)
							     collect (conc-strings
								      (ecase di (:up "^") (:down "_") (:nopos "-") (:detached ""))
								      (format nil m (lilypond-string-escape str))))))
					  (let ((m (getmark e '(:starttext- 1))))
					    (if m "\\startTextSpan" ""))
					  (let ((m (getmark e '(:endtext- 1))))
					    (if m "\\stopTextSpan" ""))	; the actual note w/ attachments
					  (let ((m (getmark e '(:endlongtrill- 1))))
					    (if m "\\stopTrillSpan" ""))
					  (let ((m (or (getmark e :tremolo) (getmark e :starttremolo)))) ;; 
					    (if (and m trf)
						(format nil "^\\markup{\"~A~A\"}"
							(conc-stringlist (loop repeat (- (roundint (log (third m) 1/2)) 2) collect "/"))
							(if (getmark e :starttremolo) "-" "")) 
						"")) 
					  (if (and (getmark e :endtremolo) (not trf))
					      "}" "")
					  (if (and (event-grace e) (getmark e :endgrace) (not (getmark e :startgrace))) "}" "")
					  (let ((uu (getmarks e :endtup)))
					    (conc-stringlist
					     (loop repeat (length uu) collect "}")))
					  (cond ((or (getmark e :end8up-) (getmark e :8up)) " \\octReset")
						((or (getmark e :end8down-) (getmark e :8down)) " \\octReset"))))))
			   (let ((b (getprop m :barline)))
			     (when b (format f "\\bar \"~A\" " (lookup (second b) +lilypond-barlines+))))
			   (format f "| %~A~%     ~A" mn (if nxm " " "")))
			  (if (< vce (1- nvce)) (format f "} \\\\~%     ") (format f "}~%  >>~%")))
		    (format f "}~%~%")
		    (if (> ns 1) 
			(format f "~A = {~%  ~A~%}~%~%"
				(conc-strings lily-partname "S") 
				(conc-stringlist
				 (loop with nu = 0
				       for n = nil then (timesig-num (meas-timesig m))
				       and d = nil then (timesig-den (meas-timesig m))
				       for m in (part-meas p)
				       when (and (getprop m :startsig) (> nu 0))
				       collect (format nil "\\skip 1*~A/~A*~A " n d nu) into re and do (setf nu 0)
				       do (incf nu)
				       finally (return (nconc re (list (format nil "\\skip 1*~A/~A*~A" n d nu))))))))))) ;)
	    (when (or *title* *subtitle* *composer*)
	      (format f "\\header {~%")
	      (when *title* (format f "  title = ~S~%" *title*))
	      (when *subtitle* (format f "  subtitle = ~S~%" *subtitle*))
	      (when *composer* (format f "  composer = ~S~%" *composer*))
	      (format f "}~%~%"))
	    (format f "\\score {~%") ;; score block
	    (loop for e in (force-list (or scorehead *lilypond-scorehead*)) do (format f "  ~A~%" e))
	    (loop
	     with in = 2
	     for p in parts and nm in (nreverse nms) do
	     (loop
	      for (xxx nu ty) in (sort (getprops p :startgroup) #'< :key #'second) do
	      (if ty
		  (ecase ty 
		    ((:group :choirgroup) (format f "~A\\new ~A <<~%" (make-string in :initial-element #\space)
						  (ecase ty
						    (:group (if (<= nu 1) "StaffGroup" "InnerStaffGroup"))
						    (:choirgroup (if (<= nu 1) "ChoirStaff" "InnerChoirStaff")))))
		    (:grandstaff (format f "~A\\new PianoStaff <<~%" (make-string in :initial-element #\space))))
		  (format f "~A<<~%" (make-string in :initial-element #\space)))
	      (incf in 2))
	     (let ((ns (instr-staves (part-instr p))))
	       (if (<= ns 1)
		   (format f "~A\\new Staff \\~A~%" (make-string in :initial-element #\space) nm)
		   (progn
		     (loop for s from 1 to ns do (format f "~A\\context Staff = ~A \\~A~%"
							 (make-string in :initial-element #\space)
							 (code-char (+ 64 s))
							 (conc-strings nm "S")))
		     (format f "~A\\context Staff = A \\new Voice \\~A~%" (make-string in :initial-element #\space) nm))))
	     (loop
	      for xxx in (getprops p :endgroup)
	      do (decf in 2) (format f "~A>>~%" (make-string in :initial-element #\space))))
	    (format f "}~%"))
	  (when twrn (format t ";; WARNING: Some string spanners are excluded~%"))))))
  (when process (view-lilypond filename options view)))

