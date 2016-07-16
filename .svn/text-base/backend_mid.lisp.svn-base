;; -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**************************************************************************************************
;; FOMUS
;; backend_mid.lisp
;;**************************************************************************************************

(in-package :fomus)
(compile-settings)

(declaim (inline midi-off midi-dur midi-note midi-ch midi-vel midi-endoff))
(defun midi-off (x) (funcall *cm-midioff* x))
(defun midi-dur (x) (funcall *cm-mididur* x))
(defun midi-note (x) (funcall *cm-midinote* x))
(defun midi-ch (x) (slot-value x *cm-midichslot*))
(defun midi-vel (x) (funcall *cm-midivel* x))
(defun midi-endoff (x) (+ (midi-off x) (midi-dur x)))

(defmacro midi-ch* (x) `(slot-value ,x *cm-midichslot*))
(defmacro midi-off* (x) `(slot-value ,x *cm-midioffslot*))
(defmacro midi-dur* (x) `(slot-value ,x *cm-mididurslot*))
(defmacro midi-vel* (x) `(slot-value ,x *cm-midivelslot*))
(defmacro midi-note* (x) `(slot-value ,x *cm-midinoteslot*))

(defun midi-sort (x y)
  (if (= (midi-off x) (midi-off y))
      (if (= (midi-ch x) (midi-ch y))
	  (cond ((and (typep x *cm-midi*) (typep y *cm-midi*))
		 (if (= (midi-dur x) (midi-dur y))
		     (cond ((and (midi-note x) (midi-note y))
			    (if (= (midi-note x) (midi-note y))
				(< (midi-vel x) (midi-vel y))
				(< (midi-note x) (midi-note y))))
			   ((midi-note x) t))
		     (< (midi-dur x) (midi-dur y))))
		((typep y *cm-midi*) t))
	  (< (midi-ch x) (midi-ch y)))
      (< (midi-off x) (midi-off y))))

;; in order of processing
(defparameter +midi-marks+
  '((:ffffff :per :d) (:fffff :per :d) (:ffff :per :d) (:fff :per :d) (:ff :per :d)
    (:f :per :d) (:mf :per :d) (:mp :per :d) (:p :per :d) (:pp :per :d) (:ppp :per :d) (:pppp :per :d) (:ppppp :per :d) (:pppppp :per :d)
    (:ffffff* :repl :ffffff :per :d) (:fffff* :repl :fffff :per :d) (:ffff* :repl :ffff :per :d) (:fff* :repl :fff :per :d) (:ff* :repl :ff :per :d)
    (:f* :repl :f :per :d) (:mf* :repl :mf :per :d) (:mp* :repl :mp :per :d) (:p* :repl :p :per :d) (:pp* :repl :pp :per :d) (:ppp* :repl :ppp :per :d)
    (:pppp* :repl :pppp :per :d) (:ppppp* :repl :ppppp :per :d) (:pppppp* :repl :pppppp :per :d)
    
    (:tremolo :a1 1 :a2 2)			; arg1 = number of divs
    (:starttremolo :bot t :repl :span-tremfirst :a1 1 :a2 2)
    (:starttremolo :top t :repl :endtremfirst :a1 1 :a2 2)
    (:endtremolo :bot t :repl :span-tremsecond :a1 1 :a2 2)
    (:endtremolo :top t :repl :endtremsecond :a1 1 :a2 2)
    (:mordent :a1 :m) (:prall :a1 :n) (:trill :a1 :n) (:longtrill :a1 :n)
    (:arpeggio :bot t :repl :span-arp :a1 1 :a2 :e)
    (:arpeggio :top t :repl :endarp :a1 1 :a2 :e)
    ((:portamento :after) :top t :repl :span-port :addnx :endport)
    ((:glissando :after) :top t :repl :span-gliss :addnx :endgliss) 
    
    (:startgraceslur- :bot t :repl :span-slur :al 1)
    (:endgraceslur- :top t :repl :endslur- :al 1)
    (:startslur- :bot t :repl :span-slur :al 1)
    (:endslur- :top t :a1 1)

    ((:harmonic :sounding) :a1 1 :a2 2) ((:harmonic :touched) :a1 1 :a2 2) ; touched/sounding notenum
    :arco :pizz    
    :stopped :open :staccato :staccatissimo
    :portato :tenuto :marcato :accent
    :rfz :sfz :spp :sp :sff :sf :fp
    (:rfz* :repl :rfz) (:sfz* :repl :sfz) (:spp* :repl :spp) (:sp* :repl :sp) (:sff* :repl :sff) (:sf* :repl :sf) (:fp* :repl :fp)
    
    (:fermata :a1 1) ((:breath :after))
;;  (:lineprall :a1 1) (:prallup :a1 1) (:pralldown :a1 1) (:downmordent :a1 1)
;;  (:upmordent :a1 1) (:downprall :a1 1) (:upprall :a1 1) (:prallmordent :a1 1)
;;  (:prallprall :a1 1) (:reverseturn :a1 1)
;;  (:turn :a1 1)

    (:startwedge> :bot t :repl :span-wedge> :al :w)
    (:endwedge> :top t :a1 :w)
    (:startwedge< :bot t :repl :span-wedge< :al :w)
    (:endwedge< :top t :a1 :w)
    (:startwedge>* :bot t :repl :span-wedge> :al :w)
    (:endwedge>* :top t :a1 :w)
    (:startwedge<* :bot t :repl :span-wedge< :al :w)
    (:endwedge<* :top t :a1 :w)))

(defparameter +midi-allmarks+
  (delete-duplicates (mapcar (lambda (x) (destructuring-bind (ma &key repl &allow-other-keys) (force-list x) (or repl (first (force-list ma))))) +midi-marks+)))
(defparameter +midi-spannermarks+
  '((:span-wedge> . :endwedge>) (:span-wedge< . :endwedge<) (:span-slur . :endslur-)
    (:span-tremfirst . :endtremfirst) (:span-tremsecond . :endtremsecond) (:span-arp . :endarp) (:span-port . :endport) (:span-gliss . :endgliss)))
(defparameter +midi-firstmarks+
  '(:portato :tenuto :marcato :accent))
(defparameter +midi-endmarks+ ; marks that should get stuck at end of sequence of notes if necessary
  '(:staccato :staccatissimo :fermata :breath :portamento :glissando))
(declaim (special *midi-persistmarks* *midi-nm* *midi-lo*)) ; init: ((:d . :mf))   nil

(defun midi-marks (ev ebot etop pmn)	; pmn = perc. pitch
  (loop for e in +midi-marks+
	for (m . n) = (destructuring-bind (ma &key al a1 a2 top bot repl addnx per) (force-list e)
			(when (and (or (null top) etop) (or (null bot) ebot))
			  (let ((g (getmark ev ma)))
			    (flet ((arg (a)
				     (when a (case a
					       (:e *midi-lo*) ; last offset
					       (:n (+ (if (chordp ev) (last-element (event-notes* ev)) (event-note* ev)) (or (second (force-list g)) (if pmn 0 1))))
					       (:m (+ (if (chordp ev) (last-element (event-notes* ev)) (event-note* ev)) (or (second (force-list g)) (if pmn 0 -1))))
					       (:w :w)
					       (otherwise (nth a (force-list g)))))))
			      (when g 
				(let ((x (list (or repl (first (force-list ma))) al (arg a1) (arg a2))))
				  (if per (progn (setf (cdr (assoc per *midi-persistmarks*)) x) nil)
				      (if addnx (cons nil x) (cons x nil)))))))))
	when m collect m into re
	when n collect n into ne
	finally (return (prog1
			    (nconc (mapcar #'cdr *midi-persistmarks*)
				   (if etop (prog1 (nconc *midi-nm* re) (setf *midi-nm* nil)) re))
			  (prenconc ne *midi-nm*)
			  (when etop (setf *midi-lo* (event-off ev)))))))

(defparameter *grace-dur-secs* 1/12)
(declaim (special *gracedur*))
(defparameter *min-amp* 1/10)
(defparameter *trdur-secs* 1/16) ; trill notes per sec. (and unmeasured tremolos)
(declaim (special *trdur*))
(defparameter *tramp* 3/4)
(defparameter *fermata-mults* '(3/2 2 3))
(defparameter *breath-dur* 1/6)
(defparameter *tempo* 60)
(defparameter *staccato-mult* 1/3)
(defparameter *staccatissimo-mult* 1/5)
(defparameter *portato-mult* 2/3)
(defparameter *tenuto-adddur* 1/8)
(defparameter *slur-adddur* 1/8)
(defparameter *trovlp-adddur* 1/8)
(defparameter *mindur-secs* 1/12)
(defparameter *arpatt* 1/3)
(defparameter *harmatt* 1/3)

(defparameter +midi-touchedharms+ (vector nil nil 34 31 28 24 nil 19 12))

;; return values: replacement note(s), offset increment for remaining notes
;; how to handle dynamics, arco, pizz??? (make them "persistant" marks?)
(defun midi-default-events-fun (ev mark arg1 arg2)
  (labels ((amp (n) (+ *min-amp* (* (/ (1+ n) 11) (- 1 *min-amp*))))
	   (trem (s)
	     (loop for v in ev nconc
		   (loop with db = (/ (midi-dur v) (max (if (<= arg2 1/32) (/ (midi-dur v) *trdur*) (min (/ (midi-dur v) *trdur*) arg1)) 1))
			 with du = (min (* db 3/2) (+ db *trovlp-adddur*))
			 for o from (midi-off v) below (midi-endoff v) by db and sk = s then (not sk)
			 when sk collect
			 (make-instance *cm-midi* :channel (midi-ch v) :time o :duration du :keynum (midi-note v) :amplitude (* (midi-vel v) *tramp*))))))
    (ecase mark
      (:ffffff (setf (midi-vel* ev) (amp (+ 8 2/3))) ev)
      (:fffff (setf (midi-vel* ev) (amp (+ 8 1/3))) ev)
      (:ffff (setf (midi-vel* ev) (amp 8)) ev)
      (:fff (setf (midi-vel* ev) (amp (+ 7 1/2))) ev)
      (:ff (setf (midi-vel* ev) (amp 7)) ev)
      (:f (setf (midi-vel* ev) (amp 6)) ev)
      (:mf (setf (midi-vel* ev) (amp 5)) ev)
      (:mp (setf (midi-vel* ev) (amp 4)) ev)
      (:p (setf (midi-vel* ev) (amp 3)) ev)
      (:pp (setf (midi-vel* ev) (amp 2)) ev)
      (:ppp (setf (midi-vel* ev) (amp (+ 1 1/2))) ev)
      (:pppp (setf (midi-vel* ev) (amp 1)) ev)
      (:ppppp (setf (midi-vel* ev) (amp 2/3)) ev)
      (:pppppp (setf (midi-vel* ev) (amp 1/3)) ev)
      (:rfz (setf (midi-vel* ev) (/ (+ (midi-vel ev) 1) 2)) ev)
      (:sfz (setf (midi-vel* ev) (/ (+ (midi-vel ev) 1) 2)) ev)
      (:spp (setf (midi-vel* ev) (/ (+ (midi-vel ev) *min-amp* *min-amp*) 3)) ev)
      (:sp (setf (midi-vel* ev) (/ (+ (midi-vel ev) *min-amp*) 2)) ev)
      (:sff (setf (midi-vel* ev) (/ (+ (midi-vel ev) 2) 3)) ev)
      (:sf (setf (midi-vel* ev) (/ (+ (midi-vel ev) 1) 2)) ev)
      (:fp ev)				; ?
      (:span-wedge<
       (let ((le (midi-vel (last-element ev))))
	 (when (> arg1 le)
	   (loop with o0 = (midi-off (first ev)) and m2 = (log (/ arg1 le))
		 for e in ev do (setf (midi-vel* e) (* (midi-vel e) (exp (/ (* (- (event-off e) o0) m2) (- arg2 o0))))))))
       ev)
      (:span-wedge>
       (let ((le (midi-vel (last-element ev))))
	 (when (< arg1 le)
	   (loop with o0 = (midi-off (first ev)) and m2 = (log (/ le arg1))
		 for e in ev do (setf (midi-vel* e) (/ (midi-vel e) (exp (/ (* (- (event-off e) o0) m2) (- arg2 o0))))))))
       ev)
      (:longtrill (loop with db = (/ (midi-dur ev) (max (* (floor (/ (midi-dur ev) *trdur*) 2) 2) 2)) 
			with du = (min (* db 3/2) (+ db *trovlp-adddur*))
			for o from (midi-off ev) below (midi-endoff ev) by db
			and pt = t then nil
			collect (make-instance *cm-midi* :channel (midi-ch ev) :time o :duration du :keynum (if pt (midi-note ev) arg1) :amplitude (* (midi-vel ev) *tramp*))))
      (:pizz ev)
      (:arco ev)	; arg1 = program num. of instr.
      (:fermata (case arg1
		  (:short (let ((i (* (midi-dur ev) (1- (first *fermata-mults*))))) (setf (midi-dur* ev) (+ (midi-dur ev) i)) (values ev i)))
		  (:long (let ((i (* (midi-dur ev) (1- (second *fermata-mults*))))) (setf (midi-dur* ev) (+ (midi-dur ev) i)) (values ev i)))
		  (:verylong (let ((i (* (midi-dur ev) (1- (third *fermata-mults*))))) (setf (midi-dur* ev) (+ (midi-dur ev) i)) (values ev i)))))
      (:span-arp (let* ((e (first ev))
			(le (1- (length ev)))
			(o1 (if arg2 (max (+ arg2 *gracedur*) (- (midi-off e) (* *gracedur* le))) (- (midi-off e) (* *gracedur* le))))
			(du (/ (- (midi-off e) o1) (max le 1))))
		   (case arg1
		     ((:up nil)
		      (loop for o from o1 by du for v in ev and i from 0 for va = (+ (- 1 *arpatt*) (* *arpatt* (/ i le))) do
			    (setf (midi-dur* v) (- (midi-endoff v) o) (midi-off* v) o (midi-vel* v) (* (midi-vel v) va))))
		     (:down (loop for o from o1 by du for v in (reverse ev) and i from 0 for va = (+ (- 1 *arpatt*) (* *arpatt* (/ i le))) do
				  (setf (midi-dur* v) (- (midi-endoff v) o) (midi-off* v) o (midi-vel* v) (* (midi-vel v) va))))))
		 ev)
      (:tremolo (loop with db = (/ (midi-dur ev) (max (floor (if (<= arg2 1/32) (/ (midi-dur ev) *trdur*) (min (/ (midi-dur ev) *trdur*) arg1))) 2))
		      with du = (min (* db 3/2) (+ db *trovlp-adddur*))
		      for o from (midi-off ev) below (midi-endoff ev) by db
		      collect (make-instance *cm-midi* :channel (midi-ch ev) :time o :duration du :keynum (midi-note ev) :amplitude (* (midi-vel ev) *tramp*))))
      (:span-tremfirst (trem t))
      (:span-tremsecond (trem nil))
      (:span-port ev)			; ?
      (:span-gliss ev)			; ?
      (:breath (values ev *breath-dur*))
      (:harmonic (case arg1
		   (:sounding (setf (midi-note* ev) arg2 (midi-vel* ev) (* (midi-vel ev) (- 1 *harmatt*))))
		   (:touched (setf (midi-note* ev)
				   (+ (midi-note ev)
				      (or (let ((x (- arg2 (midi-note ev))))
					    (if (and (>= x 0) (< x (length +midi-touchedharms+)))
						(svref +midi-touchedharms+ x)))
					  0))
				   (midi-vel* ev) (* (midi-vel ev) (- 1 *harmatt*)))))
		 ev)
      (:stopped ev)
      (:open ev)
      (:staccato (setf (midi-dur* ev) (* (midi-dur ev) *staccato-mult*)) ev)
      (:staccatissimo (setf (midi-dur* ev) (* (midi-dur ev) *staccatissimo-mult*)) ev)
      ((:prall :trill :mordent)
       (let ((md (/ (midi-dur ev) 2)))
	 (cons
	  (make-instance *cm-midi* :channel (midi-ch ev) :time (+ (midi-off ev) md) :duration md :keynum (midi-note ev) :amplitude (midi-vel ev))
	  (loop with db = (/ md (max (* (floor (/ md *trdur*) 2) 2) 2))
		with du = (min (* db 3/2) (+ db *trovlp-adddur*))
		for o from (midi-off ev) below (+ (midi-off ev) md) by db
		and pt = t then nil
		collect (make-instance *cm-midi* :channel (midi-ch ev) :time o :duration du :keynum (if pt (midi-note ev) arg1)
				       :amplitude (* (midi-vel ev) *tramp*))))))
      (:portato (setf (midi-dur* ev) (* (midi-dur ev) *portato-mult*)) ev)
      (:tenuto (setf (midi-dur* ev) (min (+ (midi-dur ev) *tenuto-adddur*) (* (midi-dur ev) 3/2))) ev)
      (:marcato (setf (midi-vel* ev) (/ (+ (midi-vel ev) 2) 3)) ev)
      (:accent (setf (midi-vel* ev) (/ (+ (midi-vel ev) 1) 2)) ev)
      (:span-slur (loop for (e n) on ev
			when (and n (<= (midi-off n) (midi-endoff e)))
			do (setf (midi-dur* e) (min (+ (midi-dur e) *slur-adddur*) (* (midi-dur n) 3/2))))
		  ev))))

(defun save-midi-aux (parts filename options play) ; if play is open stream, then uses rts realtime (ignores filename)
  (unless *cm-exists*
    (format t ";; ERROR: Common Music required for MIDI output~%")
    (return-from save-midi-aux))
  (when (>= *verbose* 1)
    (typecase play
      (boolean (out ";; Saving MIDI file ~S...~%" filename))
      ((or function symbol) (out ";; Compiling MIDI data...~%"))
      (t (out ";; Scheduling MIDI playback...~%" filename))))
  (destructuring-bind (&key (nports 1) instr-per-ch events-fun (pbend-width 2) cm-args
			    (grace-dur-secs *grace-dur-secs*) (min-amp *min-amp*) (trdur-secs *trdur-secs*) (tramp *tramp*)
			    (fermata-mults *fermata-mults*) (breath-dur *breath-dur*) (tempo *tempo*)
			    (staccato-mult *staccato-mult*) (staccatissimo-mult *staccatissimo-mult*) (tenuto-adddur *tenuto-adddur*)
			    (trovlp-adddur *trovlp-adddur*) (mindur-secs *mindur-secs*) (slur-adddur *slur-adddur*)
			    (portato-mult *portato-mult*) (arpatt *arpatt*) (harmatt *harmatt*) (delay 0) &allow-other-keys) options
    (when (typep play 'boolean) (setf nports 1))
    (let* ((*gracedur* (/ (* grace-dur-secs tempo) 60))
	   (*min-amp* min-amp)
	   (*trdur* (/ (* trdur-secs tempo) 60))
	   (*tramp* tramp)
	   (*fermata-mults* fermata-mults)
	   (*breath-dur* breath-dur)
	   (*tempo* tempo)
	   (*staccato-mult* staccato-mult)
	   (*staccatissimo-mult* staccatissimo-mult)
	   (*tenuto-adddur* tenuto-adddur)
	   (*trovlp-adddur* trovlp-adddur)
	   (*mindur-secs* mindur-secs)
	   (*slur-adddur* slur-adddur)
	   (*portato-mult* portato-mult)
	   (*arpatt* arpatt)
	   (*harmatt* harmatt)
	   (adj nil)
	   (xta nil)
	   (evs (flet ((chs (ba) (if (and *quartertones* (/= (mod ba 16) 9)) (list ba (if (= ba 8) 10 (1+ ba))) (list ba)))) 
		  (loop with aps and is and el and ps = (loop repeat nports collect (let ((x (make-array 16 :initial-element nil))) ; list of 16-ch arrays
										      (setf (svref x 9) t) (when *quartertones* (setf (svref x 8) t)) x))
		     for p in (loop for p in parts
				 for ex = (or (instr-midiprgch-ex (part-instr p)) 0)
				 when (consp ex) nconc (destructuring-bind (pp &key pizz stopped open harmonic) ex
							 (declare (ignore pp))
							 (loop for e in (list pizz stopped open harmonic)
							    and s in '(:pizz :stopped :open :harmonic)
							    while (some
								   (lambda (m) (some
										(lambda (v) (some (lambda (e) (getmark e s)) v))
										(meas-voices m)))
								   (part-meas p))
							    collect (let ((r (make-partex nil :instr (copy-instr (part-instr p) :midiprgch-ex e))))
								      (push (cons e r) aps)
								      r)))
				 collect p
				 finally (setf aps (nreverse aps)))
		     for in = (part-instr p)
		     for ex = (first (force-list (or (instr-midiprgch-ex in) 0)))
		     and (midi-ch . midi-vel) = (destructuring-bind (&key midi-ch (midi-vel 1) &allow-other-keys) (part-opts p) (cons midi-ch midi-vel))
		     for (po . ch) =
		     (let ((c (or midi-ch ; c = (port . chan)
				  (if (is-percussion p)
				      (cons (or po 0) 9)
				      (loop for o from 0 and e in ps
					 for s = (position ex e)
					 when s do (return (cons o s))))
				  (loop for o from 0 and e in ps
				     for s = (position nil e)
				     when s do (return (cons o s)))
				  (progn
				    (format t ";; ERROR: Too many parts/instruments for ~S port(s)/~S channels (use NPORTS option, MIDI-CH option in parts or MIDIPRGCH-EX slot in instruments to fix)~%"
					    nports (* nports 16))
				    (return-from save-midi-aux)))))
		       (unless (is-percussion p)
			 (loop for i in (chs (cdr c))
			    do (setf (svref (nth (car c) ps) i)
				     (or ex
					 (progn
					   (push (format nil ";; WARNING: Missing MIDI program change number in MIDIPRGCH-EX slot of instrument ~S, part ~S~%"
							 (instr-sym in) (part-name p))
						 el)
					   0))))
			 (when instr-per-ch
			   (push ex is)
			   (let ((x (count ex is)))
			     (when (>= x instr-per-ch)
			       (setf is (delete x is))
			       (mapc (lambda (e) (nsubstitute t ex e)) ps)))))
		       (cons (car c) (+ (* (car c) 16) (cdr c))))
		     and pmn = (when (is-percussion p) (mapcar (lambda (x) (cons (perc-sym x) (perc-midinote-ex x))) (instr-percs in)))
		     do
		     (prenconc (unless (is-percussion p) (loop for i in (chs ch) collect (make-instance *cm-progch* :time 0 :channel i :program ex))) xta)
		     (let ((ap (rassoc p aps))) (when ap (setf aps (delete-if (lambda (x) (and (= (car x) ex) (numberp (cdr x)))) aps) (cdr ap) ch)))
		     when (or (not (is-percussion p)) pmn) nconc
		     (let ((es (delete nil
				       (delete-duplicates
					(labels ((gr (y) (loop for e in y if (numberp (midi-ch e)) collect e else nconc (gr (midi-ch e)))))
					  (gr
					   (loop for (m . es) in
						(sort
						 (loop for r on
						      (sort
						       (loop for v from 0 below (mloop for m in (part-meas p) maximize (length (meas-voices m))) nconc
							    (let ((*midi-persistmarks* (list (cons :d (list :mf nil nil nil))))
								  (*midi-nm* nil)
								  (*midi-lo* nil))
							      (loop with ts ; ties
								 and evs = (loop for m in (part-meas p) append (nth v (meas-voices m)))
								 for ev in evs
								 for pizz = (cond ((getmark ev :pizz) t)
										  ((getmark ev :arco) nil)
										  (t pizz))
								 and (pizzch stoppedch opench harmonicch) =
								 (unless pmn (destructuring-bind (x &key pizz stopped open harmonic)
										 (force-list (or (instr-midiprgch-ex in) 0))
									       (declare (ignore x)) (list pizz stopped open harmonic)))
								 and (of . du)
								 in (flet ((gem (di pr) ; grace notes, di = direction, pr = offsets so far
									     (nreverse
									      (loop with cd and co and oo and gr ; current dur & off
										 for (e . r) on (if di evs (reverse evs)) and p in pr
										 unless (eql oo (event-off e)) do (setf oo (event-off e) cd nil)
										 if (and (event-grace e)
											 (if di (>= (event-grace e) 0) (< (event-grace e) 0)))
										 collect (if cd (cons (if di (incf co cd) (decf co cd)) cd)
											     (progn
											       (setf cd (loop for x in r
													   for su from 1
													   while (and (event-grace x)
														      (= (event-off x)
															 (event-off e)))
													   finally (return
														     (if x
															 (let ((xx (if di
																       (- (event-endoff x)
																	  (event-off e))
																       (- (event-off e)
																	  (event-off x)))))
															   (if (> xx 0)
															       (min (/ xx (+ su 2))
																    *gracedur*)
															       *gracedur*))
															 *gracedur*))))
											       (cons (setf co (if di (event-off e) (- (event-off e) cd)))
												     cd)))
										 and do (setf gr t)
										 else collect (or p (when (and gr (not (event-grace e)))
												      (if di
													  (cons (max (event-off e) (+ co cd)) nil)
													  (cons nil (- (min (event-endoff e) co)
														       (event-off e))))))
										 and do (setf gr nil)))))
								      (mapcar (lambda (x y) (cons (or (car x) (event-off y))
												  (or (cdr x) (event-dur* y))))
									      (gem nil (gem t (make-list (length evs) :initial-element nil))) evs))
								 for mi = (progn
									    (setf ts (delete-if (lambda (x) (< (midi-endoff (cdr x)) of)) ts))
									    (if (notep ev)
										(loop with n0 = (let ((z (force-list (if (chordp ev) (event-notes* ev) (event-note* ev)))))
												  (if pmn (mapcar (lambda (x)
														    (let ((m (getmark ev (list :percsym x))))
														      (if m
															  (lookup (third m) pmn)
															  (lookup x pmn))))
														  z)
												      z))
										   with ln = (length n0)
										   and cch = (or (when pizz (lookup pizzch aps))
												 (loop for v in '(:stopped :open :harmonic)
												    and c in (list stoppedch opench harmonicch)
												    for m = (getmark ev v)
												    when m do (return (lookup c aps)))
												 ch)
										   for n in n0 and x from 1
										   and tr in (force-list (or (event-tiert ev) '(nil)))
										   and tl in (force-list (or (event-tielt ev) '(nil)))
										   for bot = t then nil and top = (= x ln)
										   for i = (find-if (lambda (y) (= (midi-note (cdr y)) n)) ts) ; i = (marks . tiedobj)
										   unless (getmark ev (list :harmonic :touched n))
										   if (and i tl)
										   do (setf (midi-dur* (cdr i)) (- (event-endoff ev) (midi-off (cdr i)))
											    (car i) (delete-duplicates (nconc (midi-marks ev bot top pmn)
															      (car i))
														       :test #'equal))
										   else collect
										   (let ((i (cons (midi-marks ev bot top pmn)
												  (make-instance *cm-midi*
														 :channel cch
														 :time of :duration du
														 :keynum (if n
															     (if (and *transpose*
																      (instr-tpose in))
																 (+ (instr-tpose in) n) n)
															     (push (format nil ";; WARNING: Missing MIDI program change number in MIDIPRGCH-EX slot of instrument ~S, part ~S~%"
																	   (instr-sym in) (part-name p))
																   el))
														 :amplitude midi-vel))))
										     (when tr (push i ts))
										     i) end end)
										(list (cons (midi-marks ev t t pmn)
											    (make-instance *cm-midi* :channel ch :time of :duration du
																	  :keynum nil :amplitude 0)))))
								 when mi nconc mi))) ; list is (marks . objs)
						       (lambda (x y) (midi-sort (cdr x) (cdr y))))
						      for (ms . e) = (first r)
						      nconc (delete-if
							     (lambda (x) (find (first x) +midi-spannermarks+ :key #'cdr))
							     (mapcar (lambda (m) ; m = 1 mark for midiobj e
								       (let ((nm (cons (first m) (cddr m))))
									 (cons nm
									       (let ((nd (lookup (first m) +midi-spannermarks+)))
										 (if nd ; nd = :endmark
										     (loop for ((mx . ex) . nex) on r collect ex until
											  (find (cons nd (second m)) mx
												:key (lambda (x) (cons (first x) (second x)))
												:test #'equal)
											  finally (when (eq (second nm) :w)
												    (setf (second nm)
													  (if nex
													      (midi-vel (cdr nex))
													      (midi-vel ex))
													  (third nm)
													  (if nex
													      (midi-off (cdr nex))
													      (midi-endoff ex)))))
										     (list e))))))
								     ms)
							     :key #'car)) ; list is (mark . obj)
						 (lambda (x y) (< (position (caar x) +midi-allmarks+) (position (caar y) +midi-allmarks+))))
						for (ev0 . mu) = (let ((z (gr es)))
								   (if (find (first m) +midi-spannermarks+ :key #'car) (cons (sort z #'midi-sort) nil)
								       (cond ((find (first m) +midi-endmarks+) (cons (last-element z) nil))
									     ((find (first m) +midi-firstmarks+) (cons (first z) nil))
									     (t (cons (sort z #'midi-sort) t)))))
						when ev0 nconc
						(loop for evs in (if mu ev0 (list ev0))
						   when (eq (first m) :span-tremfirst) do (mapc (lambda (x) (setf (midi-dur* x) (* (midi-dur x) 2))) evs)
						   when (eq (first m) :span-tremsecond) do (mapc (lambda (x) (setf (midi-off* x) (- (midi-off x) (midi-dur x))
														   (midi-dur* x) (* (midi-dur x) 2))) evs)
						   nconc
						   (multiple-value-bind (x1 a1) (if events-fun (apply events-fun evs m) (values nil nil))
						     (multiple-value-bind (xx a) (if x1 (values x1 a1) (apply #'midi-default-events-fun evs m))
						       (when a (push (cons (mloop for e in xx maximize (midi-endoff e)) a) adj))
						       (let ((x (if (listp xx) (copy-list xx) (list xx))))
							 (unless (equal x '(nil))
							   (let ((xx (sort (nset-difference (force-list evs) x) #'midi-sort))) ; ones eliminated/to replace
							     (mapcar (lambda (y) (setf (midi-ch* y) nil)) xx)
							     (mapcar
							      (lambda (y)
								(let ((z (loop-return-argmax
									    (- (min (midi-endoff i) (midi-endoff y))
									       (max (midi-off i) (midi-off y)))
									    for i in xx)))
								  (when (and z (< (midi-off z) (midi-endoff y)) (< (midi-off y) (midi-endoff z))) (push y (midi-ch* z)))))
							      x))
							   x)))))))))
				       :key #'midi-note)))
		       (let ((cs (chs ch)))
			 (when (list>1p cs)
			   (let ((ll (remove-if (lambda (e) (integerp (midi-note e))) es)))
			     (mapc (lambda (x) (setf (midi-note* x) (floor (midi-note x)) (midi-ch* x) (second (chs (midi-ch x))))) ll)
			     (push (make-instance *cm-midipbend* :time 0 :channel (second cs) :bend (roundint (/ 4096 pbend-width))) xta))))
		       es)
		     finally (loop for e in (nreverse (delete-duplicates el :test #'string=)) do (format t e))))))
      (let ((o (floor (mloop with de = (/ (* delay tempo) 60) for e in evs do (incf (midi-off* e) de) minimize (midi-off e)))))
	(when (minusp o) (push (cons o (- o)) adj)))
      (loop for (o . a) in (merge-linear (sort adj #'> :key #'car) (lambda (x y) (when (= (car x) (car y)) (cons (car x) (max (cdr x) (cdr y))))))
	 do (mapc (lambda (x) (when (if (typep x *cm-midi*) (> (midi-endoff x) o) (>= (midi-off x) o))
				(if (>= (midi-off x) o) (incf (midi-off* x) a) (incf (midi-dur* x) a))))
		  evs))
      (setf evs (delete-if-not #'midi-note evs))
      (let ((md (/ (* mindur-secs tempo) 60)))
	(mapc (lambda (x)
		(when (typep x *cm-midi*)
		  (setf (midi-vel* x) (min (max (coerce (midi-vel x) 'single-float) 0.0) 1.0)
			(midi-dur* x) (max (midi-dur x) md))))
	      evs))
      (setf xta (loop for e in (split-into-groups xta #'type-of) nconc (delete-duplicates e :key #'midi-ch)))
      (if evs
	  (let ((so (sort (nconc xta evs) #'midi-sort)))
	    (typecase play
	      (boolean (apply *cm-events* so filename :tempo tempo :play play cm-args))
	      ((or function symbol) (when
					(nth-value 1 (funcall play so filename))
				      (when (funcall play so)
					(format t ";; WARNING: Can't call user function~%"))))
	      (otherwise (apply *cm-rts* (sort (nconc xta evs) #'midi-sort) play :tempo tempo cm-args))))
	  (progn
	    (when (typep play 'boolean) (ignore-errors (delete-file filename))) ; probably better than leaving possible "incorrect" output
	    (format t ";; WARNING: No MIDI events~%"))))))

(defun save-midi (parts filename options play)
  (flet ((ms (x y) (< (position x parts) (position y parts)))
	 (me (p) (destructuring-bind (&key midi-filename &allow-other-keys) (part-opts p)
		   (namestring (merge-pathnames midi-filename filename)))))
    (loop for ps in (sort (mapcar (lambda (x) (sort x #'ms))
				  (split-into-groups (remove-if-not (lambda (p)
								      (destructuring-bind (&key midi-filename &allow-other-keys) (part-opts p)
									midi-filename))
								    parts)
						     #'me :test 'equal))
			  #'ms :key #'first)
	  do (save-midi-aux ps (me (first ps)) options (when (typep play '(or function symbol)) play))))
  (save-midi-aux parts filename options play))