;; -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**************************************************************************************************
;; FOMUS
;; data.lisp
;;**************************************************************************************************

(in-package :fomus)
(compile-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GLOBAL FOR BACKENDS

#+(or linux (or darwin macos) unix) (defparameter +tmp-path+ "/tmp/")
#+(or mswindows win32) (defparameter +tmp-path+ "/")

(declaim (type boolean *acc-throughout-meas*))
(defparameter *acc-throughout-meas* t)

(declaim (type (or null string) *title* *subtitle* *composer*)
	 (type (or null symbol) *timesig-style* *tuplet-style*))
(defparameter *title* nil)
(defparameter *subtitle* nil)
(defparameter *composer* nil)
(defparameter *timesig-style* nil) ; fractional style timesigs if nil or :fraction, use "C" symbol if :common
(defparameter *tuplet-style* nil) ; denominator-only style if nil or :single, ratio style if :ratio

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; QUALITY

(declaim (type (real 0) *quality*))
(defparameter *quality* 1)

(defmacro set-quality (&body forms)
  `(let ((*quality* (if (>= *quality* 1) *quality* (/ (- 2 *quality*)))))
    ,@forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; QUANTIZING

;; nested tuplets indicated by a list
(declaim (type (or null (integer 2) list) *max-tuplet*))
(defparameter *max-tuplet* 13)

(declaim (type (or (integer 1) list) *beat-division*))
(defparameter *beat-division* 16) ; 64th notes--basic number of divisions-per-beat, if list, second value is compound meter

(declaim (type (real (0)) *min-tuplet-dur* *max-tuplet-dur*))
(defparameter *min-tuplet-dur* 1/3) ; fraction of beat smallest tuplets should span at minimum (1/2 = half a beat, etc.)--can be nil
(defparameter *max-tuplet-dur* 4)

(declaim (type (or null (rational (0)) (cons (rational (0)) (rational (0)))) *default-beat*))
(defparameter *default-beat* 1/4)

(declaim (type (rational (0)) *default-grace-dur*))
(defparameter *default-grace-dur* 1/4)	; dur, grace#

;; pitch quantizing
(declaim (type (rational (0)) *note-precision*))
(declaim (special *note-precision*))
(declaim (type boolean *quartertones*))
(defparameter *quartertones* nil)

(declaim (type boolean *transpose*))
(defparameter *transpose* t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONVERSION

(declaim (type (vector integer) +notenum+))
(defparameter +notenum+ (vector 9 11 0 2 4 5 7))
(defun note-to-num (note)
  (roundto
   (if (and *cm-keynumfun* *use-cm*)
       (if *cm-scale* (funcall *cm-keynumfun* note :in *cm-scale*) (funcall *cm-keynumfun* note))
       (if (symbolp note)
	   (let* ((s (symbol-name note))
		  (b (svref +notenum+ (- (char-int (aref s 0)) 65)))
		  (a (case (aref s 1)
		       ((#\+ #\S) (incf b) 2)
		       ((#\- #\F) (decf b) 2)
		       (otherwise 1))))
	     (+ (* (parse-integer (subseq s a)) 12) b 12))
	   note))
   *note-precision*))
(defun is-note (note)
  (let ((*note-precision* 1)) (realp (ignore-errors (note-to-num note)))))
(defun parse-usernote (no)
  (let ((a (when (consp no) (rest no)))
	(no (note-to-num (if (consp no) (first no) no))))
    (if a
	(cons no (mapcar (lambda (x) (if (listp x)
					 (if (list>1p x) (cons (acc-to-num (first x) 1) (acc-to-num (second x) 1/2)) (acc-to-num (first x) 1))
					 (acc-to-num x 1)))
			 a))
	no)))

(declaim (type cons +accnum+))
(defparameter +accnum+ '(("S" . 1) ("+" . 1) ("F" . -1) ("-" . -1) ("SS" . 2) ("++" . 2) ("FF" . -2) ("--" . -2) ("N" . 0)))
;;(declaim (inline acc-to-num))
(defun acc-to-num (acc prec)
  (if (symbolp acc) (lookup (symbol-name acc) +accnum+ :test #'string=)
      (roundto acc prec)))
(defun is-acc (acc)
  (typecase acc (integer (<= (abs acc) 2)) (symbol (find (symbol-name acc) +accnum+ :key #'car :test #'string=))))

(defun dur-to-num (dur bt)
  (if (and *cm-rhythmfun* *use-cm* (symbolp dur))
      (funcall *cm-rhythmfun* dur 60 bt)
      dur))
(defun is-dur (dur)
  (if (and *cm-rhythmfun* *use-cm* (symbolp dur))
      (ignore-errors (funcall *cm-rhythmfun* dur 60))
      (typep dur '(real (0)))))

(defparameter +notesym-type+
  '(or* real symbol
    (cons* (satisfies is-note)
     (or* null (list-of* (or* (satisfies is-acc) (list* (satisfies is-acc)) (list* (satisfies is-acc) (member -1/2 0 1/2))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLEFS

;; ordered list (lowest clef to highest) w/ note at middle staff line
(declaim (type cons +clefs+))
(defparameter +clefs+
  '((:subbass-8dn . 35) (:bass-8dn . 38) (:c-baritone-8dn . 41) (:f-baritone-8dn . 41) (:tenor-8dn . 45) (:subbass . 47) (:alto-8dn . 48) (:bass . 50) (:mezzosoprano-8dn . 52)
    (:c-baritone . 53) (:f-baritone . 53) (:soprano-8dn . 55) (:tenor . 57) (:subbass-8up . 59) (:treble-8dn . 59) (:alto . 60) (:bass-8up . 62) 
    (:mezzosoprano . 64) (:c-baritone-8up . 65) (:f-baritone-8up . 65) (:soprano . 67) (:tenor-8up . 69) (:treble . 71) (:alto-8up . 72) 
    (:mezzosoprano-8up . 76) (:soprano-8up . 79) (:treble-8up . 83) (:percussion . nil)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline is-clef))
  (defun is-clef (sym)
    "Utility function:
Returns the symbol if it refers to one of FOMUS's clefs (and NIL if it doesn't)"
    (declare (type symbol sym)) (find sym +clefs+ :key #'car)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSTRUMENTS

(defstruct (perc (:constructor make-perc-aux) (:copier nil) (:predicate percp))
  (sym nil :type (or symbol real))
  (staff 1 :type (integer 1))
  (voice 1 :type (integer 1))
  (note nil :type (or symbol integer))
  (autodur t :type boolean)
  (marks nil :type list)
  (midinote-im nil :type (or null (integer 0 127) cons))
  (midinote-ex nil :type (or null (integer 0 127))))
(declaim (type cons +perc-keys+))
(defparameter +perc-keys+ '(:sym :staff :voice :note :autodur :marks :midinote-im :midinote-ex))
(defprint-struct perc
    (perc-sym :sym) (perc-staff :staff) (perc-voice :voice) (perc-note :note) (perc-autodur :autodur) (perc-marks :marks)
    (perc-midinote-im :midinote-im) (perc-midinote-ex :midinote-ex))

(declaim (inline make-perc))
(defun make-perc (sym &rest args) (apply #'make-perc-aux :sym sym args))

(declaim (inline copy-perc))
(defun copy-perc (perc &key (sym (perc-sym perc)) (staff (perc-staff perc)) (note (perc-note perc)) (voice (perc-voice perc))
		  (autodur (perc-autodur perc)) (marks (perc-marks perc)) (midinote-im (perc-midinote-im perc)) (midinote-ex (perc-midinote-ex perc)))
  (declare (type perc perc) (type (or symbol real) sym) (type (integer 1) staff) (type (integer 1) voice) (type (or symbol integer) note)
	   (type boolean autodur) (type list marks) (type (or null (integer 0 127) cons) midinote-im) (type (or null (integer 0 127)) midinote-ex))
  (make-perc-aux :sym sym :staff staff :note note :voice voice :autodur autodur :marks marks :midinote-im midinote-im :midinote-ex midinote-ex))

(declaim (type cons +perc-type+))
(defparameter +perc-type+
  `(with-error* (perc "~~A of PERC struct ~S" ,#'perc-sym)
    (struct* perc
     (perc-sym (check* (or symbol real) "Found ~S, expected (OR SYMBOL REAL) in SYM slot" t))
     (perc-staff (check* (integer 1) "Found ~S, expected (INTEGER 1) in STAFF slot" t))
     (perc-voice (check* (integer 1) "Found ~S, expected (INTEGER 1) in VOICE slot" t))
     (perc-note (check* (or* null (type* +notesym-type+) integer) "Found ~S, expected NIL, INTEGER or valid note/accidental symbol in NOTE slot" t))
     (perc-autodur (check* boolean "Found ~S, expected BOOLEAN in PERC-AUTODUR slot" t))
     (perc-marks (check* (or* null (with-error* ("~~A in MARKS slot") (type* +markmarks-type+)))))
     (perc-midinote-im (check* (or null (integer 0 127) cons) "Found ~S, expected NIL, (INTEGER 0 127) or list of (INTEGER 0 127) in MIDINOTE-IM slot" t))
     (perc-midinote-ex (check* (or null (integer 0 127) cons) "Found ~S, expected NIL, (INTEGER 0 127) in MIDINOTE-EX slot" t)))))

(declaim (type list *percussion*))
(defparameter *percussion* nil)

(eval-when (:load-toplevel :execute)
  (declaim (type cons +percussion+))
  (defparameter +percussion+
    (list (make-perc :bass-drum :midinote-im '(35 36) :midinote-ex 35)
	  (make-perc :side-stick :midinote-im 37 :midinote-ex 37)
	  (make-perc :snare-drum :midinote-im '(38 40) :midinote-ex 38)
	  (make-perc :hand-clap :midinote-im 39 :midinote-ex 39)
	  (make-perc :low-floor-tom :midinote-im 41 :midinote-ex 41)
	  (make-perc :closed-hihat :midinote-im 42 :midinote-ex 42)
	  (make-perc :high-floor-tom :midinote-im 43 :midinote-ex 43)
	  (make-perc :pedal-hihat :midinote-im 44 :midinote-ex 44)
	  (make-perc :low-tom :midinote-im 45 :midinote-ex 45)
	  (make-perc :open-hihat :midinote-im 46 :midinote-ex 46)
	  (make-perc :low-mid-tom :midinote-im 47 :midinote-ex 47)
	  (make-perc :high-mid-tom :midinote-im 48 :midinote-ex 48)
	  (make-perc :crash-cymbal :midinote-im '(49 57) :midinote-ex 49)
	  (make-perc :high-tom :midinote-im 50 :midinote-ex 50)
	  (make-perc :ride-cymbal :midinote-im '(51 59) :midinote-ex 51)
	  (make-perc :chinese-cymbal :midinote-im 52 :midinote-ex 52)
	  (make-perc :ride-bell :midinote-im 53 :midinote-ex 53)
	  (make-perc :tambourine :midinote-im 54 :midinote-ex 54)
	  (make-perc :splash-cymbal :midinote-im 55 :midinote-ex 55)
	  (make-perc :cowbell :midinote-im 56 :midinote-ex 56)
	  (make-perc :vibraslap :midinote-im 58 :midinote-ex 58)
	  (make-perc :high-bongo :midinote-im 60 :midinote-ex 60)
	  (make-perc :low-bongo :midinote-im 61 :midinote-ex 61)
	  (make-perc :high-conga :midinote-im '(63 62) :midinote-ex 63)
	  (make-perc :low-conga :midinote-im 64 :midinote-ex 64)
	  (make-perc :high-timbale :midinote-im 65 :midinote-ex 65)
	  (make-perc :low-timbale :midinote-im 66 :midinote-ex 66)
	  (make-perc :high-agogo :midinote-im 67 :midinote-ex 67)
	  (make-perc :low-agogo :midinote-im 68 :midinote-ex 68)
	  (make-perc :cabasa :midinote-im 69 :midinote-ex 69)
	  (make-perc :maracas :midinote-im 70 :midinote-ex 70)
	  (make-perc :whistle :midinote-im '(71 72) :midinote-ex 71)
	  (make-perc :guiro :midinote-im '(73 74) :midinote-ex 73)
	  (make-perc :claves :midinote-im 75 :midinote-ex 75)
	  (make-perc :high-wood-block :midinote-im 76 :midinote-ex 76)
	  (make-perc :low-wood-block :midinote-im 77 :midinote-ex 77)
	  (make-perc :cuica :midinote-im '(79 78) :midinote-ex 79)
	  (make-perc :triangle :midinote-im '(81 80) :midinote-ex 81))))

;; all leglines data indicates maximum allowed before change of clef/use of ottava
;; leglines may be number or list of (number-default and/or (list :clef :up/down-keyword number))
;; 8up/down leglines = (cons into-ottava outof-ottava), or I think it can also be just a number
;;(declaim (inline make-instr))
(defstruct (instr (:constructor make-instr-aux) (:copier nil) (:predicate instrp))
  (sym nil :type (or symbol real))
  (clefs :treble :type (or symbol cons))
  (staves 1 :type (integer 1))
  (minp nil :type (or integer null))
  (maxp nil :type (or integer null))
  (simultlim 1 :type (or (integer 1) null))
  (tpose 0 :type (or null integer))
  (cleflegls 2 :type (or (integer 1) cons))
  (8uplegls nil :type (or null (integer 1) cons))
  (8dnlegls nil :type (or null (integer 1) cons))
  (percs nil :type list)
  (midiprgch-im nil :type (or null (integer 0 127) cons))
  (midiprgch-ex nil :type (or null (integer 0 127) cons)))
(declaim (type cons +instr-keys+))
(defparameter +instr-keys+ '(:sym :clefs :staves :minp :maxp :simultlim :tpose :cleflegls :8uplegls :8dnlegls :percs :midiprgch-im :midiprgch-ex))
(defprint-struct instr
    (instr-sym :sym) (instr-clefs :clefs) (instr-staves :staves) (instr-minp :minp) (instr-maxp :maxp) (instr-simultlim :simultlim)
    (instr-tpose :tpose) (instr-cleflegls :cleflegls) (instr-8uplegls :8uplegls) (instr-8dnlegls :8dnlegls) (instr-percs :percs)
    (instr-midiprgch-im :midiprgch-im) (instr-midiprgch-ex :midiprgch-ex))

(declaim (inline make-instr))
(defun make-instr (sym &rest args) (apply #'make-instr-aux :sym sym args))

(declaim (inline copy-instr))
(defun copy-instr (instr &key (sym (instr-sym instr)) (clefs (instr-clefs instr)) (staves (instr-staves instr)) (minp (instr-minp instr)) (maxp (instr-maxp instr))
		   (simultlim (instr-simultlim instr)) (tpose (instr-tpose instr)) (cleflegls (instr-cleflegls instr)) (8uplegls (instr-8uplegls instr))
		   (8dnlegls (instr-8dnlegls instr)) (percs (instr-percs instr)) (midiprgch-im (instr-midiprgch-im instr)) (midiprgch-ex (instr-midiprgch-ex instr)))
  (declare (type instr instr) (type (or symbol real) sym) (type (or symbol cons) clefs) (type (integer 1) staves) (type (or integer null) minp maxp)
	   (type (or (integer 1) null) simultlim) (type (or null integer) tpose) (type (or (integer 1) cons) cleflegls) (type (or null (integer 1) cons) 8uplegls 8dnlegls)
	   (type list percs) (type (or null (integer 0 127) cons) midiprgch-im) (type (or null (integer 0 127) cons) midiprgch-ex))
  (make-instr-aux :sym sym :clefs clefs :staves staves :minp minp :maxp maxp :simultlim simultlim :tpose tpose :cleflegls cleflegls
		  :8uplegls 8uplegls :8dnlegls 8dnlegls :percs percs :midiprgch-im midiprgch-im :midiprgch-ex midiprgch-ex))

(declaim (type cons +instr-type+))
(defparameter +instr-type+
  `(with-error* (instr "~~A of INSTR struct ~S" ,#'instr-sym)
    (struct* instr
     (instr-sym (check* (or symbol real) "Found ~S, expected (OR SYMBOL REAL) in SYM slot" t))
     (instr-clefs (check* (or* symbol (list-of* (satisfies is-clef))) "Found ~S, expected valid clef or list of valid clefs in CLEFS slot" t))
     (instr-staves (check* (integer 1) "Found ~S, expected (INTEGER 1) in STAVES slot" t))
     (instr-minp (check* (or null integer) "Found ~S, expected NIL or INTEGER in MINP slot" t))
     (instr-maxp (check* (or null integer) "Found ~S, expected NIL or INTEGER in MAXP slot" t))
     (instr-simultlim (check* (or null (integer 1)) "Found ~S, expected NIL or (INTEGER 1) in SIMULTLIM slot" t))
     (instr-tpose (check* (or null integer) "Found ~S, expected NIL or INTEGER in TPOSE slot" t))
     (instr-cleflegls (check* (or* (integer 1)
				   (cons-of* (integer 1)
					     (and* (list-of* (list* (and* symbol (check* (satisfies is-clef)
											 "Found ~S, expected valid clef symbol in list in CLEFLEGLS slot" t))
								    (and* symbol (check* (member :up :dn) "Found ~S, expected :UP or :DN in list in CLEFLEGLS slot" t))
								    (integer 1)))
						   (length* <= 2))))
			      "Found ~S, expected (INTEGERS 1) or SYMBOLS in the form I, (I (S S I) ...) in CLEFLEGLS slot" t))
     (instr-8uplegls (check* (or* null (integer 1) (list* (integer 1) (integer 1))) "Found ~S, expected NIL, (INTEGER 1) or ((INTEGER 1) (INTEGER 1)) in 8UPLEGLS slot" t))
     (instr-8dnlegls (check* (or* null (integer 1) (list* (integer 1) (integer 1))) "Found ~S, expected NIL, (INTEGER 1) or ((INTEGER 1) (INTEGER 1)) in 8DNLEGLS slot" t))
     (instr-percs (check* (or* null (list-of* (or* (type* +perc-type+) (cons* symbol (key-arg-pairs* ,@+perc-keys+)))))
			  "Found ~S, expected list of PERC objects or (SYMBOL/(INTEGER 0 127) KEYWORD/ARGUMENT-PAIRS...) in PERCS slot" t))
     (instr-midiprgch-im (check* (or* null (integer 0 127) (list-of* (integer 0 127)))
				 "Found ~S, expected NIL, (integer 0 127) or list of (integer 0 127) in MIDIPRGCH-IM slot" t))
     (instr-midiprgch-ex (check* (or* null (integer 0 127) (cons* (integer 0 127) key-arg-pairs*))
				 "Found ~S, expected NIL, (integer 0 127) or ((integer 0 127) KEYWORD-ARGUMENT-PAIRS...) in MIDIPRGCH-EX slot" t)))))

;; tpose = mod. for sounding pitch
;; 8up/8down = (threshold-for-ottava-brackets . threshold-for-back-to-normal)

(declaim (type list *instruments*))
(defparameter *instruments* nil)

;; MIDI Instruments not in here
;; 10 Music Box
;; 15 Dulcimer (add this)
;; 44 - 45 Tremolo Strings, Pizzicato Strings
;; 48 - 55 String Ens. 1, String Ens. 2, SynthStrings 1, SynthStrings 2, Choir Aahs, Voice Oohs, Synth Voice, Orch. Hit
;; 61 - 63 Brass Section, SynthBrass1, SynthBrass 2
;; 74 - 79 Recorder, Pan Flute, Blown Bottle, Shakuhachi, Whistle, Ocarina (add some of these)
;; 96 - 103 FXs (fucking dumb)
;; 104 - 109 Sitar, Banjo, Shamisen, Koto, Kalimba, Bagpipe (add these)
;; 111 Shanai (add)
;; 112 - ... Various Percussion
(eval-when (:load-toplevel :execute)
  (declaim (type instr *default-instr*))
  (defparameter *default-instr* (make-instr :default :staves 2 :clefs '(:treble :bass) :simultlim nil))
  (declaim (type cons +instruments+))
  (defparameter +instruments+
    (list (make-instr :piccolo :clefs :treble :tpose 12 :minp 74 :maxp 108 :midiprgch-im 72 :midiprgch-ex 72)
	  (make-instr :flute :clefs :treble :minp 60 :maxp 96 :midiprgch-im 73 :midiprgch-ex 73)
	  (make-instr :alto-flute :clefs :treble :tpose -5 :minp 55 :maxp 86 :midiprgch-ex 73)
	  (make-instr :bass-flute :clefs :treble :tpose -12 :minp 48 :maxp 79 :midiprgch-ex 73)
	  
	  (make-instr :oboe :clefs :treble :minp 58 :maxp 91 :midiprgch-im 68 :midiprgch-ex 68)
	  (make-instr :oboe-damore :clefs :treble :tpose -3 :minp 56 :maxp 86 :midiprgch-ex 86)
	  (make-instr :english-horn :clefs :treble :tpose -7 :minp 52 :maxp 82 :midiprgch-im 69 :midiprgch-ex 69)
	  (make-instr :baritone-oboe :clefs :treble :tpose -12 :minp 47 :maxp 77 :midiprgch-ex 69)
	  (make-instr :heckelphone :clefs :treble :tpose -12 :minp 45 :maxp 77 :midiprgch-ex 69)
	  
	  (make-instr :ef-clarinet :clefs :treble :tpose 3 :minp 55 :maxp 94 :midiprgch-ex 71)
	  (make-instr :d-clarinet :clefs :treble :tpose 2 :minp 54 :maxp 93 :midiprgch-ex 71)
	  (make-instr :bf-clarinet :clefs :treble :tpose -2 :minp 50 :maxp 91 :midiprgch-im 71 :midiprgch-ex 71)
	  (make-instr :a-clarinet :clefs :treble :tpose -3 :minp 49 :maxp 90 :midiprgch-ex 71)
	  (make-instr :alto-clarinet :clefs :treble :tpose -9 :minp 43 :maxp 77 :midiprgch-ex 71)
	  (make-instr :basset-horn :clefs :treble :tpose -7 :minp 41 :maxp 84 :midiprgch-ex 71)
	  (make-instr :bass-clarinet :clefs :treble :tpose -14 :minp 37 :maxp 72 :midiprgch-ex 71) 
	  (make-instr :contra-alto-clarinet :clefs :treble :tpose -21 :minp 30 :maxp 65 :midiprgch-ex 71)
	  (make-instr :contrabass-clarinet :clefs :treble :tpose -26 :minp 22 :maxp 60 :midiprgch-ex 71)
	  
	  (make-instr :bassoon :clefs '(:bass :tenor) :minp 34 :maxp 72 :midiprgch-im 70 :midiprgch-ex 70)
	  (make-instr :contrabassoon :clefs '(:bass :tenor) :tpose -12 :minp 22 :maxp 57 :midiprgch-ex 70)
	  
	  (make-instr :sopranino-saxophone :clefs :treble :tpose 3 :minp 61 :maxp 90 :midiprgch-ex 64)
	  (make-instr :soprano-saxophone :clefs :treble :tpose -2 :minp 56 :maxp 85 :midiprgch-im 64 :midiprgch-ex 64)
	  (make-instr :alto-saxophone :clefs :treble :tpose -9 :minp 49 :maxp 80 :midiprgch-im 65 :midiprgch-ex 65)
	  (make-instr :tenor-saxophone :clefs :treble :tpose -14 :minp 44 :maxp 75 :midiprgch-im 66 :midiprgch-ex 66)
	  (make-instr :baritone-saxophone :clefs :treble :tpose -21 :minp 37 :maxp 68 :midiprgch-im 67 :midiprgch-ex 67)
	  (make-instr :bass-saxophone :clefs :treble :tpose -26 :minp 32 :maxp 63 :midiprgch-ex 67)
	  (make-instr :contrabass-saxophone :clefs :treble :tpose -33 :minp 25 :maxp 56 :midiprgch-ex 67)
	  
	  (make-instr :f-alto-horn :clefs :treble :tpose -7 :minp 38 :maxp 68 :midiprgch-ex 60)
	  (make-instr :ef-alto-horn :clefs :treble :tpose -9 :minp 36 :maxp 66 :midiprgch-ex 60)
	  (make-instr :horn :clefs '(:treble :bass) :tpose -7 :minp 35 :maxp 77 :midiprgch-im 60)
	  (make-instr :bf-wagner-tuba :clefs '(:bass :treble) :tpose -2 :minp 40 :maxp 77 :midiprgch-ex 60)
	  (make-instr :f-wagner-tuba :clefs '(:bass :treble) :tpose -7 :minp 35 :maxp 77 :midiprgch-ex 60)
	  
	  (make-instr :ef-cornet :clefs :treble :tpose 3 :minp 57 :maxp 87 :midiprgch-ex 56)
	  (make-instr :bf-cornet :clefs :treble :tpose -2 :minp 52 :maxp 82 :midiprgch-ex 56)
	  (make-instr :bf-piccolo-trumpet :clefs :treble :tpose 10 :minp 59 :maxp 89 :midiprgch-ex 56)
	  (make-instr :a-piccolo-trumpet :clefs :treble :tpose 9 :minp 58 :maxp 88 :midiprgch-ex 56)
	  (make-instr :ef-trumpet :clefs :treble :tpose 3 :minp 57 :maxp 87 :midiprgch-ex 56)
	  (make-instr :d-trumpet :clefs :treble :tpose 2 :minp 56 :maxp 86 :midiprgch-ex 56)
	  (make-instr :c-trumpet :clefs :treble :minp 52 :maxp 84 :midiprgch-ex 56)
	  (make-instr :bf-trumpet :clefs :treble :tpose -2 :minp 52 :maxp 82 :midiprgch-im '(56 59) :midiprgch-ex 56)
	  (make-instr :flugelhorn :clefs :treble :tpose -2 :minp 52 :maxp 82 :midiprgch-ex 56)
	  (make-instr :ef-bass-trumpet :clefs :treble :tpose -26 :minp 33 :maxp 63 :midiprgch-ex 56)
	  (make-instr :bf-bass-trumpet :clefs :treble :tpose -26 :minp 28 :maxp 58 :midiprgch-ex 56)
	  
	  (make-instr :alto-trombone :clefs :alto :minp 45 :maxp 77 :midiprgch-ex 57)
	  (make-instr :tenor-trombone :clefs '(:bass :tenor) :minp 40 :maxp 72 :midiprgch-im 57 :midiprgch-ex 57)
	  (make-instr :bass-trombone :clefs '(:bass :tenor) :minp 36 :maxp 71 :midiprgch-ex 57)
	  (make-instr :contrabass-trombone :clefs :bass :minp 28 :maxp 58 :midiprgch-ex 57)

	  (make-instr :baritone :clefs :bass :minp 40 :maxp 70 :midiprgch-ex 58)
	  (make-instr :euphonium :clefs :bass :minp 40 :maxp 70 :midiprgch-ex 58)
	  (make-instr :tuba :clefs :bass :minp 26 :maxp 67 :midiprgch-im 58 :midiprgch-ex 58)

	  (make-instr :timpani :clefs :bass :minp 38 :maxp 60 :midiprgch-im 47 :midiprgch-ex 47)
	  (make-instr :percussion :clefs :percussion)
	  (make-instr :glockenspiel :clefs :treble :tpose 24 :simultlim 2 :minp 79 :maxp 108 :midiprgch-im 9 :midiprgch-ex 9)
	  (make-instr :xylophone :clefs '(:treble) :simultlim 2 :tpose 12 :8uplegls '(5 2) :minp 69 :maxp 108 :midiprgch-im 13 :midiprgch-ex 13)
	  (make-instr :vibraphone :clefs :treble :simultlim 2 :minp 53 :maxp 89 :midiprgch-im 11 :midiprgch-ex 11)
	  (make-instr :marimba :clefs '(:treble :bass) :simultlim 2 :8uplegls '(5 2) :minp 45 :maxp 96 :midiprgch-im 12 :midiprgch-ex 12)
	  (make-instr :chimes :clefs :treble :minp 60 :maxp 77 :midiprgch-im 14 :midiprgch-ex 14)
	  (make-instr :celesta :clefs '(:treble :bass) :simultlim 5 :tpose 12 :simultlim 5 :8uplegls '(5 2) :minp 60 :maxp 112
		      :midiprgch-im 8 :midiprgch-ex 8)
	  
	  (make-instr :troubadour-harp :clefs '(:treble :bass) :staves 2 :simultlim 5 :minp 36 :maxp 92 :midiprgch-ex 46)
	  (make-instr :harp :clefs '(:treble :bass) :staves 2 :simultlim 5 :8uplegls '(5 2) :8dnlegls '(5 2) :minp 20 :maxp 104 :midiprgch-im 46 :midiprgch-ex 46)
	  (make-instr :piano :clefs '(:treble :bass) :staves 2 :simultlim 5 :8uplegls '(5 2) :8dnlegls '(5 2) :minp 21 :maxp 108
		      :midiprgch-im '(0 1 2 3) :midiprgch-ex 0)
	  (make-instr :electric-piano :clefs '(:treble :bass) :staves 2 :simultlim 5 :8uplegls '(5 2) :8dnlegls '(5 2) :minp 21 :maxp 108
		      :midiprgch-im '(4 5 7 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95) :midiprgch-ex 4)
	  (make-instr :harpsichord :clefs '(:treble :bass) :staves 2 :minp 29 :maxp 89 :midiprgch-im 6 :midiprgch-ex 6)
	  (make-instr :harmonium :clefs '(:bass :treble) :staves 2 :simultlim 5 :minp 29 :maxp 89 :midiprgch-im '(16 17 18 20) :midiprgch-ex 16)
	  (make-instr :organ-manuals :clefs '(:treble :bass) :8uplegls '(5 2) :staves 2 :minp 36 :maxp 96 :midiprgch-im 19 :midiprgch-ex 19)
	  (make-instr :organ-pedals :clefs :bass :minp 36 :maxp 67 :midiprgch-ex 19)

	  (make-instr :accordion :clefs :treble :minp 41 :maxp 105 :midiprgch-im '(21 23) :midiprgch-ex 21)
	  (make-instr :harmonica :clefs '(:bass :treble) :minp 28 :maxp 103 :midiprgch-im 22 :midiprgch-ex 22)
	  (make-instr :ukulele :clefs :treble :minp 60 :maxp 81 :midiprgch-ex 24)
	  (make-instr :mandolin :clefs :treble :minp 55 :maxp 85 :midiprgch-ex 24)
	  (make-instr :guitar :clefs :treble :tpose -12 :minp 40 :maxp 83 :midiprgch-im '(24 25 26 27 28 29 30 31) :midiprgch-ex 24)
	  (make-instr :bass-guitar :clefs :bass :tpose -12 :minp 28 :maxp 60 :midiprgch-im '(32 33 34 35 36 37 38 39) :midiprgch-ex 32)

	  (make-instr :soprano :clefs :treble :minp 56 :maxp 87 :midiprgch-ex 52)
	  (make-instr :mezzo-soprano :clefs :treble :minp 55 :maxp 82 :midiprgch-ex 52)
	  (make-instr :contralto :clefs :treble :minp 53 :maxp 78 :midiprgch-ex 52)
	  (make-instr :tenor :clefs :treble :minp 55 :maxp 78 :midiprgch-ex 53)
	  (make-instr :tenor-8dn :clefs :treble-8dn :minp 60 :maxp 84 :midiprgch-ex 53)
	  (make-instr :baritone :clefs :bass :minp 42 :maxp 67 :midiprgch-ex 53)
	  (make-instr :bass :clefs :bass :minp 34 :maxp 65 :midiprgch-ex 53)
	  
	  (make-instr :soprano-choir :clefs :treble :minp 60 :maxp 81 :midiprgch-ex 52)
	  (make-instr :alto-choir :clefs :treble :minp 54 :maxp 77 :midiprgch-ex 52)
	  (make-instr :tenor-choir :clefs :treble-8dn :minp 48 :maxp 69 :midiprgch-ex 53)
	  (make-instr :bass-choir :clefs :bass :minp 40 :maxp 62 :midiprgch-ex 53)
	  
	  (make-instr :violin :clefs :treble :8uplegls '(5 2) :minp 55 :maxp 103 :midiprgch-im '(40 110) :midiprgch-ex '(40 :pizz 45))
	  (make-instr :viola :clefs '(:treble :alto) :8uplegls '(5 2) :minp 48 :maxp 93 :midiprgch-im 41 :midiprgch-ex '(41 :pizz 45))
	  (make-instr :cello :clefs '(:bass :tenor :treble) :minp 36 :maxp 84 :midiprgch-im 42 :midiprgch-ex '(42 :pizz 45))
	  (make-instr :contrabass :clefs '(:bass :tenor) :tpose -12 :minp 28 :maxp 67 :midiprgch-im 43 :midiprgch-ex '(43 :pizz 45)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline is-instr))
  (defun is-instr (sym)
    "Utility function:
Returns an INSTR object given an ID symbol (or NIL if there is no such
instrument)"
    (declare (type symbol sym))
    (or (find sym *instruments* :key #'perc-sym) (find sym +instruments+ :key #'perc-sym))))

(declaim (type cons +instr-group-tree-type-aux+ +instr-group-tree-type+))
(defparameter +instr-group-tree-type-aux+    
  '(or* (satisfies is-instr) (cons* (member :group :choirgroup :grandstaff) (list-of* (type* +instr-group-tree-type-aux+)))))
(defparameter +instr-group-tree-type+
  '(list-of* (cons* symbol (list-of* (type* +instr-group-tree-type-aux+)))))

(declaim (type list *instr-groups*) (type cons +instr-groups+))
(defparameter *instr-groups* nil)
(defparameter +instr-groups+
  (list '(:orchestra
	  (:group
	   (:group :piccolo :flute :alto-flute :bass-flute)
	   (:group :oboe :oboe-damore :english-horn :baritone-oboe :heckelphone)
	   (:group :ef-clarinet :d-clarinet :bf-clarinet :a-clarinet :alto-clarinet :basset-horn :bass-clarinet :contra-alto-clarinet :contrabass-clarinet)
	   (:group :sopranino-saxophone :soprano-saxophone :alto-saxophone :tenor-saxophone :baritone-saxophone :bass-saxophone :contrabass-saxophone)
	   (:group :bassoon :contra-bassoon))
	  (:group
	   (:group :f-alto-horn :ef-alto-horn :horn :bf-wagner-tuba :f-wagner-tuba)
	   (:group :ef-cornet :bf-cornet :bf-piccolo-trumpet :a-piccolo-trumpet :ef-trumpet :d-trumpet :c-trumpet :bf-trumpet
	    :flugelhorn :ef-bass-trumpet :bf-bass-trumpet)
	   (:group :alto-trombone :tenor-trombone :bass-trombone :contrabass-trombone)
	   (:group :baritone :euphonium :tuba))
	  :timpani :percussion 
	  :glockenspiel :xylophone :vibraphone :marimba :chimes :celesta
	  (:grandstaff :troubadour-harp) (:grandstaff :harp) (:grandstaff :piano) (:grandstaff :electric-piano) (:grandstaff :harpsichord)
	  (:grandstaff :harmonium) (:group (:grandstaff :organ-manuals) :organ-pedals)
	  :accordion :harmonica :ukulele :mandolin :guitar :bass-guitar
	  :soprano :mezzo-soprano :contralto :tenor :tenor-8dn :baritone :bass
	  (:group :soprano-choir :alto-choir :tenor-choir :bass-choir)
	  (:group (:group :violin) (:group :viola) (:group :cello) (:group :contrabass)))

	(cons :small-ensemble
	      (loop for e in +instruments+
		    for sy = (instr-sym e)
		    if (or (eq sy :percussion) (find sy '(:timpani :glockenspiel :xylophone :vibraphone :marimba :chimes :celesta))) collect (list :group sy) into p
		    else if (eq sy :organ-manuals) collect '(:group (:grandstaff :organ-manuals) :organ-pedals) into k
		    else if (eq sy :organ-pedals) do (progn nil)
		    else if (= (instr-staves e) 2) collect (list :grandstaff sy) into k
		    else if (find sy '(:soprano :mezzo-soprano :contralto :tenor :tenor-8dn :baritone :bass)) collect sy into v
		    else if (find sy '(:soprano-choir :alto-choir :tenor-choir :bass-choir)) collect sy into c
		    else collect (cons (list :group sy) (instr-minp e)) into i
		    finally (return (nconc (mapcar #'car (sort i #'> :key #'cdr)) p
					   (list (cons :choirgroup v)) (list (cons :choirgroup c)) k))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFAULT DIVISIONS

;; list of default measure divisions
;; numbers are beat values
;;(declaim (type boolean *use-default-meas-divs*))
;;(defparameter *use-default-meas-divs* t)
(declaim (type list *default-meas-divs*) (type cons +default-meas-divs+))
(defparameter *default-meas-divs* nil) ; to override some or all of the defaults

(defparameter +default-meas-divs+
  '((2 (1 1))
    (3 (2 1) (1 2))
    (4 (2 2))
    (5 (3 2) (2 3))
    (6 (4 2) (2 4))
    (7 (4 3) (3 4))
    (8 (4 4) (3 3 2) (3 2 3) (2 3 3))
    (9 (4 2 3) (4 3 2) (2 3 4) (3 2 4))
    (10 (8 2) (2 8))
    (11 (8 3) (3 8))
    (12 (8 4) (4 8))
    (13 (8 2 3) (8 3 2) (2 3 8) (3 2 8))))

;;(declaim (type boolean *use-default-tuplet-divs*))
;;(defparameter *use-default-tuplet-divs* t)
(declaim (type list *default-tuplet-divs*) (type cons +default-tuplet-divs+))
(defparameter *default-tuplet-divs* nil) ; to override some or all of the defaults

(defparameter +default-tuplet-divs+
  '((2 (1 1))
    (3 (2 1) (1 2))
    (4 (2 2))
    (5 (3 2) (2 3) (4 1) (1 4))
    (6 (4 2) (2 4))
    (7 (4 3) (3 4))
    (8 (4 4) (3 3 2) (3 2 3) (2 3 3))
    (9 (4 2 3) (4 3 2) (2 3 4) (3 2 4) (8 1) (1 8))
    (10 (8 2) (2 8))
    (11 (8 3) (3 8))
    (12 (8 4) (4 8))
    (13 (8 2 3) (8 3 2) (2 3 8) (3 2 8))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TIMESIG/PART PROPERTIES

(declaim (type cons +timesig-props+ +part-props+))
(defparameter +timesig-props+
  `(with-unique* (sy)
    (list-of*
     (check*
      (or*
       (let* ((x (unique* sy (member :barline))))
	 (list* x (member :single :double :final :repeatleft :repeatright :repeatleftright :invisible)))
       (let* ((x (unique* sy (member :keysig))))
	 (cons* x (or* (list* (member :cmaj :amin
				      :gmaj :emin
				      :dmaj :bmin
				      :amaj :f+min :fsmin
				      :emaj :c+min :csmin
				      :bmaj :g+min :gsmin :c-maj :cfmaj :a-min :afmin
				      :f+maj :fsmaj :d+min :dsmin :g-maj :gfmaj :e-min :efmin
				      :c+maj :csmaj :a+min :asmin :d-maj :dfmaj :b-min :bfmin
				      :a-maj :afmaj :fmin
				      :e-maj :efmaj :cmin
				      :b-maj :bfmaj :gmin
				      :fmaj :dmin))
		       null
		       (with-unique* (k)
			 (list-of* (or* (unique* k :c (member :c+ :cs :d- :df))
					(unique* k :d (member :d+ :ds :e- :ef))
					(unique* k :f (member :f+ :fs :g- :gf))
					(unique* k :g (member :g+ :gs :a- :af))
					(unique* k :a (member :a+ :as :b- :bf)))))))))
      "Found ~S, expected valid property" t))))
(defparameter +part-props+
  `(with-unique* (sy)
    (list-of*
     (check*
      (or*
       (let* ((x (unique* sy (member :distr))))
	 (cons* x (list-of* (cons* (or symbol real) (list-of* (or* (integer 1) (list* (integer 1) (integer 1)))))))))
      "Found ~S, expected valid property" t))))
	   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETTINGS

;; exported symbols/arguments to main function
(declaim (type cons +settings+))
(defparameter +settings+
  `((:debug-filename (or null string)) (:verbose (integer 0 2)) 
    (:use-cm boolean) (:cm-scale t)
    (:backend (or* symbol (cons* symbol key-arg-pairs*) (list-of* (or* symbol (cons* symbol key-arg-pairs*))))
     "(SYMBOL KEYWORD/ARGUMENT-PAIRS...) or list of (SYMBOL KEYWORD/ARGUMENT-PAIRS...)") ; deprecated
    (:output (or* symbol (cons* symbol key-arg-pairs*) (list-of* (or* symbol (cons* symbol key-arg-pairs*))))
     "(SYMBOL KEYWORD/ARGUMENT-PAIRS...) or list of (SYMBOL KEYWORD/ARGUMENT-PAIRS...)")
    (:filename string)
    (:quality real)

    (:global (or* null (list-of* (type* +timesig-type+))) "list of TIMESIG objects")
    (:parts (list-of* (type* +part-type+)) "list of PART objects")
    (:events (or* null (list-of* (or* (type* +note-type+) (type* +rest-type+) (type* +mark-type+)))) "list of NOTE or REST objects")
    (:chunks (or null list)) ;; need this?
    
    (:check-ranges boolean) (:transpose boolean) (:input-beat-value (or null (real (0)))) (:input-offset (or null real))
    (:instruments (or* null (list-of* (or* (type* +instr-type+) (cons* symbol (key-arg-pairs* ,@+instr-keys+))))) "list of INSTR objects")
    (:percussion (or* null (list-of* (or* (type* +perc-type+) (cons* symbol (key-arg-pairs* ,@+instr-keys+))))) "list of PERC objects")
    (:instr-groups (or* null (type* +instr-group-tree-type+)) "list of nested lists of SYMBOLS")
    (:default-instr (type* +instr-type+) "INSTR object")
    (:ensemble-type (or* null symbol (cons* symbol (list-of* +instr-group-tree-type-aux+))) "NIL, SYMBOL or nested lists of SYMBOLS")

    (:title (or null string)) (:subtitle (or null string)) (:composer (or null string))
    (:timesig-style (member nil :fraction :common) "NIL, :FRACTION or :COMMON")
    (:tuplet-style (member nil :ratio :single) "NIL, :RATIO or :SINGLE")

    (:default-grace-dur (rational (0))) (:default-grace-num integer) (:effective-grace-dur-mul (rational (0)))
    
    (:min-auto-timesig-dur (rational (0))) (:default-timesig (type* +timesig-repl-type+) "TIMESIG object")
    (:auto-timesig-comp boolean)

    (:quartertones boolean)
    (:auto-accidentals boolean) (:auto-cautionary-accs boolean) (:auto-staff/clef-changes boolean)
    (:auto-ottavas boolean) (:auto-grace-slurs boolean) (:auto-voicing boolean) (:auto-beams boolean)
    (:auto-quantize boolean) (:auto-multivoice-rests boolean) (:auto-multivoice-notes boolean)
    (:auto-override-timesigs boolean) 
    (:auto-pizz/arco boolean) (:auto-dyn-nodup boolean) (:auto-percussion-durs boolean)

    (:split-plugin symbol) (:auto-accs-plugin symbol) (:auto-voices-plugin symbol) (:auto-distr-rests-plugin symbol) ; -plugins are deprecated
    (:auto-multivoice-comb-plugin symbol) (:auto-ottavas-plugin symbol) (:auto-beam-plugin symbol) (:auto-quantize-plugin symbol)
    (:auto-staff/clefs-plugin symbol)
    (:split-module symbol) (:auto-accs-module symbol) (:auto-voices-module symbol) (:auto-distr-rests-module symbol)
    (:auto-multivoice-comb-module symbol) (:auto-ottavas-module symbol) (:auto-beam-module symbol) (:auto-quantize-module symbol)
    (:auto-staff/clefs-module symbol) (:tuplet-function symbol)

    (:default-tuplets (or* null (list-of* (list* (integer 1) (integer 1)))) "list of ((INTEGER 1) (INTEGER 1))")
    (:default-meas-divs (or* null (list-of* (cons* (rational (0)) (list-of* (list-of* (rational (0))))))) "list of ((RATIONAL (0)) ((RATIONAL (0)) ...) ...)")
    (:default-tuplet-divs (or* null (list-of* (cons* (integer 1) (list-of* (list-of* (integer 1)))))) "list of ((INTEGER 1) ((INTEGER 1) ...) ...)")

    (:default-beat (or null (rational (0)) (cons (rational (0)) (rational (0)))))
    (:beat-division (or* (integer 1) (and* (list* (integer 1) (integer 1)))) "(INTEGER 1) or ((INTEGER 1) (INTEGER 1))")
    (:min-tuplet-dur (real (0))) (:max-tuplet-dur (real (0))) 
    (:max-tuplet (or* null (integer 2) (list-of* (integer 2))) "NIL, (INTEGER 2) or list of (INTEGER 2)") 
    (:tuplet-dotted-rests boolean) (:double-dotted-notes boolean)
    (:dotted-note-level (member t :all :top :sig) "T, :ALL, :TOP or :SIG")
    (:shortlongshort-notes-level (member t :all :top :sig) "T, :ALL, :TOP or :SIG")
    (:syncopated-notes-level boolean)
    (:min-split-all-parts-dur (real (0)))
    
    (:max-caut-acc-dist (real (0))) (:use-double-accs boolean) (:acc-throughout-meas boolean) (:caut-acc-ottavas boolean)
    (:caut-acc-octaves (or boolean (integer 1))) (:caut-acc-next-meas (or boolean (integer 1 2))) 

    (:max-ottava-rest-dist (real (0)))

    (:long-eighth-beams (member nil t :always) "NIL, T or :ALWAYS")
    (:long-eighth-beam-count (integer 1))
    (:comp-long-eighth-beam-count (integer 1))

    (:min-multivoice-chords-dur (real (0)))

    (:grandstaff-hide-rests (member nil t :some :all) "NIL, T, :SOME or :ALL")
    (:min-grandstaff-hide-rests-dur (real (0)))

    #-fomus-nolilypond (:lilypond-exe string) #-fomus-nolilypond (:lilypond-version (or null string))
    #-fomus-nolilypond (:lilypond-opts (or* null (list-of* string)) "NIL or list of STRINGS")
    #-fomus-nolilypond (:lilypond-out-ext string)
    #-fomus-nolilypond (:lilypond-view-exe string)
    #-fomus-nolilypond (:lilypond-view-opts (or* null (list-of* string)) "NIL or list of STRINGS")

    #-fomus-nolilypond (:lilypond-filehead (or* null string (list-of* string)) "NIL, STRING or list of STRING") 
    #-fomus-nolilypond (:lilypond-scorehead (or* null string (list-of* string)) "NIL, STRING or list of STRING")
    #-fomus-nolilypond (:lilypond-text-markup string)
    #-fomus-nolilypond (:lilypond-textdyn-markup string)
    #-fomus-nolilypond (:lilypond-texttempo-markup string)
    #-fomus-nolilypond (:lilypond-textnote-markup string)
    #-fomus-nolilypond (:lilypond-textacc-markup string)
    
    #-fomus-nocmn (:cmn-view-exe string)
    #-fomus-nocmn (:cmn-view-opts (or* null (list-of* string)) "NIL or list of STRINGS")    
    ))

(declaim (type list +deprecated-repl+))
(defparameter +deprecated-repl+
  '((:backend . :output)
    (:split-plugin . :split-module) (:auto-accs-plugin . :auto-accs-module) (:auto-voices-plugin . :auto-voices-module) (:auto-distr-rests-plugin . :auto-distr-rests-module)
    (:auto-multivoice-comb-plugin . :auto-multivoice-comb-module) (:auto-ottavas-plugin . :auto-ottavas-module) (:auto-beam-plugin . :auto-beam-module)
    (:auto-quantize-plugin . :auto-quantize-module) (:auto-staff/clefs-plugin . :auto-staff/clefs-module)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MARKS

;; for bar objects: :varcoda :coda :segno 
(declaim (type cons +notemark-type+))
(defparameter +notemark-type+
  '(or*
    (let* ((x (member :ignore)))
      (or* x (list* x)))
    (let* ((x (unique* sy (member :arco :pizz
				  :start8down- :8down- :end8down- :8down :start8up- :8up- :end8up- :8up
				  :startwedge> :startwedge< :wedge< :wedge> :endwedge< :endwedge>
				  :startwedge>* :startwedge<* :wedge<* :wedge>* :endwedge<* :endwedge>*
				  :startgraceslur- :graceslur- :endgraceslur-
				  :clef- :endclef- :staff :endstaff-
				  :cautacc :autodur :forceacc
				  :rfz :sfz :spp :sp :sff :sf :fp :ffffff :fffff :ffff :fff :ff :f :mf :mp :p :pp :ppp :pppp :ppppp :pppppp
				  :rfz* :sfz* :spp* :sp* :sff* :sf* :fp* :ffffff* :fffff* :ffff* :fff* :ff* :f* :mf* :mp* :p* :pp* :ppp* :pppp* :ppppp* :pppppp*))))
      (or* x (list* x)))	 ; spanners w/ only 1 level, non-articulations 
    (let* ((x (unique* sy (member :fermata))))
      (or* x (list* x) (list* x (member :short :long :verylong))))
    (let* ((x (unique* sy (member :arpeggio))))
      (or* x (list* x) (list* x (member :up :down))))
    (let* ((x (member :portamento :glissando))) ; default is before
      (or* (unique* sy :glissbefore x) (list* (unique* sy :glissbefore x))
	   (list* (unique* sy :glissbefore x) (member :before)) (list* (unique* sy :glissafter x) (member :after))))
    (let* ((x (member :breath)))
      (or* (unique* sy :breathafter x) (list* (unique* sy :breathafter x))
	   (list* (unique* sy :breathbefore x) (member :before)) (list* (unique* sy :breathafter x) (member :after))))
    (let* ((x (member :tie)))
      (or* (unique* sy :tieafter x) (list* (unique* sy :tieafter x))
	   (list* (unique* sy :tiebefore x) (member :before)) (list* (unique* sy :tieafter x) (member :after))))
    (let* ((x (member :harmonic)))
      (or* (cons* (unique* sy :harmtouched x)
		  (or* (list* (type* +notesym-type+)) (list* (member :touched) (type* +notesym-type+)) (list* (type* +notesym-type+) (member :touched))))
	   (cons* (unique* sy :harmsounding x)
		  (or* (list* (member :sounding) (type* +notesym-type+)) (list* (type* +notesym-type+) (member :sounding))))))
    (let* ((x (unique* sy (member :stopped :open :staccato :staccatissimo
				  :righttoe :lefttoe :rightheel :leftheel
				  :thumb :downbow :upbow :portato :tenuto :marcato :accent))))
      (or* x (list* x) (list* x integer))) ; articulations, dynamics, some spanners
    (let* ((x (unique* sy (member :longtrill :mordent :prall :trill))))
      (or* x (list* x) (list* x (type* +notesym-type+))))
    (let* ((x (unique* sy :clef (member :clef :startclef-))))
      (list* x (satisfies is-clef)))
    (let* ((x (unique* sy :staff (member :staff :startstaff-))))
      (cons* x (list-of* (integer 1))))
    (let* ((x (unique* sy (member :notehead))))
      (list* x (member :harmonic :diamond :x :xcircle :triangle :slash)))	
    (let* ((x (unique* sy (member :size))))
      (list* x (member :small :tiny)))	
    (let* ((x (unique* sy :tremolo (member :tremolo :tremolofirst :tremolosecond))))
      (or* x (list* x) (list* x (rational (0))) (list* x (rational (0)) (member :notated)) (list* x (member :notated) (rational (0))))) ; tremolos
    (let* ((x (member :startslur-)))
      (or* (unique* si 1 x) (unique* si 1 (list* x))
	   (cons* x (or* (list* (unique* si (integer 1)))
			 (list* (unique* si 1 (member :dotted)))
			 (list* (unique* si (integer 1)) (member :dotted))
			 (list* (member :dotted) (unique* si (integer 1))))))) ; startslur-
    (let* ((x (member :slur- :endslur-)))
      (or* (unique* si 1 x) (unique* si 1 (list* x)) (list* x (unique* si (integer 1)))))
    (let* ((x (member :textnote :texttempo :textdyn :text)))
      (or* (list* x string) (list* x string (member :up :down :nopos :detached)) (list* x (member :up :down :nopos :detached) string)))
    (let* ((x (member :text- :endtext-)))
      (or* (unique* tx 1 x) (unique* tx 1 (list* x)) (list* x (unique* tx (integer 1)))))
    (let* ((x (member :starttext-)))
      (cons* x (or* (unique* tx 1 (list* string))
		    (unique* tx 1 (list* string (member :up :down :nopos :detached)))
		    (unique* tx 1 (list* (member :up :down :nopos :detached) string))
		    (list* string (unique* tx (integer 1)))
		    (list* (unique* tx (integer 1)) string)
		    (list* (member :up :down :nopos :detached) string (unique* tx (integer 1)))
		    (list* (member :up :down :nopos :detached) (unique* tx (integer 1)) string)
		    (list* string (member :up :down :nopos :detached) (unique* tx (integer 1)))
		    (list* (unique* tx (integer 1)) (member :up :down :nopos :detached) string)
		    (list* string (unique* tx (integer 1)) (member :up :down :nopos :detached))
		    (list* (unique* tx (integer 1)) string (member :up :down :nopos :detached)))))))

(declaim (type string *checktype-markerr* *checktype-markserr*))
(defparameter *checktype-markerr* "Found ~S, expected valid/unique mark")
(defparameter *checktype-markserr* "Found ~S, expected list of valid marks")

(declaim (type cons +notemarks-type+ +markmarks-type+ +restmarks-type+ +marks-rests+))
(defparameter +notemarks-type+
  '(check*
    (with-unique* (sy si tx)
      (list-of*
       (check* (type* +notemark-type+) *checktype-markerr* t)))
    *checktype-markserr* t))

(defparameter +markmarks-type+
  '(check*
    (with-unique* (sy si tx)
      (list-of*
       (check* (or* (type* +notemark-type+)
		    (cons* (member :mark) (cons* (or* (real 0) (list* real)) (and* list (type* +notemark-type+)))))
	       *checktype-markerr* t)))
    *checktype-markserr* t))

(defparameter +restmarks-type+
  '(and*
    (check*
     (list-of* (check* (or* (satisfies is-restmarksym) (cons (satisfies is-restmarksym) list)) *checktype-markerr* t))
     *checktype-markserr* t)
    (type* +notemarks-type+)))

;; include :staff but not :clef
(defparameter +marks-rests+
  '(:fermata :notehead :textnote :texttempo :textdyn :text :text- :endtext- :starttext- :size :staff :staff- :startstaff- :endstaff-))

(defparameter +marks-first-rest+
  '(:textnote :texttempo :textdyn :text :text- :starttext-))
(defparameter +marks-last-rest+
  '(:fermata :endtext-))

(declaim (inline is-restmarksym))
(defun is-restmarksym (sym)
  (find sym +marks-rests+))

(declaim (type cons +marks-important+))
(defparameter +marks-important+
  '(:arco :pizz :startgraceslur- :graceslur- :endgraceslur- :startwedge> :startwedge< :wedge< :wedge> :endwedge< :endwedge>
    :startwedge>* :startwedge<* :wedge<* :wedge>* :endwedge<* :endwedge>*
    :rfz :sfz :spp :sp :sff :sf :fp :ffffff :fffff :ffff :fff :ff :f :mf :mp :p :pp :ppp :pppp :ppppp :pppppp
    :rfz* :sfz* :spp* :sp* :sff* :sf* :fp* :ffffff* :fffff* :ffff* :fff* :ff* :f* :mf* :mp* :p* :pp* :ppp* :pppp* :ppppp* :pppppp* 
    :fermata :arpeggio :glissando :breath :tie :harmonic
    :stopped :open :staccato :staccatissimo
    :mordent :prall :trill :longtrill :longtrill- :righttoe :lefttoe :rightheel :leftheel
    :thumb :downbow :upbow :portato :tenuto :marcato :accent :notehead :size
    :startslur- :slur- :endslur- :textnote :textdyn))
    
(declaim (type boolean *auto-pizz/arco* *auto-dyn-nodup*))
(defparameter *auto-pizz/arco* t)
(defparameter *auto-dyn-nodup* t)

;; marks only at beginning or end of tied notes
(declaim (type cons +marks-first-tie+ +marks-last-tie+ +marks-onoff+ +marks-before-after+ +marks-indiv-voices+
	       +marks-spanner-voices+ +marks-spanner-staves+ +marks-expand+ +marks-defaultup+ +marks-withacc+ +marks-withaccdn+
	       +marks-midistaff+ +marks-midistaffends+))
(defparameter +marks-first-tie+
  '(:startslur- :startgraceslur- :start8up- :start8down- :starttext- :startwedge< :startwedge> :startwedge<* :startwedge>* :endgraceslur-
    :pppppp :ppppp :pppp :ppp :pp :p :mp :mf :f :ff :fff :ffff :fffff :ffffff :fp :sf :sff :sp :spp :sfz :rfz
    :pppppp* :ppppp* :pppp* :ppp* :pp* :p* :mp* :mf* :f* :ff* :fff* :ffff* :fffff* :ffffff* :fp* :sf* :sff* :sp* :spp* :sfz* :rfz* 
    :text :textdyn :textnote :texttempo 
    :accent :marcato :tenuto :portato
    :upbow :downbow :thumb :leftheel :rightheel :lefttoe :righttoe
    :trill :prall :mordent  
    :pizz :arco :open :stopped (:breath :before) (:tie :before)
    :arpeggio (:glissando :before) (:portamento :before) ; special ones
    :cautacc :8up :8down :clef :staff :longtrill :startlongtrill-))
(defparameter +marks-last-tie+
  '(:endslur- :end8up- :end8down- :endtext- :endwedge< :endwedge> :endwedge<* :endwedge>*
    :fermata :staccatissimo :staccato (:breath :after) (:tie :after) :endlongtrill-
    (:glissando :after) (:portamento :after)))

(defparameter +marks-midistaff+
  '((:startwedge>* . :startwedge>) (:startwedge<* . :startwedge<) (:wedge<* . :wedge<) (:wedge>* . :wedge>) (:endwedge<* . :endwedge<) (:endwedge>* . :endwedge>)
    (:rfz* . :rfz) (:sfz* . :sfz) (:spp* . :spp) (:sp* . :sp) (:sff* . :sff) (:sf* . :sf) (:fp* . :fp) (:ffffff* . :ffffff) (:fffff* . :fffff) (:ffff* . :ffff)
    (:fff* . :fff) (:ff* . :ff) (:f* . :f) (:mf* . :mf) (:mp* . :mp) (:p* . :p) (:pp* . :pp) (:ppp* . :ppp) (:pppp* . :pppp) (:ppppp* . :ppppp) (:pppppp* . :pppppp)))
(defparameter +marks-midistaffends+ '(:wedge<* :wedge>* :endwedge<* :endwedge>*))

(defparameter +marks-withacc+
  '(:longtrill :prall :trill :mordent)) ; don't want spanners in this list!
(defparameter +marks-withaccdn+
  '(:mordent))

(defparameter +marks-onoff+
  '((*auto-pizz/arco* . (:pizz . :arco)) (nil . ((:size :small) . (:size :normal 2))) (nil . ((:size :tiny) . (:size :normal 1)))))
(defparameter +marks-nodup+
  '((*auto-dyn-nodup* . (:pppppp :ppppp :pppp :ppp :pp :p :mp :mf :f :ff :fff :ffff :fffff :ffffff))
    (*auto-dyn-nodup* . (:pppppp* :ppppp* :pppp* :ppp* :pp* :p* :mp* :mf* :f* :ff* :fff* :ffff* :fffff* :ffffff*))))

(defparameter +marks-before-after+
  '((:glissando . :before) (:portamento . :before) (:breath . :after) (:tie . :after)))

;; marks that prevent notes from combining into chords if they differ
(defparameter +marks-indiv-voices+
  '(:starttext- :startwedge< :startwedge> :startwedge<* :startwedge>* 
    :pppppp :ppppp :pppp :ppp :pp :p :mp :mf :f :ff :fff :ffff :fffff :ffffff :fp :sf :sff :sp :spp :sfz :rfz
    :pppppp* :ppppp* :pppp* :ppp* :pp* :p* :mp* :mf* :f* :ff* :fff* :ffff* :fffff* :ffffff* :fp* :sf* :sff* :sp* :spp* :sfz* :rfz* 
    :text :textdyn :textnote
    :accent :marcato :tenuto :portato
    :upbow :downbow :thumb :leftheel :rightheel :lefttoe :righttoe 
    :trill :longtrill :startlongtrill- :prall :mordent 
    :pizz :arco :open :stopped :breath
    :notehead :harmonic :arpeggio :glissando :portamento :size))

;; spanners
;; startsym, contsym, endsym, replsym (nil = remove if on one note, t = can span one note, other symbols are replacements if on one note)
(defparameter +marks-spanner-voices+
  '((:startslur- :slur- :endslur- < s) ; < = endsyms cannot overlap with startsyms and must be shifted one event left, s = forced organization (bigger slurs over smaller slurs)
    (:startgraceslur- :graceslur- :endgraceslur- nil)
    (:starttext- :text- :endtext- :text)
    (:startwedge< :wedge< :endwedge< nil)
    (:startwedge> :wedge> :endwedge> nil)
    (:startwedge<* :wedge<* :endwedge<* nil)
    (:startwedge>* :wedge>* :endwedge>* nil)
    (:startlongtrill- :longtrill- :endlongtrill- t 1))) ; forced lvl 1 for all
(defparameter +marks-spanner-staves+
  '((:start8up- :8up- :end8up- :8up) ; put 1's here also?????
    (:start8down- :8down- :end8down- :8down)))

(defparameter +marks-expand+
  '(((:longtrill . nil) . (:startlongtrill- . :endlongtrill-))))

(defparameter +marks-defaultdir+ ; default placements (up or down in relation to staff)
  '((:longtrill . :up) (:startlongtrill- . :up) (:texttempo . :up) (:textnote . :up) (:starttext- . :down)
    (:startwedge< . :down) (:startwedge> . :down) (:textdyn . :down) (:text . :down)))
(defparameter +marks-long+ '(:startlongtrill-))
