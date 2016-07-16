;; -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**************************************************************************************************
;; FOMUS
;; deps.lisp
;;**************************************************************************************************

(in-package :fomus)
(compile-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMON MUSIC

(declaim (type boolean *use-cm* *cm-exists*)
	 (type (or function null) *cm-notefun* *cm-keynumfun* *cm-rhythmfun*))
(defparameter *use-cm* t)
(defparameter *cm-exists* nil)

(defparameter *cm-scale* nil)
(defparameter *cm-notefun* nil)
(defparameter *cm-keynumfun* nil)
(defparameter *cm-rhythmfun* nil)
(defparameter *cm-midi* nil)
(defparameter *cm-events* nil)
(defparameter *cm-rts* nil)
(defparameter *cm-midipbend* nil)

(defparameter *cm-midioff* nil)
(defparameter *cm-midioffslot* nil)
(defparameter *cm-mididur* nil)
(defparameter *cm-mididurslot* nil)
(defparameter *cm-midinote* nil)
(defparameter *cm-midinoteslot* nil)
(defparameter *cm-midichslot* nil)
(defparameter *cm-midivel* nil)
(defparameter *cm-midivelslot* nil)
(defparameter *cm-progch* nil)

(defun find-cm ()
  (when (and (not *cm-exists*) (find-package "CM"))
    (when (>= *verbose* 2) (format t ";; Common Music package detected~%"))
    (setf *cm-exists* t
	  *cm-notefun* (symbol-function (find-symbol "NOTE" :cm))
	  *cm-keynumfun* (symbol-function (find-symbol "KEYNUM" :cm))
	  *cm-rhythmfun* (symbol-function (find-symbol "RHYTHM" :cm))
	  *cm-midi* (find-symbol "MIDI" :cm)
	  *cm-progch* (find-symbol "MIDI-PROGRAM-CHANGE" :cm)
	  *cm-midioff* (symbol-function (find-symbol "OBJECT-TIME" :cm))
	  *cm-midioffslot* (find-symbol "TIME" :cm)
	  *cm-mididur* (symbol-function (find-symbol "MIDI-DURATION" :cm))
	  *cm-mididurslot* (find-symbol "DURATION" :cm)
	  *cm-midinote* (symbol-function (find-symbol "MIDI-KEYNUM" :cm))
	  *cm-midinoteslot* (find-symbol "KEYNUM" :cm)
	  *cm-midichslot* (find-symbol "CHANNEL" :cm)
	  *cm-midivel* (symbol-function (find-symbol "MIDI-AMPLITUDE" :cm))
	  *cm-midivelslot* (find-symbol "AMPLITUDE" :cm)
	  *cm-events* (symbol-function (find-symbol "EVENTS" :cm))
	  *cm-midipbend* (find-symbol "MIDI-PITCH-BEND" :cm)
	  *cm-rts* (ignore-errors (symbol-function (find-symbol "RTS" :cm)))
	  )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMON MUSIC NOTATION

(defparameter *cmn-exists* nil)

(defun find-cmn ()
  (when (and (not *cmn-exists*) (find-package "CMN"))
    (when (>= *verbose* 2) (format t ";; Common Music Notation package detected~%"))
    (setf *cmn-exists* t
	  )))