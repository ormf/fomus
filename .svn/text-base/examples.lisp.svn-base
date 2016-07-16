;; EXAMPLES
;; This is currently being used as a scratch file for testing and debugging

(in-package :fomus)

;; test the chunks

(let ((c1 (fomus
	   :output :chunk
	   :global (list (make-timesig :off 0 :time '(5 8)))
	   :auto-cautionary-accs t
	   :parts
	   (list
	    (make-part
	     :partid 'hp		; identical ids are matched
	     :name "Harpsichord"
	     :instr :harpsichord
	     :events
	     (loop
	      for off from 0 to 8 by 1/2
	      collect (make-note :off off
				 :dur (if (< off 8) 1/2 1)
				 :note (+ 48 (random 25))))))))
      (c2 (fomus
	   :output :chunk
	   :global (list (make-timesig :off 0 :time '(5 8)))
	   :auto-cautionary-accs t
	   :parts
	   (list
	    (make-part
	     :partid 'hp
	     :name "Harpsichord"
	     :instr :harpsichord
	     :events
	     (loop
	      for off from 10 to 16 by 1/2
	      collect (make-note :off off
				 :dur (if (< off 16) 1/2 1)
				 :note (+ 48 (random 25)))))))))
  (fomus (list c1 c2)
	 :output '(:lilypond :view t)))

;; stuff

(fomus
 :output '((:lilypond :view t))
 :filename "/tmp/test.xxx"
 :verbose 1
 :global (list (make-timesig :off 0 :time '(4 4) :props '((:keysig :dmaj))))
 :LONG-EIGHTH-BEAMS nil
 :auto-cautionary-accs t
 :instr-groups
 '((:quartet
    (:group
     (:group :violin :viola :cello))))
 :parts
 (list
  (make-part
   :name "Harpsichord"
   :instr '(:harpsichord :simultlim 1 :clefs (:treble :bass))
   :events
   (loop
    for off from 0 to 8 by 1/2
    collect (make-note :off off
		       :dur (if (< off 10) 1/2 1)
		       :note (+ 48 (random 25))
		       :marks '((:staff 2)))))))

(fomus
 (list (fomus
	:output '(:none) 
	:verbose 1
	:ensemble-type :orchestra
	:global (list (make-timesig :off 0 :time '(5 8) :div '(3/2 1)))
	:parts
	(list
	 (make-part
	  :name "Piano"
	  :instr '(:piano :simultlim 1)
	  :events
	  (loop
	   for off from 0 to 4 by 1/2
	   collect (make-note :off off
			      :dur (if (< off 10) 1/2 1)
			      :note (+ 48 (random 25)))))))
       (fomus
	:output '(:none) 
	:verbose 1
	:ensemble-type :orchestra
	:global (list (make-timesig :off 0 :time '(5 8) :div '(3/2 1)))
	:parts
	(list
	 (make-part
	  :name "Piano"
	  :instr '(:piano :simultlim 1)
	  :events
	  (loop
	   for off from 5 to 10 by 1/2
	   collect (make-note :off off
			      :dur (if (< off 10) 1/2 1)
			      :note (+ 48 (random 25))))))))
 :output '(:raw (:lilypond :view t)))

(fomus
 :output '(:lilypond :view t)
 :verbose 2
 :quality 2
 :ensemble-type :orchestra
 ;; :auto-accs-module :nokey2
 :parts
 (list
  (make-part
   :name "Violin 1"
   :partid 'v1
   :instr :violin
   :events
   (loop repeat 4
	 nconc (loop
		for off from 0 to 10 by 1/2
		collect (make-note :off off
				   :dur (if (< off 10) 1/2 1)
				   :note (+ 60 (random 25))
				   :voice '(1 2 3 4))))
   :props '((:distr (v2 2) (v3 3) (v4 4) #|(v5 5) (v6 6)|#)))
  (make-part :name "Violin 2" :instr :violin :partid 'v2)
  (make-part :name "Violin 3" :instr :violin :partid 'v3)
  (make-part :name "Violin 4" :instr :violin :partid 'v4)
  (make-part :name "Violin 5" :instr :violin :partid 'v5)
  (make-part :name "Violin 6" :instr :violin :partid 'v6)))

(fomus
 :backend '(:lilypond :view t)
 :ensemble-type :orchestra
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :events
   (loop
    for off from 0 to 10 by 3/2
    for no = (+ 48 (random 25))
    collect (make-note :off off
		       :dur (if (< off 10) 3/2 2)
		       :note no
		       :marks `((:longtrill ,(+ no 1))))))))

(fomus
 :backend '((:data) (:lilypond :view t))
 :ensemble-type :orchestra
 :default-beat 1/4
 :global (list (make-timesig :off 0 :time '(3 4))
	       (make-timesig :off 7 :time '(5 8)))
 :parts
 (list (make-part
	:name "Piano"
	:instr :piano
	:events
	(loop
	 for basenote in '(54 42)
	 nconc (loop for off = 0 then (+ off dur)
		     and dur = (/ (1+ (random 4)) 2)
		     while (< (+ off dur) 12)
		     collect (make-note :voice '(1 2)
					:off off
					:dur dur
					:note (+ basenote (random 25))))))))

(fomus
 :backend '((:data) (:lilypond :view t) #|(:midi :tempo 120 :delay 1)|# #|(:cmn :view t)|#)
 :ensemble-type :orchestra
 :beat-division 8
 :quartertones t
 :parts (list
	 (make-part
	  :partid 'flute
	  :name "Flute"
	  :instr :flute)
	 (make-part
	  :partid 'tuba
	  :name "Tuba"
	  :instr :tuba))
 :events (loop repeat 5
	       for off = (random 1.0) then (+ off (1+ (random 1.0)))
	       and dur = (random 1.0)
	       and inst = (if (eq inst 'flute) 'tuba 'flute)
	       collect (make-note :partid inst
				  :off off
				  :dur dur
				  :note (+ (case inst
					     (flute 72)
					     (tuba 36))
					   (/ (random 25) 2))
				  :marks (case (random 3)
					   (0 '(:accent))
					   (1 '(:staccato))))))

;; Nested Tuplets

(fomus
 :backend '(:data (:lilypond :view t))
 :ensemble-type :orchestra
 :beat-division 8
 :max-tuplet '(7 3)
 :parts (list
	 (make-part
	  :partid 'flute
	  :name "Flute"
	  :instr :flute))
 :events (loop repeat 5
	       for off = (random 1.0) then (+ off (1+ (random 1.0)))
	       and dur = (+ 0.5 (random 0.5))
	       and inst = 'flute
	       collect (make-note :partid inst
				  :off off
				  :dur dur
				  :note (+ (case inst
					     (flute 72))
					   (/ (random 25) 2))
				  :marks (case (random 3)
					   (0 '(:accent))
					   (1 '(:staccato))))))

;; Part ordering/grouping

(fomus
 :backend '(:lilypond :view t)
 :ensemble-type :orchestra
 :global (list (make-timesig :off 0 :time '(3 4)))
 :parts (list
	 (make-part
	  :name "Flute 1"
	  :instr :flute
	  :events (list (make-note :off 0 :dur 1 :note 60)))
	 (make-part
	  :partid 'fl2
	  :name "Flute 2"
	  :instr :flute
	  :events (list (make-note :off 0 :dur 1 :note 60)))
	 (make-part
	  :partid 'cl1
	  :name "Clarinet 1"
	  :instr :bf-clarinet
	  :events (list (make-note :off 0 :dur 1 :note 60)))
	 (make-part
	  :name "Clarinet 2"
	  :instr :bf-clarinet
	  :events (list (make-note :off 0 :dur 1 :note 60)))
	 (make-part
	  :name "Violin 1"
	  :instr :violin
	  :events (list (make-note :off 0 :dur 1 :note 60)))
	 (make-part
	  :name "Violin 2"
	  :instr :violin
	  :events (list (make-note :off 0 :dur 1 :note 60)))
	 (make-part
	  :name "Cello 1"
	  :instr :cello
	  :events (list (make-note :off 0 :dur 1 :note 48)))
	 (make-part
	  :name "Cello 2"
	  :instr :cello
	  :events (list (make-note :off 0 :dur 1 :note 48)))
	 (make-part
	  :name "Tuba"
	  :instr :tuba
	  :events (list (make-note :off 0 :dur 1 :note 36)))))

;; Mark objects

(fomus
 :backend '((:data) (:cmn :view t))
 :ensemble-type :orchestra
 :parts (list
	 (make-part
	  :partid :flute
	  :name "Flute"
	  :instr :flute
	  :events (loop for o from 0 to 20 by 1/2
			collect (make-note :off o :dur 1/2 :note 72)))
	 (make-part
	  :partid :tuba
	  :name "Tuba"
	  :instr :tuba
	  :events (loop for o from 0 to 20 by 1/2
			collect (make-note :off o :dur 1/2 :note 48))))
 :events (loop repeat 10
	       collect (make-mark :partid (case (random 2) (0 :flute) (1 :tuba))
				  :off (random 20.0)
				  :marks '(:accent))))

(fomus
 :backend '((:data) (:cmn :view t))
 :ensemble-type :orchestra
 :parts (list
	 (make-part
	  :partid :flute
	  :name "Flute"
	  :instr :flute
	  :events (loop for o from 0 to 20 by 1/2 collect (make-note :off o :dur 1/2 :note 72)))
	 (make-part
	  :partid :tuba
	  :name "Tuba"
	  :instr :tuba
	  :events (loop for o from 0 to 20 by 1/2 collect (make-note :off o :dur 1/2 :note 48))))
 :events (loop repeat 10 collect (make-mark :partid (case (random 2) (0 :flute) (1 :tuba)) :off (random 20.0) :marks '(:accent))))

;; Quantizing

(fomus
 :backend '((:data) (:lilypond :view t) (:musicxml-finale))
 :ensemble-type :orchestra
 :beat-division 2
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :events
   (loop
    for off = 0 then (+ off dur)
    and dur = (+ 1.0 (random 0.5))
    until (> off 15)
    collect (make-note :off off
		       :dur dur
		       :note (+ 48 (random 25))
		       :marks (when (<= (random 3) 0)
				'(:staccato)))))))

;; Grace notes

(fomus
 :backend '((:data) (:cmn :view t) (:midi :tempo 80 :delay 1))
 :ensemble-type :orchestra
 :auto-grace-slurs nil
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :events
   (loop
    for off from 0 to 4 by 1/2
    for note = (+ 48 (random 25))
    nconc (loop repeat (random 4) for grace from 100
		collect (make-note :off off
				   :dur (list 1/4 grace)
				   :note (if (= (random 2) 0) (- note (random 6)) (+ note (random 6))))) 
    collect (make-note :off off
		       :dur (if (< off 10) 1/2 1)
		       :note note
		       :marks (when (<= (random 3) 0)
				'(:staccato)))))))

;; Tremolos

(fomus
 :backend '((:data) :musicxml #|(:lilypond :view t)|# (:cmn :view t) #|(:midi :tempo 60 :delay 1)|#)
 :ensemble-type :orchestra
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :events
   (loop repeat 5
	 for off = (/ (random 60) 2)
	 and dur = (1+ (/ (random 6) 2))
	 collect (make-note :off off :dur dur :note (+ 60 (random 25)) :marks '(:tremolofirst))
	 collect (make-note :off off :dur dur :note (+ 60 (random 25)))))))

(fomus
 :backend '((:data) (:lilypond :view t) #|(:cmn :view t)|# #|(:midi :tempo 60 :delay 1)|#)
 :ensemble-type :orchestra
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :events
   (list (make-note :off 0 :dur (+ 1 1/2 1/4) :note (+ 60 (random 25)) :marks '(:tremolofirst))
	 (make-note :off 0 :dur (+ 1 1/2 1/4) :note (+ 60 (random 25)))))))

(progn
  (fomus-init :output '(:lilypond :view t))
  (fomus-newpart 1 :instr :piano)
  (fomus-newnote 1 :note 'g4 :off 0 :dur 1/2 :marks '(:startslur-))
  (fomus-newnote 1 :note 'g4 :off 1/2 :dur 1/2 :marks '(:endslur-))
  (fomus-newnote 1 :note 'f4 :off 1 :dur '(1/2 0))
  (fomus-newnote 1 :note 'g4 :off 1 :dur 1) ; :marks '(:endslur-))
  (fomus-newnote 1 :note 'g4 :off 2 :dur 1) ; :marks '(:endslur-))
  (fomus-exec))

(progn
  (fomus-init :output '(:lilypond :view t))
  (fomus-newpart 1 :instr :piano)
  (fomus-newtimesig :off 0 :time '(4 4) :props '((:keysig :bfmaj)))
  (fomus-newnote 1 :note 'g4 :off 0 :dur 1/2 :marks '((:textdyn "sfz")))
  (fomus-exec))

(progn
  (fomus-init :output '(:lilypond :view t))
  (fomus-newpart 1 :instr :piano)
  (fomus-newnote 1 :note 'g4 :off 0 :dur 3/2)
  (fomus-newnote 1 :note 'g4 :off 3/2 :dur 1/2 :marks '((:tie :before)))
  (fomus-exec))

(progn
  (fomus-init :output '(:lilypond :view t))
  (fomus-newpart 1 :instr '(:oboe :clefs :treble-8up))
  (fomus-newtimesig :off 0 :time '(5 4))
  (fomus-newnote 1 :note 'g4 :off 0 :dur 1/2)
  (fomus-newnote 1 :note 'g4 :off 1/2 :dur 1/2)
  (fomus-newnote 1 :note 'g4 :off 1 :dur 1/2)
  (fomus-newnote 1 :note 'g4 :off 3/2 :dur 1/2)
  (fomus-newnote 1 :note 'g4 :off 2 :dur 1/2)
  (fomus-newnote 1 :note 'g4 :off 5/2 :dur 1/2)
  (fomus-newnote 1 :note 'g4 :off 3 :dur 1/2)
  (fomus-newnote 1 :note 'g4 :off 7/2 :dur 1/2)
  (fomus-newnote 1 :note 'g4 :off 4 :dur 1/2)
  (fomus-newnote 1 :note 'g4 :off 9/2 :dur 1/2)
  (fomus-exec))

(fomus
 :backend '((:data) :musicxml (:lilypond :view t :version "2.10"))
 :ensemble-type :orchestra
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :events
   (loop repeat 5
	 for off = (/ (random 60) 2)
	 and dur = (1+ (/ (random 6) 2))
	 collect (make-note :off off :dur dur :note (+ 60 (random 25)) :marks '((:tremolo :notated 1/8)))))))

;; Trills w/ accidentals

(fomus
 :backend '((:data) (:lilypond :view t) (:midi :tempo 80 :delay 1 :play t))
 :ensemble-type :orchestra
 :auto-multivoice-notes nil
 :parts
 (list
  (make-part
   :name "Piano"
   :instr '(:piano :staves 1)
   :events
   (loop for vo from 1 to 2
	 nconc (loop
		for off from 0 to 10 by 1/2
		for note = (+ (- 72 (* vo 24)) (random 25))
		collect (make-note :off off
				   :dur (if (< off 10) 1/2 1)
				   :note note
				   :voice vo
				   :marks (when (<= (random 3) 0)
					    (if (= (random 2) 0)
						(list (list :mordent (- note 2)))
						(list (list :trill (+ note 2)))))))))))

;; Chords

(fomus
 :backend '(:lilypond :view t)
 :ensemble-type :orchestra
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :events
   (loop repeat 10
	 for off = (random 30.0)
	 and dur = (1+ (random 3.0))
	 collect (make-note :off off :dur dur :note (+ 60 (random 25)))))))

(fomus
 :backend '((:data) (:lilypond :view t))
 :ensemble-type :orchestra
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :events
   (loop
    for off from 0 to 10 by 1/2
    nconc (loop for note from 36 to 72 by 12
		collect (make-note :off off
				   :dur (if (< off 10) 1/2 1)
				   :note (+ note (random 13))
				   :marks (when (<= (random 3) 0)
					    '(:staccato))))))))

;; Arpeggios

(fomus
 :backend '((:data) (:lilypond :view t) (:midi :tempo 60 :delay 1))
 :ensemble-type :orchestra
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :events
   (loop
    for off from 0 to 10 by 1
    collect (make-note :off off
		       :dur 1
		       :note (+ 60 (random 13))
		       :marks (list
			       (ecase (random 3)
				 (0 :arpeggio)
				 (1 '(:arpeggio :up))
				 (2 '(:arpeggio :down)))))
    collect (make-note :off off
		       :dur 1
		       :note (+ 66 (random 13)))
    collect (make-note :off off
		       :dur 1
		       :note (+ 72 (random 13)))))))

;; Harmonics

(fomus
 :backend '((:data) (:lilypond :view t) (:midi :tempo 80 :delay 1))
 :ensemble-type :orchestra
 :parts
 (list
  (make-part
   :name "Cello"
   :instr :cello
   :events
   (loop
    for off from 0 to 10 by 1/2
    for note = (+ 36 (random 25))
    collect (make-note :off off
		       :dur (if (< off 10) 1/2 1)
		       :note note
		       :marks (list (list :harmonic :touched (+ note 5))))))))

(fomus
 :backend '((:data) (:lilypond :view t))
 :ensemble-type :orchestra
 :parts
 (list
  (make-part
   :name "Cello"
   :instr :cello
   :events
   (loop
    for off from 0 to 10 by 1/2
    collect (make-note :off off
		       :dur (if (< off 10) 1/2 1)
		       :note 36
		       :marks (list (list :harmonic :sounding 60)))))))

;; Note Heads

(fomus
 :backend '((:data) (:lilypond :view t))
 :ensemble-type :orchestra
 :parts
 (list
  (make-part
   :name "Violin"
   :instr :violin
   :events
   (loop
    for off from 0 to 10 by 1/2
    for note = (+ 55 (random 25))
    collect (make-note :off off
		       :dur (if (< off 10) 1/2 1)
		       :note note
		       :marks '((:notehead :x)))))))

;; Percussion

(fomus
 :backend '(:lilypond :view t)
 :ensemble-type :orchestra
 :parts (list
	 (make-part
	  :name "Percussion"
	  :instr (list :percussion :percs (list (make-perc :woodblock :note 'e4)
						(make-perc :snaredrum :note 'a3)))
	  :events (loop for o from 0 to 20 by 1/2 collect
			(make-note :off o :dur 1/2
				   :note (case (random 2)
					   (0 :woodblock)
					   (1 :snaredrum)))))))

(fomus
 :backend '(:lilypond :view t)
 :ensemble-type :orchestra
 :parts (list
	 (make-part
	  :name "Percussion"
	  :instr (list :percussion :percs (list (make-perc :woodblock :voice 1 :note 'e4)
						(make-perc :snaredrum :voice 2 :note 'a3)))
	  :events (loop for o from 0 to 20 by 1/2 collect
			(make-note :off o :dur 1/2
				   :note (case (random 2)
					   (0 :woodblock)
					   (1 :snaredrum)))))))

;; Text

(fomus
 :backend '((:data) (:lilypond :view t))
 :ensemble-type :orchestra
 :parts (list
	 (make-part
	  :name "Piano"
	  :instr :piano
	  :events (list (make-note :off 0 :dur 1 :note 72 :marks '((:text "Text" :nopos)))
			(make-note :off 1 :dur 1 :note 72 :marks '((:textnote "nt" :nopos)))
			(make-note :off 2 :dur 1 :note 72 :marks '((:textdyn "dyn" :nopos)))
			(make-note :off 3 :dur 1 :note 72 :marks '((:texttempo "Tempo")))
			
			(make-note :off 4 :dur 1 :note 72 :marks '((:text :up "Text")))
			(make-note :off 5 :dur 1 :note 72 :marks '((:textnote "nt" :up)))
			(make-note :off 6 :dur 1 :note 72 :marks '((:textdyn :up "dyn")))
			(make-note :off 7 :dur 1 :note 72 :marks '((:texttempo "Tempo":up)))

			(make-note :off 8 :dur 1 :note 72 :marks '((:text :down "Text")))
			(make-note :off 9 :dur 1 :note 72 :marks '((:textnote "nt" :down)))
			(make-note :off 10 :dur 1 :note 72 :marks '((:textdyn :down "dyn")))
			(make-note :off 11 :dur 1 :note 72 :marks '((:texttempo "Tempo":down)))

			(make-note :off 16 :dur 1 :note 72 :marks '((:starttext- "Textspan")))
			(make-note :off 17 :dur 1 :note 72)
			(make-note :off 18 :dur 1 :note 72)
			(make-note :off 19 :dur 1 :note 72 :marks '(:endtext-))
			
			(make-note :off 20 :dur 1 :note 72 :marks '((:textdyn :down "f")))
			(make-note :off 21 :dur 1 :note 72 :marks '(:f))
			(make-note :off 22 :dur 1 :note 72)
			(make-note :off 23 :dur 1 :note 72)))))

;; Glissandi

(fomus
 :backend '(:lilypond :view t)
 :ensemble-type :orchestra
 :parts (list
	 (make-part
	  :name "Piano"
	  :instr :piano
	  :events (list (make-note :off 0 :dur 1 :note 60)
			(make-note :off 1 :dur 1 :note 66 :marks '((:glissando :before)))
			(make-note :off 2 :dur 1 :note 72 :marks '((:glissando :before)))
			(make-note :off 3 :dur 1 :note 78 :marks '((:glissando :before)))))))

;; Breath Marks

(fomus
 :backend '((:data) (:lilypond :view t))
 :ensemble-type :orchestra
 :parts (list
	 (make-part
	  :name "Piano"
	  :instr :piano
	  :events (list (make-note :off 0 :dur 1 :note 60)
			(make-note :off 1 :dur 1 :note 60 :marks '(:breath))
			(make-note :off 2 :dur 1 :note 60) 

			(make-note :off 4 :dur 1 :note 60)
			(make-note :off 5 :dur 1 :note 60 :marks '((:breath :before)))
			(make-note :off 6 :dur 1 :note 60)

			(make-note :off 8 :dur 1 :note 60)
			(make-note :off 9 :dur 1 :note 60 :marks '((:breath :after)))
			(make-note :off 10 :dur 1 :note 60)))))

;; Speed Test

(fomus
 :backend '((:data) (:lilypond :view t ))
 :ensemble-type :orchestra
 :verbose 2
 :quality 1/2
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :events
   (loop
    for off from 0 to 500 by 1/2
    collect (make-note :off off
		       :dur (if (< off 500) 1/2 1)
		       :note (+ 48 (random 25))
		       :marks (when (<= (random 3) 0)
				'(:staccato)))))))

;; Lilypond Options

(fomus
 :backend '((:data) (:lilypond :view t :filehead ("#(set-default-paper-size \"tabloid\")") :scorehead ("%% SCOREHEAD")))
 :ensemble-type :orchestra
 :quality 1/2
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :opts '(:lily-parthead ("%% PARTHEAD1" "%% PARTHEAD2") :lily-partname "special")
   :events
   (loop
    for off from 0 to 50 by 1/2
    collect (make-note :off off
		       :dur (if (< off 50) 1/2 1)
		       :note (+ 48 (random 25))
		       :marks (when (<= (random 3) 0)
				'(:staccato)))))))

;; MusicXML (not working yet)

(fomus
 :backend '((:data) (:musicxml))
 :ensemble-type :orchestra
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :events
   (loop
    for off from 0 to 10 by 1/2
    collect (make-note :off off
		       :dur (if (< off 10) 1/2 1)
		       :note (+ 48 (random 25))
		       :marks (when (<= (random 3) 0)
				'(:staccato)))))))

;; Percussion Autodurations

(fomus
 :backend '(:lilypond :view t)
 :ensemble-type :orchestra
 :parts (list
	 (make-part
	  :name "Snare Drum"
	  :instr '(:percussion :percs ((:snare-drum :note a3)))
	  :events (loop for o from 0 to 40 by 1/2 when (= (random 2) 0) collect
			(make-note :off o
				   :note :snare-drum)))))

;; User Rests

(fomus
 :backend '((:data) (:lilypond :view t))
 :verbose 2
 :ensemble-type :orchestra
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :events
   (cons (make-rest :off 19/2 :dur 2 :marks '(:fermata (:text "Here!")))
	 (loop
	  for off from 0 below 19/2 by 1/2
	  collect (make-note :off off
			     :dur 1/2
			     :note (+ 48 (random 25))
			     :marks (when (<= (random 3) 0)
				      '(:staccato))))))))

;; Auto Pizz/Arco

(fomus
 :backend '((:data) (:lilypond :view t) (:midi :tempo 60 :delay 1 :play t))
 :ensemble-type :orchestra
 :beat-division 8
 ;;:quartertones t
 :parts (list
	 (make-part
	  :name "Violin"
	  :instr :violin))
 :events (loop repeat 5
	       for off = (random 1.0) then (+ off (1+ (random 1.0)))
	       and dur = (random 1.0)
	       collect (make-note :off off
				  :dur dur
				  :note (+ 55 (/ (random 25) 2))
				  :marks (case (random 2)
					   (0 '(:pizz))))))

;; Auto On/Offs

(fomus					; :auto-accidentals
 :backend '((:data) (:lilypond :view t))
 :ensemble-type :orchestra
 :auto-accidentals nil
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :events
   (loop
    for off from 0 to 10 by 1/2
    and note = (+ 48 (random 25))
    collect (make-note :off off
		       :dur (if (< off 10) 1/2 1)
		       :note (list note (svref #(0 -1 0 -1 0 0 1 0 -1 0 -1 0) (mod note 12))))))))

(fomus 
 :backend '((:data) (:lilypond :view t))
 :ensemble-type :orchestra
 :auto-accidentals nil
 :quartertones t
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :events
   (loop
    for off from 0 to 10 by 1/2
    collect (make-note :off off
		       :dur (if (< off 10) 1/2 1)
		       :note '(60.5 (-1 -0.5)))))))

(fomus					; :auto-cautionary-accs
 :backend '((:data) (:lilypond :view t))
 :ensemble-type :orchestra
 :auto-accidentals nil
 :auto-cautionary-accs t
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :events
   (loop
    for off from 0 to 10 by 1/2
    and note = (+ 48 (random 25))
    collect (make-note :off off
		       :dur (if (< off 10) 1/2 1)
		       :note (list note (svref #(0 -1 0 -1 0 0 1 0 -1 0 -1 0) (mod note 12))))))))

(fomus					; :auto-ottavas
 :backend '((:data) (:lilypond :view t))
 :ensemble-type :orchestra
 :auto-ottavas t
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :events
   (loop
    for off from 0 to 20 by 1/2
    and note = (+ 72 (random 37))
    collect (make-note :off off
		       :dur (if (< off 20) 1/2 1)
		       :note note)))))

(fomus					; :auto-voicing
 :backend '((:data) (:lilypond :view t))
 :ensemble-type :orchestra
 :auto-voicing nil
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :events
   (loop
    for off from 0 to 10 by 1/2
    collect (make-note :off off
		       :voice '(1)	; (1+ (random 2))
		       :dur (if (< off 10) 1/2 1)
		       :note (+ 48 (random 25)))))))

(fomus					; :auto-grace-slurs 
 :backend '((:data) (:lilypond :view t))
 :ensemble-type :orchestra
 :auto-grace-slurs nil
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :events
   (loop
    for off from 0 to 4 by 1/2
    for note = (+ 48 (random 25))
    nconc (loop repeat (random 4) for grace from -100
		collect (make-note :off off
				   :dur (list 1/4 grace)
				   :note (if (= (random 2) 0) (- note (random 6)) (+ note (random 6))))) 
    collect (make-note :off off
		       :dur (if (< off 10) 1/2 1)
		       :note note
		       :marks (when (<= (random 3) 0)
				'(:staccato)))))))

(fomus					; :auto-beams
 :backend '((:data) (:lilypond :view t))
 :ensemble-type :orchestra
 :auto-beams nil
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :events
   (loop
    for off from 0 to 4 by 1/2
    for note = (+ 48 (random 25))
    nconc (loop repeat (random 4) for grace from -100
		collect (make-note :off off
				   :dur (list 1/4 grace)
				   :note (if (= (random 2) 0) (- note (random 6)) (+ note (random 6))))) 
    collect (make-note :off off
		       :dur (if (< off 10) 1/2 1)
		       :note note
		       :marks (when (<= (random 3) 0)
				'(:staccato)))))))

(fomus					; :auto-quantize
 :backend '((:data) (:lilypond :view t))
 :ensemble-type :orchestra
 :auto-quantize nil
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :events
   (loop
    for off from 0 to 4 by 1/2
    for note = (+ 48 (random 25))
    nconc (loop repeat (random 4) for grace from -100
		collect (make-note :off off
				   :dur (list 1/4 grace)
				   :note (if (= (random 2) 0) (- note (random 6)) (+ note (random 6))))) 
    collect (make-note :off off
		       :dur (if (< off 10) 1/2 1)
		       :note note
		       :marks (when (<= (random 3) 0)
				'(:staccato)))))))

(fomus					; :auto-staff/clef-changes
 :backend '((:data) (:lilypond :view t ))
 :ensemble-type :orchestra
 :quality 1/2
 :auto-staff/clef-changes nil
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :events
   (loop
    for off from 0 to 100 by 1/2
    collect (make-note :off off
		       :dur (if (< off 100) 1/2 1)
		       :note (+ 48 (random 25)))))))

(fomus					; :auto-multivoice-rests
 :backend '((:data) (:lilypond :view t))
 :ensemble-type :orchestra
 :auto-multivoice-rests nil
 :parts (list
	 (make-part
	  :name "Percussion"
	  :instr (list :percussion :percs (list (make-perc :woodblock :voice 1 :note 'e4)
						(make-perc :snaredrum :voice 2 :note 'a3)))
	  :events (loop for o from 0 to 50 by 1/2 when (= (random 4) 0) collect
			(make-note :off o :dur 1/2
				   :note (case (random 2)
					   (0 :woodblock)
					   (1 :snaredrum)))))))

(fomus					; :auto-multivoice-notes
 :backend '(:lilypond :view t)
 :ensemble-type :orchestra
 :auto-multivoice-notes nil
 :parts
 (list
  (make-part
   :name "Violin"
   :instr :violin 
   :events
   (loop for b in '(55 67) nconc
	 (loop
	  for off from 0 to 10 by 1/2
	  collect (make-note :off off
			     :voice '(1 2)
			     :dur (if (< off 10) 1/2 1)
			     :note (+ b (random 19))))))))

(fomus					; :auto-percussion-durs
 :backend '((:data) (:lilypond :view t))
 :ensemble-type :orchestra
 :auto-percussion-durs t
 :parts (list
	 (make-part
	  :name "Percussion"
	  :instr (list :percussion :percs (list (make-perc :woodblock :note 'e4 :autodur t)
						(make-perc :snaredrum :note 'a3 :autodur t)))
	  :events (loop for o from 0 to 40 by 1/2 when (= (random 2) 0) collect
			(make-note :off o
				   :note (case (random 2)
					   (0 :woodblock)
					   (1 :snaredrum)))))))

(fomus					; :auto-pizz/arco
 :backend '((:data) (:lilypond :view t))
 :ensemble-type :orchestra
 :beat-division 8
 :quartertones t
 :auto-pizz/arco nil
 :parts (list
	 (make-part
	  :name "Violin"
	  :instr :violin))
 :events (loop repeat 5
	       for off = (random 1.0) then (+ off (1+ (random 1.0)))
	       and dur = (random 1.0)
	       collect (make-note :off off
				  :dur dur
				  :note (+ 55 (/ (random 25) 2))
				  :marks (case (random 2)
					   (0 '(:pizz))
					   (1 '(:arco))))))

(fomus					; :auto-override-timesigs
 :backend '((:data) (:lilypond :view t ))
 :ensemble-type :orchestra
 :verbose 2
 :quality 1/2
 :auto-override-timesigs nil
 :global
 (list (make-timesig :off 0 :time '(4 4)) (make-timesig :off 10 :time '(4 4)) (make-timesig :off 11 :time '(4 4)))
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :events
   (loop
    for off from 0 to 20 by 1/2
    collect (make-note :off off
		       :dur (if (< off 20) 1/2 1)
		       :note (+ 48 (random 25))
		       :marks (when (<= (random 3) 0)
				'(:staccato)))))))

;; MIDI output

(fomus
 :backend '((:data) (:lilypond :view t ) (:midi :tempo 120 :play t))
 :ensemble-type :orchestra
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :events
   (loop
    for off from 0 to 10 by 1/2
    collect (make-note :off off
		       :dur (if (< off 10) 1/2 1)
		       :note (+ 48 (random 25))
		       :marks (when (<= (random 3) 0)
				'(:staccato)))))))

;; User Overrides
;; Grace note rests
;; Mark Spanners
;; Compound meter

(fomus
 :backend '((:raw) (:lilypond :view t) #|(:midi :tempo 120 :delay 1)|#)
 :ensemble-type :orchestra
 :verbose 2
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :events
   (loop
    for off from 0 to 10 by 1/2
    collect (make-note :off off
		       :dur (if (< off 10) 1/2 1)
		       :note (+ 48 (random 25))
		       :marks (if (<= (random 3) 0)
				'(:startslur-) '(:slur-)))))))

;; CM

(defparameter *part* (new fms:part :instr '(:piano :simultlim 1) :partid 'pno))

(defun polygen (voice len minp maxp)
  (process repeat len
	   output (new fms:note
		    :off (now)
		    :voice voice
		    :partid 'pno
		    :note (between minp maxp)
		    :dur 1/2)
	   wait 1/2))

(events (list (polygen 1 20 60 80) (polygen 2 20 40 60)) "/tmp/fomus.ly" :parts *part* :view t)

;; fomus decides voice
(events (list (polygen '(1 2 3) 20 50 70) (polygen '(1 2 3) 20 50 70) (polygen '(1 2 3) 20 50 70))
	"/tmp/test.ly" :parts *part* :view t)

;; 3/4 voices... (does fairly well with the exception of a few large leaps here and there)
(events (list (polygen '(1 2 3) 20 30 80) (polygen '(1 2 3) 20 30 80) (polygen '(1 2 3) 20 30 80)) "/tmp/test.ly" :parts *part* :view t)
(events (list (polygen '(1 2 3 4) 20 30 80) (polygen '(1 2 3 4) 20 30 80) (polygen '(1 2 3 4) 20 30 80)) "/tmp/test.ly" :parts *part* :view t)

(fomus
 :output '(:lilypond :view t)
 :quartertones t
 :parts
 (list
  (make-part
   :name "Flute"
   :instr :flute
   :events
   (loop
    for off from 0 to 10 by 1/2
    collect (make-note :off off
		       :dur (if (< off 10) 1/2 1)
		       :note (+ 70 (/ (random 4) 2)))))))

;; FROM WEB PAGE

;; Example 9.1. Simple Example

(fomus
 :output '(:lilypond :view t)
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :events
   (loop
    for off from 0 to 10 by 1/2
    collect (make-note :off off
		       :dur (if (< off 10) 1/2 1)
		       :note (+ 48 (random 25)))))))
	
;; Example 9.2. Staccato and Accent Marks

(fomus
 :output '(:lilypond :view t)
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :events
   (loop
    for off from 0 to 10 by 1/2
    collect (make-note :off off
		       :dur (if (< off 10) 1/2 1)
		       :note (+ 48 (random 25))
		       :marks (case (random 3)
				(0 nil)
				(1 '(:staccato))
				(2 '(:accent))))))))
	
;; Example 9.3. Quartertones

(fomus
 :output '(:lilypond :view t)
 :quartertones t
 :parts
 (list
  (make-part
   :name "Flute"
   :instr :flute
   :events
   (loop
    for off from 0 to 10 by 1/2
    collect (make-note :off off
		       :dur (if (< off 10) 1/2 1)
		       :note (+ 70 (/ (random 4) 2)))))))
	
;; Example 9.4. Polyphony with Slurs

(fomus
 :output '(:lilypond :view t)
 :verbose 1
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :events
   (loop for v from 1 to 2
	 nconc (loop
		for off from 0 to 10 by 1/2
		collect (make-note :off off
				   :dur (if (< off 10) 1/2 1)
				   :note (+ 60 (random 25))
				   :voice v
				   :marks (when (= (random 3) 0)
					    '(:startslur-))))))))

;; Example 9.5. Piano Chords

(fomus
 :output '(:lilypond :view t)
 :verbose 1
 :ensemble-type :orchestra
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :events
   (loop repeat 3
	 nconc (loop
		for off from 0 to 10 by 1/2
		collect (make-note :off off
				   :dur (if (< off 10) 1/2 1)
				   :note (+ 48 (random 25))
				   :voice '(1 2)))))))

;; Example 9.6. Quantizing/Chords

(fomus
 :output '(:lilypond :view t)
 :parts
 (list
  (make-part
   :name "Piano"
   :instr :piano
   :events
   (loop repeat 10
	 for off = (random 30.0)
	 and dur = (1+ (random 3.0))
	 collect (make-note :off off :dur dur :note (+ 60 (random 25)))))))
	
;; Example 9.7. Mark Objects

(fomus
 :output '(:lilypond :view t)
 :parts (list
	 (make-part
	  :partid :flute
	  :name "Flute"
	  :instr :flute
	  :events (loop for o from 0 to 20 by 1/2
			collect (make-note :off o :dur 1/2 :note 72)))
	 (make-part
	  :partid :tuba
	  :name "Tuba"
	  :instr :tuba
	  :events (loop for o from 0 to 20 by 1/2
			collect (make-note :off o :dur 1/2 :note 48))))
 :events (loop repeat 10
	       collect (make-mark :partid (case (random 2) (0 :flute) (1 :tuba))
				  :off (random 20.0)
				  :marks '(:accent))))

;; Example 9.8. Percussion 1

(fomus
 :output '(:lilypond :view t)
 :parts (list
	 (make-part
	  :name "Percussion"
	  :instr (list :percussion :percs (list (make-perc :woodblock :note 'e4)
						(make-perc :snaredrum :note 'a3)))
	  :events (loop for o from 0 to 20 by 1/2 collect
			(make-note :off o :dur 1/2
				   :note (case (random 2)
					   (0 :woodblock)
					   (1 :snaredrum)))))))

;; Example 9.9. Percussion 2

(fomus
 :output '(:lilypond :view t)
 :parts (list
	 (make-part
	  :name "Percussion"
	  :instr (list :percussion :percs (list (make-perc :woodblock :voice 1 :note 'e4)
						(make-perc :snaredrum :voice 2 :note 'a3)))
	  :events (loop for o from 0 to 20 by 1/2 collect
			(make-note :off o :dur 1/2
				   :note (case (random 2)
					   (0 :woodblock)
					   (1 :snaredrum)))))))

;; Example 9.10. Percussion with Automatic Durations

(fomus
 :output '(:lilypond :view t)
 :parts (list
	 (make-part
	  :name "Snare Drum"
	  :instr '(:percussion :percs ((:snare-drum :note a3)))
	  :events (loop for o from 0 to 40 by 1/2 when (= (random 2) 0) collect
			(make-note :off o
				   :note :snare-drum)))))
	
;; Example 9.11. Semi-Orchestra Score

(fomus
 :output '(:lilypond :view t)
 :ensemble-type :orchestra
 :global (list (make-timesig :off 0 :time '(3 4)))
 :parts (list
	 (make-part
	  :name "Flute 1"
	  :instr :flute
	  :events (list (make-note :off 0 :dur 1 :note 60)))
	 (make-part
	  :partid 'fl2
	  :name "Flute 2"
	  :instr :flute
	  :events (list (make-note :off 0 :dur 1 :note 60)))
	 (make-part
	  :partid 'cl1
	  :name "Clarinet 1"
	  :instr :bf-clarinet
	  :events (list (make-note :off 0 :dur 1 :note 60)))
	 (make-part
	  :name "Clarinet 2"
	  :instr :bf-clarinet
	  :events (list (make-note :off 0 :dur 1 :note 60)))
	 (make-part
	  :name "Violin 1"
	  :instr :violin
	  :events (list (make-note :off 0 :dur 1 :note 60)))
	 (make-part
	  :name "Violin 2"
	  :instr :violin
	  :events (list (make-note :off 0 :dur 1 :note 60)))
	 (make-part
	  :name "Cello 1"
	  :instr :cello
	  :events (list (make-note :off 0 :dur 1 :note 48)))
	 (make-part
	  :name "Cello 2"
	  :instr :cello
	  :events (list (make-note :off 0 :dur 1 :note 48)))
	 (make-part
	  :name "Tuba"
	  :instr :tuba
	  :events (list (make-note :off 0 :dur 1 :note 36)))))
	
;; Example 9.12. Key Signatures

(fomus
 :output '(:lilypond :view t)
 :verbose 1
 :global (list (make-timesig :off 0 :time '(5 8) :div '(3/2 1) :props '((:keysig :dmaj))))
 :auto-cautionary-accs t
 :parts
 (list
  (make-part
   :name "Piano"
   :instr '(:piano :simultlim 1)
   :events
   (loop
    for off from 0 to 8 by 1/2
    collect (make-note :off off
		       :dur (if (< off 10) 1/2 1)
		       :note (+ 48 (random 25)))))))
	
