;; -*-lisp-*-
;; Load file for FOMUS

(loop with fl = '("package" "version" "misc" "deps" "data" "classes" "util"
		  "splitrules"
		  ("accidentals" "beams" "marks" "other" "ottavas" "parts" "postproc" "split" "staves" "voices" "quantize")
		  (#-fomus-nocmn "backend_cmn" #-fomus-nolilypond "backend_ly" #-fomus-nomusicxml "backend_xml" #-fomus-nomidi "backend_mid")
		  "backends" "main" "interface" "final")
      and nw
      for na in fl
      for cl = (if (listp na) (mapcar (lambda (x) (merge-pathnames x *load-truename*)) na) (list (merge-pathnames na *load-truename*)))
      for cn = (mapcar (lambda (x) (compile-file-pathname x)) cl)
      do (loop with nw0
	       for cn0 in cn
	       and cl0 in cl
	       when (or nw
			(not (probe-file cn0))
			(>= (file-write-date cl0) (file-write-date cn0)))
	       do (compile-file cl0) (setf nw0 t)
	       finally (setf nw nw0)) 
      (map nil (lambda (x) (load x)) cn))