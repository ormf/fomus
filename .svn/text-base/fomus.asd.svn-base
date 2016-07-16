;; -*-lisp-*-
;; ASDF System for FOMUS

(asdf:defsystem "fomus"
  
  :description "Lisp music notation formatter"
  :version "0.2.28"
  :author "David Psenicka"
  :licence "LLGPL"

  :components
  ((:file "package")
   (:file "version" :depends-on ("package"))
   (:file "misc" :depends-on ("package"))
   (:file "deps" :depends-on ("package"))
   (:file "data" :depends-on ("misc" "deps"))
   (:file "classes" :depends-on ("data"))
   (:file "util" :depends-on ("classes"))

   (:file "splitrules" :depends-on ("util"))
   
   (:file "accidentals" :depends-on ("util"))
   (:file "beams" :depends-on ("util"))
   (:file "marks" :depends-on ("util"))
   (:file "other" :depends-on ("util"))
   (:file "ottavas" :depends-on ("util"))
   (:file "parts" :depends-on ("util"))
   (:file "postproc" :depends-on ("util"))
   (:file "split" :depends-on ("util" "splitrules"))
   (:file "staves" :depends-on ("util"))
   (:file "voices" :depends-on ("util"))
   (:file "quantize" :depends-on ("util" "splitrules"))

   #-fomus-nocmn (:file "backend_cmn" :depends-on ("util"))
   #-fomus-nolilypond (:file "backend_ly" :depends-on ("util"))
   #-fomus-nomusicxml (:file "backend_xml" :depends-on ("util"))
   #-fomus-nomidi (:file "backend_mid" :depends-on ("util"))
   (:file "backends" :depends-on (#-fomus-nocmn "backend_cmn" #-fomus-nolilypond "backend_ly" #-fomus-nomusicxml "backend_xml" #-fomus-nomidi "backend_mid" "version"))
   
   (:file "main" :depends-on ("accidentals" "beams" "marks" "other" "ottavas" "parts" "postproc" "split" "staves" "voices" "quantize" "backends"))

   (:file "interface" :depends-on ("main"))

   (:file "final" :depends-on ("version" "interface") :in-order-to ((load-op (load-op "interface"))))
   ))