;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                           SOAL - Sonic Object Analysis Library
;;;                                      Version: 3.0.0
;;;
;;;                           Software conception by Didier Guigue
;;;                             Development by Hildegard Paulino
;;;                              Documentation by Didier Guigue
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; file updated -> 5/02/2010

;--------------------------------------------------
; Files to be loaded. The files are:
;
;   - aux.lisp: Auxiliary functions used by soal
;   - constants.lisp: Definition of constants used by soal
;   - piano-specifics.lisp: Piano specific algorithms (sonic qualities)
;   - space.lisp: Achronic analysis algorithms (spatial)
;   - time.lisp: Diachronic analysis algorithms (time)
;   - stats.lisp: Statistical and utilities algorithms
;   - soalreader.lisp: mf-info redefiniton (it suits soal needs)
;   - OM-Methods.lisp: patches' interface definition
;--------------------------------------------------

(defvar *SOAL-lib-files* '("auxiliary"
                           "constants"
                           "piano-specific"
                           "space"
                           "time"
                           "stats"
                           "soalReader"
                           "soalWriter"
                           "OM-Methods"))


;--------------------------------------------------
; Compiling and loading files
;--------------------------------------------------

(mapc #'(lambda (file) (compile&load (om-relative-path '("sources") file))) *SOAL-lib-files*)


;--------------------------------------------------
; SOAL subpackages initialization
; ("sub-pack-name" subpackage-lists class-list function-list class-alias-list)
;--------------------------------------------------

(defvar *soal-functions-list* '( ("Achronic Analysis" ( ("Spatial Analysis" nil nil (SPATIAL-ANALYSIS) nil)
                                                        ("Spatial Filling Analysis" nil nil (SPATIAL-DENSITY
                                                                                             SPATIAL-LINEARITY
                                                                                            HARMONICITY
                                                                                            ;harmonicity-2
                                                                                            COGNITIVE-SONANCE) nil)
                                                        ("Velocity Rate" nil nil (VELOCITY-RATE) nil)) nil nil nil)


                                 ("Diachronic Analysis" ( ("Span Analysis" nil nil (SMALLER-IMPULSE
                                                                                   FILE-DURATION
                                                                                   RELATIVE-SPAN) nil)
                                                          ("Analysis Per Onset" nil nil (NOTES-PER-ONSET
                                                                                   VELOCITY-PER-ONSET
                                                                                   DENSITY-PER-ONSET
                                                                                   HARMONICITY-PER-ONSET
                                                                                   COGNITIVE-SONANCE-PER-ONSET
                                                                                   SPATIAL-LINEARITY-PER-ONSET) nil)
                                                         ("Time Filling Analysis" nil nil (EVENTS-DENSITY
                                                                                          TIME-LINEARITY
                                                                                          PITCH-DIRECTION
                                                                                          AMPLITUDE-DEVIATION
                                                                                            PITCH-DEVIATIONS) nil)) nil nil nil)
                                 ("Piano Specific" nil nil ( SONIC-QUALITY-ANALYSIS
                                                               PIANO-SPATIAL-ANALYSIS) nil)

                                 ("Stats & Utils" (("Deviations" nil nil (ABSOLUTE-DEVIATIONS
                                                                          RELATIVE-DEVIATIONS
                                                                          DEVIATION-BETWEEN-TWO-LISTS
                                                                          LISTS-DEVIATION
                                                                          SOAL-RELATIVE-ENTROPY) nil)

                                                    ("Distribution Stats" nil nil (RANGE-FILLING-RATE
                                                                                     RANGES-FILLING-DENSITY
                                                                                     ACTIVE-RANGES
                                                                                     DENSITY-RATE
                                                                                     RATE
                                                                                     LINEAR-DISTRIBUTION-RATE) nil)

                                                     ("List Stats" nil nil (LIST-STATS
                                                                             RELATIVE-LIST-STATS
                                                                            LIST-INDEX
                                                                             LIST-WEIGHT
                                                                             HOW-MANY-ELEMENTS?
                                                                             KEEP-DUPLICATES
                                                                             FIND-ELEMENTS
                                                                             SOALSUBSTITUTE
                                                                             SOALINCLUDED?) nil)

                                                     ("Misc" nil nil (MC->PC-SOAL
                                                                      MC->N-SOAL) nil)) nil nil nil)

                                ("Soal reader" nil nil (SOAL-READER
                                                       multi-midi-reader
                                                       chordseq-reader
                                                       sdif-reader
                                                       )nil)
                                ("SOAL Segmentation" nil nil (SEGMENTATION-MIDI
                                                              SAVE-SOAL-SEGMENTATION
                                                              SOAL-MULTILISTS-FLAT) nil)
                                ("Partitional Analysis" nil nil (c
                                                index-T-Total-Relations
                                                index-i-Relations-of-Identity
                                                index-c-Relations-of-Contrast
                                                ) nil)))




;--------------------------------------------------
; filling packages
;--------------------------------------------------

;Aqui da erro!!
(om::fill-library *soal-functions-list*)
