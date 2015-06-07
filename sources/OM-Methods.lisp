 (in-package :om)

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; DIACHRONIC ANALYSIS
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;----------------------------
;inicializacao dos valores default, que sao diferenciados dos que tem mesmo nome, com o d na frente

(setq dnotes '((6000 6400 6700)))
(setq donsets '((0 1000 2000)))
(setq ddurations '((500 500 500)))
(setq dvelocities '((69 69 69)))
(setq dCC4s '((64)))
(setq dquantfcs '(5))
(setq dnotes-per-onset '(((0 (6000)) (1000 (6400)) (2000 (6700)))))
(setq dnumber-notes-onset '(((0 1) (1000 1) (2000 1))))

;----------------------------
;----------------------------
;Time Filling Analysis
;----------------------------
;*****************************************************************************************
;**********************  Versão recursiva ************************************************
;*****************************************************************************************
(defmethod! notes-per-onset ((notes list) (onsets list))
  :doc
"Returns the number of notes per onset

Outputs:
[1] a list of the notes (midicents) on each onset (of each input file);
[2] the number of notes  per onset;
[3] the lowest note per onset (smallest midicents value) (for each file);
[4] the highest note per onset (greatest midicents value) (for each file);
[5] the notesÕ average value (in mcs) per onset (for each file);
[6] the notesÕ mode value (in mcs) per onset, i.e. the most frequent note on each onset. If not existing, the algorithm
returns back the average note (as in the 5th ouput)."

  :initvals (list dnotes donsets)
  :indoc '("list of notes (midicents)"
           "list of onsets (ms)")
  :icon 131
  :numouts 6

  (let* ((f-list-min #'list-min)
         (f-length #'length)
         (f-list-max #'list-max)
         (f-media #' media)
         (f-vmode #'vmode-sem-texto))

    (values (primeiro-par-notes-by-onsets notes onsets)
            (segundo-par-notes-by-onsets notes onsets f-length)
            (terceiro-par-notes-by-onsets notes onsets f-list-min)
            (quarto-par-notes-by-onsets notes onsets f-list-max)
            (quinto-par-notes-by-onsets notes onsets f-media)
            (sexto-par-notes-by-onsets notes onsets f-vmode))))

;; ---------------------------------------------------------------------

(defmethod! velocity-per-onset((velocities list) (onsets list))
  :doc

"Returns the number of velocities per onset

Outputs
[1] list of the velocities per onset (of each input file);
[2] the number of velocities  per onset;
[3] the lowest velocity value on each onset (for each file);
[4] the highest velocity value on each onset (for each file);
[5] the velocitiesÕ average value per onset (for each file);
[6] the velocitiesÕ mode value per onset. If not existing, the algorithm returns back the average velocity (as in the 5th
ouput)."

     :initvals (list dvelocities donsets)
     :indoc '("list of velocities"
           "list of onsets (ms)")
     :icon 131
     :numouts 6

     (let* ((f-list-min #'list-min)
          (f-length #'length)
          (f-list-max #'list-max)
          (f-media #' media)
          (f-vmode #'vmode-sem-texto))

       ;essa funcoes estao em stats.lisp
       (values (primeiro-par-notes-by-onsets velocities onsets)
            (segundo-par-notes-by-onsets velocities onsets f-length)
            (terceiro-par-notes-by-onsets velocities onsets f-list-min)
            (quarto-par-notes-by-onsets velocities onsets f-list-max)
            (quinto-par-notes-by-onsets velocities onsets f-media)
            (sexto-par-notes-by-onsets velocities onsets f-vmode))))

;; ---------------------------------------------------------------------

(defmethod! density-per-onset ((notes list) (onsets list))
  :doc
  "Returns the density qualities per onset according to a choice of criteria (see outputs below)

Outputs
[1] relative densities per onset;
[2] average density value from [1] (one value per file);
[3] weighted index of [2] (1.0) corresponds to the higher value of [2]);
[4] absolute densities per onset (an integer corresponding to the number of notes per onset);
[5] average value from [4];
[6] weighted index of [5] (1.0) corresponds to the higher value of [5])."

  :initvals (list dnotes donsets)
  :indoc '("list of notes (midicents)"
           "list of onsets (ms)")
  :icon 131
  :numouts 6

  (let ((notes-onsets (primeiro-par-notes-by-onsets notes onsets))
         (number-onsets (segundo-par-notes-by-onsets notes onsets #'length)))
    ;;essas funcoes estao em time.lisp
  (values (density-rel-by-onset-rec notes-onsets)
          (average-density-rel-by-onset-rec notes-onsets)
          (weighted-av-dens-rel-by-onset notes-onsets)
          (density-abs-onset-rec number-onsets)
          (average-density-abs-onset-rec number-onsets)
          (weighted-av-dens-abs-onset number-onsets))))


(defmethod! harmonicity-per-onset ((notes list) (onsets list) (range number) (use-virtual-fundamental? number) (user-fundamental number)
                           (threshold number) (round-harmonic-object? number) (transpose-pitches? number) (remove-duplicates? number))
         :doc
  "Returns the harmonicity qualities per onset according to a choice of criteria (see outputs below)

Outputs
[1] relative harmonicity per onset;
[2] average harmonicity value from [1] (one value per file);
[3] weighted index of [2] (1.0) corresponds to the higher value of [2]);
[4] absolute harmonicities per onset (an integer corresponding to the number of notes per onset);
[5] average value from [4];
[6] weighted index of [5] (1.0) corresponds to the higher value of [5])."

  :initvals (list dnotes donsets 16 1 0 3500 1 1 1)
  :indoc '("list of notes (midicents)"
           "list of onsets (ms)"
           "a single integer, which informs the number of harmonics to be taken in account for the building of the paradigmatic harmonic spectrum the input file will be compared to. Recommended values: 10 to 16 (default)."
            "if YES (1) (default),  the virtual fundamental detection algorithm is activated. If NO (0),  the fundamental will be considered to be the lowest pitch of the input file. "
            "a pitch in midicents to be use as the fundamental. If different of 0 (default) all other fundamental options are bypassed."
            "a single value in midicents, used by the virtual fundamental algorithm; avoids transposing any pitch below this threshold, and other restrictions. Default (3500) well suited  for piano music."
            "if YES (1) (default), the paradigmatic harmonic object is chromatically rounded. If NO (0),  no."
            "if YES (1) (default),  the e-deviation  algorithm transposes down the input pitches which are situated outside the user-defined harmonic range, in such a way there are also considered for harmonic quality evaluation. If NO (0), these pitches are not considered."
           "if YES (1) (default,  recommended), all duplicated input pitches are removed for analysis. If NO (0), they are not removed.")
  :icon 131
  :numouts 5

  (let* ((notes-onsets (primeiro-par-notes-by-onsets notes onsets))
        (primeirasaida (harmonicity-rel-per-onset-rec notes-onsets range use-virtual-fundamental? user-fundamental threshold round-harmonic-object? transpose-pitches? remove-duplicates?)))

    (values primeirasaida
            (medialistadelistas primeirasaida)
            (weight-per-onset-rec notes-onsets range use-virtual-fundamental? user-fundamental threshold round-harmonic-object? remove-duplicates?)
            (fundamental-per-onset-rec notes-onsets use-virtual-fundamental? user-fundamental threshold remove-duplicates?)
            (coincidents-harmonics-per-onset-rec notes-onsets range use-virtual-fundamental? user-fundamental threshold round-harmonic-object? remove-duplicates?))
))


(defmethod! cognitive-sonance-per-onset ((midicents list) (onsets list) (intervals list))
            :doc
            "Returns the cognitive sonance rate per onset. See Cognitive-Sonance function for more informations.

Inputs
Midicents: a list of lists (in midicents).
Intervals: a single list of 12 integers. ordered from the most consonant to the most dissonant interval. Default: the values in the above table, 'interv.' column.

Output
[1] List of lists containing the sequence of each file intervals (integers
starting from 1 for minor second), from the lowest pitch up, per onset.
[2] The sonance weight of each on these intervals, per onset.
[3] Average value of each [2] list, which is the sonance weight per onset."
            :initvals (list dnotes donsets '(12 7 5 4 9 8 3 6 10 2 11 1))
            :indoc '("midicents"
                     "onsets"
                     "intervals")
            :icon 131
            :numouts 3

            (let* ((notes-onsets (primeiro-par-notes-by-onsets midicents onsets))
                  (entrada (entrada-sonance-per-onset notes-onsets))
                  (intervalos (intervalos-sonance-per-onset entrada))
                  (oitavas (oitavas-sonance-per-onset entrada intervalos))
                  (tabela '((2 3 4 5 5 5 6 7 7 9 15 17)
                   (0 0.09 0.18 0.27 0.36 0.45 0.54 0.63 0.72 0.81 0.9 1)
                   (0 0 0.09 0.135 0.18 0.225 0.27 0.315 0.36 0.54 0.6 0.75)
                   (0 0 0 0 0 0 0 0 0 0.27 0.3 0.5)
                   (0 0 0 0 0 0 0 0 0 0 0 0.25)
                   (0 0 0 0 0 0 0 0 0 0 0 0)))
         (tabela (insere-inicio tabela intervals))
         (tabela-oitavas '(0 1 2 3 4))
         (quartoifaninhado (quarto-if-aninhado-sonance-per-onset tabela tabela-oitavas oitavas intervalos))
         (terceiroifaninhado (terceiro-if-aninhado-sonance-per-onset tabela tabela-oitavas oitavas intervalos quartoifaninhado))
         (segundoifaninhado (segundo-if-aninhado-sonance-per-onset tabela tabela-oitavas oitavas intervalos terceiroifaninhado))
         (primeiroifaninhado (primeiro-if-aninhado-sonance-per-onset tabela tabela-oitavas oitavas intervalos segundoifaninhado)))
          (values entrada
                  primeiroifaninhado
                  (media-lista-de-listas-per-onset primeiroifaninhado))))

(defmethod! spatial-linearity-per-onset ((midicents list) (onsets list) (remove-duplicates? number))
      :doc
      "Measures how equidistant are distributed the pitches. See spatial-linearity for more details.

Input
Midicents: a list of lists (in midicents).
Remove-duplicates: if YES (1) (the default, recommended), removes the duplicates values of the midicents list.

Output
[1] Gives the spatial-linearity weight per onset. Caution: (0.0) means maximum linearity, (1.0) means minimum linearity (i.e. maximum complexity).
[2] Gives the paradigmatic interval (in mcs), per onset."
      :initvals (list dnotes donsets 1)
      :indoc '("midicents"
               "onsets"
               "if YES (1) (default,  recommended), all duplicated input pitches are removed for analysis. If NO (0), they are not removed.")
      :icon 131
      :numouts 2

     (let* ((notes-onsets (primeiro-par-notes-by-onsets midicents onsets)))
       (values (s-linearity-rec-per-onset notes-onsets remove-duplicates?)
           (itv-paradigm-rec-per-onset notes-onsets))
))


;; ---------------------------------------------------------------------

(defmethod! events-density ((onsets list) (duration list) (smaller number))
   :doc
"Gives the relative density of events along the input file.
Input: onsets, duration and an optional smaller-impulse given by the user.
When default (0), this value is calculated by the 'smaller-impulse' function

Output
A list of the relative eventsÕ density of each file."

   :initvals (list donsets ddurations 0)
   :indoc '("list of onsets (ms)"
            "list of duration (ms)"
            "smaller impulse")
   :icon 131

   (t-density-new-rec onsets duration smaller))

;; ---------------------------------------------------------------------

(defmethod! time-linearity ((onsets list) (step number) (duration list))
   :doc
"Measures how equidistant are distributed the events along the time.
The time gap between each adjacent onset is compared to a paradigmatic distribution which corresponds to the value of milliseconds
it would be necessary for the onsets to be exactly equidistant. The higher the weight, the less linear the event distribution.
The 'step' input allows to work with bigger units than one ms (the default value). The 3d input needs the first output of 'file-duration', in order to include in the evaluation of equidistance, the final offset of the file.

Outputs
[1] gives the time-linearity weight of each file. Caution: (0.0) means maximum linearity, (1.0) means minimum
linearity.
[2] gives the paradigmatic interval of equidistance (in ms), for each file."

   :initvals (list donsets 1 '(5000))
   :indoc '("list of onsets (ms)"
            "the time unit to work with (ms)"
            "file-duration first output")
   :icon 131
   :numouts 2

   (values (t-linearity-rec (insert-position duration (remove-duplicates-list-of-lists onsets)) step 0)
           (itv-paradigm-rec onsets step))) ;esta localizada em space.lisp; t-linearity-rec se encontra em time.lisp

;; ---------------------------------------------------------------------

(defmethod! pitch-direction ((notes list) (onsets list))
   :doc
"Gives the objectÕs directional profile, i.e. the global direction (descending and/or ascending) of the objectÕs sequence of events.
The more unidirectional the global pitch profile (as a scale-object would be), the higher the weight. The output
can be relative (first output), resulting in a negative value if the global direction is descending.
The absolute value (second output) is an indicator of the strength of the directionality.

Outputs
[1] the relative pitch-direction rate (negative value if the profile is descending);
[2] the absolute pitch-direction rate (always positive)."

   :initvals (list dnotes donsets)
   :indoc '("list of notes (midicents)"
            "list of onsets (ms)")
   :icon 131
   :numouts 2

   ;Para o metodo nao-recursivo, usa-se apenas "abs" ao inves da construcao abaixo com mapcar
   (values (p-direction-rec notes onsets)
           (mapcar #'abs (p-direction-rec notes onsets))))


;; ---------------------------------------------------------------------

(defmethod! amplitude-deviation ((velocities list))
   :doc
"Gives the relative dispersion rate, with the addition of an evaluation of the direction and slope rate of the attack and decay curves.
This is obtained by measuring the velocity differences between the two first (attack) and two last (decay) events.
The stronger the attack portion of the file, the higher the positive value of the attack algorithm. The softer this attack, the higher
the negative value. And reversely for the decay. The tenth of the square-root of the absolute sum of these two data is summed to the relative dispersion rate in order to produce the relative amplitude-deviation value.

Outputs
[1] A  list of formated  lists : (relative-deviation (attack decay) amplitude-deviation);
[2] Amplitude-deviation (a single value for each input file)."
   :initvals (list dvelocities)
   :indoc '("list of velocities list")
   :icon 131
   :numouts 2

   (values (v-env-rec velocities)
           (LastValueRec velocities)))

;; ---------------------------------------------------------------------

(defmethod! rate (value max (base number) (decimals number))
  :doc
"Returns the percentage of 'val' in 'max'. The user may enter a custom 'base' <default: 100> and a number of 'decimals' for results.

Output
The rate."
;Os valores default estao passiveis de substituicao
  :initvals '('(25 50) 50 100 2)
  :indoc '("value (number or list)"
           "max (number or list)"
           "base"
           "decimals")
  :icon 135

  (taux value max base decimals))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;ACHRONIC ANALYSIS
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;*****************************************************************************************
;**********************  Versão recursiva ************************************************
;*****************************************************************************************
(defmethod! spatial-analysis ((notes list) (range-list number) (registers-list list) (register-weight-list list) (remove-duplicates? number))
   :doc
"Gives the input fileÕs RELATIVE RANGE and several register informations.
Range (soalambitus) means a pitch space defined by two boundary pitches.
Relative range compares the input objectÕs pitch range to a predefined global pitch range, which can be for instance the whole
music pitch range, the instrument or ensemble global pitch range, or the human ear pitch range ('range' input) . Provides
complementary REGISTER informations, particularly the REGISTER DISTRIBUTION, which  informs how pitches are distributed along
the objectÕs pitch range. The user must declare in how many slices (registers) the global pitch range is to be partitionned ('registers' input)
Ñ the default partition, by octaves, can be useful in many cases. Moreover, if he wants so, the user may define a quality weight
to each of these registers ('register-weight' input). Thus the algorithm informs how many notes are in how many registers, and can give to the input
object a sonic quality weight according to the way the pitches are distributed in the different registers.


Outputs (from left to right):

[Output (from left to right)
[1]  gives a list of lists of the following values in this order: relative  range, absolute range, register-filling, register
distribution, average spatial quality;
[2]  returns the fileÕs average spatial quality Ñ i.e. the average value of [3] (relative pitch range) and [10] (register
distribution).
[3]  gives the relative range of the input file, compared to the predefined global pitch range; a value (1.0) means the
input file occupies the whole global range;
[4]  gives the absolute range of the input file, i.e. the diference between the highest and the lowest notes.
[5]  gives the number of notes per register - a list of integers, ordered from lowest to highest register; no note in a
register returns 0;
[6]  gives the register-filling, a relative value which gives the filling rate of the registers, compared to their total
number; a value (1.0) means each register contains at least one note;
[7]  is the list of the numeric label of the  felt registers;  the label is an integer from 0 upwards (0 is the lowest register);
[8]  returns the lowest value of [6].
[9]  returns the highest value of [6]. These two outputs are useful when evaluating a sequence of files (midifiles),
because they can be used to draw the graphic envelope of register-filling;
[10] gives the register distribution, a relative weight which is based on  the user-defined 'register-weight' input."

   :initvals (list dnotes
               8700
               '((1200 2300) (2400 3500) (3600 4700) (4800 5900) (6000 7100) (7200 8300) (8400 9500) (9600 10700) (10800 11900))
               '(1)
               1)
   :indoc '("the input object, a list of at least two pitches (in midicents)."
            "a single value in midicents, which informs the interval of the global range (soalambitus) of the instrument or music or whatever global pitch range  the input data is to be compared. The default value (8700) means an instrument which range is 88 notes, e.g. a piano."
            "a list of sub- lists of two elements in midicents, which indicates the lowest and higest pitch boundaries of each of the registers (slices) in which the instrument or music is to be partitioned. The default values partition the input file into 9 registers of 1 octave each, from C to B."
            "a list of values which sum must be 1. Each value gives a qualitative weight - scaled 0-1 - to the corresponding register. There must be as weight values as there is register partitions, and their sum must always be 1. The default value (1) allows to bypass this parameter."
            "if YES (1) (default,  recommended), all duplicated input pitches are removed for analysis. If NO (0), they are not removed.")
   :icon 130

   :icon 130
   :menuins '((4 (("use" 1) ("don't" 0))))
   :numouts 10



(values  (primeiro-par-spatial-analysis notes range-list registers-list register-weight-list remove-duplicates?)
          (segundo-par-spatial-analysis notes range-list registers-list register-weight-list remove-duplicates?)
          (terceiro-par-spatial-analysis notes range-list remove-duplicates?)
          (quarto-par-spatial-analysis notes remove-duplicates?)
          (quinto-par-spatial-analysis notes registers-list remove-duplicates?)
       	  (sexto-par-spatial-analysis notes registers-list remove-duplicates?)
          (setimo-par-spatial-analysis notes registers-list remove-duplicates?)
	  (oitavo-par-spatial-analysis notes registers-list remove-duplicates?)
	  (nono-par-spatial-analysis notes registers-list remove-duplicates?)
	  (decimo-par-spatial-analysis notes registers-list register-weight-list remove-duplicates?)))


;funções estão em space2.

;****************************************************************************************

;*****************************************************************************************
;**********************  Versão recursiva ************************************************
;*****************************************************************************************
(defmethod! piano-spatial-analysis ((notes list) (remove-duplicates? number))
   :doc
"Gives the input fileÕs RELATIVE RANGE and several register informations. This patch is a version of Spatial Analysis optimized for piano music.
Relative range compares the input objectÕs pitch range to the piano global pitch range (88 semitons). Provides
complementary REGISTER informations, particularly the REGISTER DISTRIBUTION, which  informs how pitches are distributed along
the objectÕs pitch range. Seven piano registers have been defined according to some global timbral differences caused by
physical or mechanical variations:
      -3: (2100 3400)
      -2: (3500 4300)
      -1: (4400 5200)
       0: (5300 7700)
       1: (7800 8600)
       2: (8700 9800)
       3: (9900 10800)
The REGISTER DISTRIBUTION quality weight increases from (0.0) for the central register (F3-F5, label 0) to (0.25) for both highest and lowest
regions.
The greater the number of registers an object occupies, and/or the more extreme the register, the greater the weight.

Input
Midicents: the ÒmidicentsÓ list of each MIDI file, inside the piano range (2100-10700).
Remove-duplicates: if YES (1) (the default, recommended), removes the duplicates values of the midicents list. If (0),
they are not removed.

Output lists
 [1] gives a list of the following values in this order: relative  range,  absolute range, register-filling, register
distribution, average spatial quality;
[2] returns the fileÕs average spatial quality Ñ i.e. the average value of [3] (relative pitch range) and [10] (register
distribution).
[3] gives the relative range of the input file, compared to the piano global pitch range; a value (1.0) means the input file
occupies the whole piano range;
[4]  gives the absolute range of the input file, i.e. the diference between the highest and the lowest notes.
[5] gives the number of notes per register - a list of integers, ordered from lowest to highest register; no note in a
register returns 0;
[6] gives the register-filling, a relative value which gives the filling rate of the registers, compared to their total
number; a value (1.0) means each register contains at least one note;
[7] is the list of the numeric label of the  felt registers;  the label is an integer from -3 to 3 (0 is the central register);
[8]  returns the lowest value of [7].
[9] returns the highest value of [7]. These two output are useful when evaluating a sequence of files, because they can
be used to draw the graphic enveloppe of register-filling;
[10] gives the register distribution."

   :initvals (list dnotes
               1)
   :indoc '("the input object, a list of at least two pitches (in midicents), inside the piano range (2100-10700)."
            "if YES (1) (default,  recommended), all duplicated input pitches are removed for analysis. If NO (0), they are not removed.")
   :icon 129
   :menuins '((1 (("use" 1) ("don't" 0))))
   :numouts 10

   (let* ((range-list 8700)
          ;A DIFERENCA DA SPATIAL-ANALYSIS EH O VALOR DADO A LISTA DE REGISTROS #REGISTERS# e de #REGISTER-WEIGHT#
          (registers-list '((2100 3400) (3500 4300) (4400 5200) (5300 7700) (7800 8600) (8700 9800) (9900 10800)))
          (register-weight-list '(0.25 0.166 0.083 0 0.083 0.166 0.25))
          (setimo-par (setimo-par-piano-spatial-analysis notes registers-list remove-duplicates?)))


(values  (primeiro-par-spatial-analysis notes range-list registers-list register-weight-list remove-duplicates?)
          (segundo-par-spatial-analysis notes range-list registers-list register-weight-list remove-duplicates?)
          (terceiro-par-spatial-analysis notes range-list remove-duplicates?)
          (quarto-par-spatial-analysis notes remove-duplicates?)
          (quinto-par-spatial-analysis notes registers-list remove-duplicates?)
       	  (sexto-par-spatial-analysis notes registers-list remove-duplicates?)
          ;(setimo-par-piano-spatial-analysis notes registers-list remove-duplicates?);modif
	  setimo-par
          ;(oitavo-par-piano-spatial-analysis notes registers-list remove-duplicates?)
	  ;(nono-par-piano-spatial-analysis notes registers-list remove-duplicates?)
	  (oitavo-par-piano-spatial-analysis2 setimo-par)
          (nono-par-piano-spatial-analysis2 setimo-par)
          (decimo-par-spatial-analysis notes registers-list register-weight-list remove-duplicates?))));



;*****************************************************************************************
;**********************  Versão recursiva ************************************************
;*****************************************************************************************
(defmethod! sonic-quality-analysis ((notes list) (velocities list) (ped list)
                                    (output-choice  number) (round number))

"A method for analysing the sonic qualities (q) of a Piano set of pitches (in midicents).
Each written note is given a weight (k), a function which measures the relative decline
of the timbral complexity of each note in proportion to the pitch of the fundamental frequency.
Then this value is weighted by the relative intensity (v) and the Pedal.
The resulting sonic quality weight is 'q'.
See Manual for details.

Outputs
 [1] returns a list containing a selection of main output results, in this order: Q sum (3d output), average k value (5th
output), q gaps (6th output), velocities mode (7th output).
[2] gives the list of the q value of each input note, in their onset order;
[3] returns ÒQ sumÓ (the most significant information) Ñ the weighted sum of q values, including pedal multiplicator;
[4]  gives the list of the k value of each note;
[5] returns the average k value for all notes;
[6]  returns the q gaps Ñ i.e.the average value of intervalic gaps between adjacent q values;
[7] returns the velocities mode Ñ i.e. most frequent value (or, if not applicable, the average value) of the input file(s)Õs
velocities; scale 0-127."


  :initvals (list dnotes dvelocities dCC4s 1 2)

  :indoc '("list of notes (midicents) (2100 - 10800)"
           "list of velocities (0-127)"
           "list of pedal (0 - 127) (see manual for explanations)"
           "mode or average of list of pedal"
           "the number of rounding")
  :icon 129
  :menuins '((3 (("mean" 1) ("moda" 0))))
  :numouts 7

  (values (primeiro-par-sonic-quality-analysis notes velocities ped output-choice round)
          (segundo-par-sonic-quality-analysis notes velocities round)
          (terceiro-par-sonic-quality-analysis notes velocities ped output-choice round)
          (quarto-par-sonic-quality-analysis notes  round)
          (quinto-par-sonic-quality-analysis notes  round)
          (sexto-par-sonic-quality-analysis notes velocities round)
          (setimo-par-sonic-quality-analysis velocities)))
;****************************************************************************************


;--------------------------------
;  Spatial Fillig Analysis
;--------------------------------


;*******************************************************************************************
;**********************  Versao recursiva harmonicity  *************************************
;*******************************************************************************************
(defmethod! harmonicity ((notes list) (range number) (use-virtual-fundamental? number) (user-fundamental number)
                           (threshold number) (round-harmonic-object? number) (transpose-pitches? number) (remove-duplicates? number))
   :doc
"Evaluates the closeness of the input fileÕs achronic pitch distribution to a paradigmatic harmonic spectrum-like structure.
This structure is deduced from a real or virtual fundamental, and constructed inside a user-given spectrum bandwidth.
The fundamental may be the actual (i.e. real) lowest pitch of the file, or a downward transposition (i.e. virtual) of it, one or
two octaves below. The user makes the option between real or virtual fundamental calculation through the 'use-virtual-fundamental?' input.
The algorithm takes also into account the actual pitch of the lowest tone of the object: a 'threshold' input allows the user to
force the fundamental detection algorithm not to transpose the lowest pitch if it is situated below this threshold, and to
transpose it only an octave lower if it is located within an octave of the threshold.
The user so yet enter a fixed fundamental ('user-fundamental' input).
The spectral bandwidth is computed according to the rank of the upper partial of the paradigmatic harmonic structure.
This rank, an integer, is given by the user ('range' input).
See the manual for a more complete description.

Output
[1] Harmonicity: the average value of [2] and [3]. Caution: (0.0) means maximum harmonicity, or a single note object,
and (1.0) means maximum inharmonicity.
[2] E-Deviation: The deviation between the file's pitches and the deduced harmonic series from the fundamental.
[3] Harmonic Weight: The relative weight of the position of the file's pitches in the harmonic series. The higher the
position, higher the weight.
[4] Fundamental: A list with the fundamental note used by the algorithm (C4 = mcs 6000) and its midicent
corresponding value.
[5] Coincident harmonics: the harmonic ranges' list of the file's pitches that match the harmonic series."

   :initvals (list dnotes 16 1 0 3500 1 1 1)
   :indoc '("list of midicents"
            "a single integer, which informs the number of harmonics to be taken in account for the building of the paradigmatic harmonic spectrum the input file will be compared to. Recommended values: 10 to 16 (default)."
            "if YES (1) (default),  the virtual fundamental detection algorithm is activated. If NO (0),  the fundamental will be considered to be the lowest pitch of the input file. "
            "a pitch in midicents to be use as the fundamental. If different of 0 (default) all other fundamental options are bypassed."
            "a single value in midicents, used by the virtual fundamental algorithm; avoids transposing any pitch below this threshold, and other restrictions. Default (3500) well suited  for piano music."
            "if YES (1) (default), the paradigmatic harmonic object is chromatically rounded. If NO (0),  no."
            "if YES (1) (default),  the e-deviation  algorithm transposes down the input pitches which are situated outside the user-defined harmonic range, in such a way there are also considered for harmonic quality evaluation. If NO (0), these pitches are not considered."
           "if YES (1) (default,  recommended), all duplicated input pitches are removed for analysis. If NO (0), they are not removed.")
   :icon 130
   :menuins '((2 (("use" 1) ("don't" 0)))
             (5 (("use" 1) ("don't" 0)))
            (6 (("use" 1) ("don't" 0)))
             (7 (("use" 1) ("don't" 0))))
 :numouts 5

(values
(primeira-saida-harmonicity notes :listout 1
                                      :range range
                                      :use-virtual-fundamental use-virtual-fundamental?
                                      :user-fundamental user-fundamental
                                      :threshold threshold
                                      :use-round-midicents round-harmonic-object?
                                      :use-transpoct transpose-pitches?
                                      :use-remove-duplicates remove-duplicates?)

(segunda-saida-harmonicity notes :range range
                                      :use-virtual-fundamental use-virtual-fundamental?
                                      :user-fundamental user-fundamental
                                      :threshold threshold
                                      :use-round-midicents round-harmonic-object?
                                      :use-transpoct transpose-pitches?
                                      :use-remove-duplicates remove-duplicates?)
 (terceira-saida-harmonicity notes :listout 1
                                      :range range
                                      :use-virtual-fundamental use-virtual-fundamental?
                                      :user-fundamental user-fundamental
                                      :threshold threshold
                                      :use-round-midicents round-harmonic-object?
                                      :use-remove-duplicates remove-duplicates?)

(quarta-saida-harmonicity  notes :use-virtual-fundamental  use-virtual-fundamental?
                                      :user-fundamental user-fundamental
                                      :threshold  threshold
                                      :use-remove-duplicates  remove-duplicates?)

(quinta-saida-harmonicity notes :listout 1
                                     :range range
                                     :use-virtual-fundamental  use-virtual-fundamental?
                                     :user-fundamental user-fundamental
                                     :threshold  threshold
                                     :use-round-midicents round-harmonic-object?
                                     :use-remove-duplicates  remove-duplicates?)))



;******************************************************************************************

;*******************************************************************************************
;**********************  Versao recursiva spatial-linearity ********************************
;*******************************************************************************************
(defmethod! spatial-linearity ((notes list) (remove-duplicates? number))
   :doc
"Measures how equidistant are distributed the pitches.
The interval between the contiguous pitches is compared to a paradigmatic interval which corresponds to the interval that would
be necessary for pitches to be exactly equidistant (chromatically approximated). The higher the weight, the less linear the pitch
distribution, and, consequently, the more 'complex' the sonic object
Output
[1] gives the spatial-linearity weight of each input file. Caution: (0.0) means maximum linearity, (1.0) means minimum
linearity (i.e. maximum complexity).
[2] gives the paradigmatic interval (in mcs)."

   :initvals (list dnotes 1)
   :indoc '("list of notes (midicents)"
            "if YES (1) (default,  recommended), all duplicated input pitches are removed for analysis. If NO (0), they are not removed.")
   :icon 130
   :menuins '((1 (("use" 1) ("don't" 0))))
   :numouts 2
   (values (s-linearity-rec notes :use-remove-duplicates remove-duplicates?)
           (itv-paradigm-rec notes 100)))

;funções estão em space2.
;*******************************************************************************************

;*******************************************************************************************
;**********************  Versao recursiva spatial-density **********************************
;*******************************************************************************************
(defmethod! spatial-density ((notes list) (remove-duplicates? number))
   :doc
"Gives the relative density of the input file, which is obtained by dividing the total number of pitches
by the theoretical maximum possible number of them, within the actual range of the file. For
instance, a musical file in a typical chromatic cluster form would receive the maximum weight
(1.00). By convention, single note objects return 0.

Output
[1] gives the relative density value of each input file. "

   :initvals (list dnotes 1)
   :indoc '("list of notes (midicents)"
           "if YES (1) (default,  recommended), all duplicated input pitches are removed for analysis. If NO (0), they are not removed.")
   :icon 130
   :menuins '((1 (("use" 1) ("don't" 0))))
   (s-density-unit-rec notes :use-remove-duplicates remove-duplicates?))

(defmethod! number-density ((notes list) (remove-duplicates? number))
  :doc
"Gives the numbers density"
   :initvals (list dnotes 1)
   :indoc '("list of notes (midicents)"
           "if YES (1) (default,  recommended), all duplicated input pitches are removed for analysis. If NO (0), they are not removed.")
   :icon 130
   :menuins '((1 (("use" 1) ("don't" 0))))
   (s-number-density-rec notes :use-remove-duplicates remove-duplicates?))

;*******************************************************************************************

;*******************************************************************************************
;**********************  Versão recursiva **************************************************
;******************************************************************************************
(defmethod! smaller-impulse ((onsets list) (durs list))
  :doc
"Returns the smallest value of the file in ms.

Output
The smaller impulse of each list, in ms."

  :initvals (list donsets ddurations)
  :indoc '("list of onsets (ms)"
           "list of durations (ms)")
  :icon 131


  (ppi-rec onsets durs))

;******************************************************************************************

;*******************************************************************************************
;**********************  Versão recursiva **************************************************
;*******************************************************************************************
(defmethod! file-duration ((onsets list) (durations list))
   :doc
"Returns the duration of the input file in ms.

Output
[1] Absolute duration of each input file, in ms.
[2] Relative duration of each file of the list. The longer file weights (1.0)."

   :initvals (list donsets ddurations)
   :indoc '("list of onsets (ms)"
            "list of durations (ms)")
   :icon 131
   :numouts 2

   (values (tot-duration-rec onsets durations)
            (relative-duration onsets durations)))

(defmethod! relative-span ((onsets list) (durations list) (divider number))
  :doc
  "Taking the longer file as reference, returns the relative duration of all files. Alternatively, the user may enter (in the 3rd input) an absolute value (in ms) as reference (The default value 0 bypass this function).

Output
[1] Relative duration of each file of the list. The longer file weights (1.0), except when 3d input is used.
[2] A list containing ((the real duration of the longer file of the list) (a list containing the real duration of all files of the
entry))."

  :initvals (list donsets ddurations 0)
  :indoc '("list of onsets (ms)"
          "list of durations (ms)"
          "ref. duration (ms)")
  :icon 131
  :numouts 2

  (values (t-span-relative-duration onsets durations divider)
          (segunda-saida-relative-span onsets durations))
)

;******************************************************************************************

;*****************************************************************************************
;********************  Versão Recursiva **************************************************
;*****************************************************************************************
(defmethod! absolute-deviations ((list list) (use-reference number) (reference number) (value number))
  :doc
"This is the standard deviation function, with 6 different options for the
reference value (third input).
0 -> the average value of the input list, which gives the original
standard deviation as used in common statistics;
1 -> the first element of the list;
2 -> the smallest element of the list;
3 -> the greatest element of the list;
4 -> the mode ,i.e., the most frequent element of the list Ñ if not
existing, the algorithm goes back to the average value.
The 6th option is offered trhough the 4th input: a fixed number to be
entered by the user.
The 2d input lets the computer know what is the user's choice - either 3d
or 4th input. "

   :initvals (list dnotes 1 4 0)
   :indoc '("list of values"
            "if 1, uses the reference value entered in 3d input; if 0, uses the reference value in the 4th input"
            "reference value of the input list: 0 = average; 1 = first element; 2 = smallest element; 3 = ;greatest element; 4 = mode (or average if no mode)"
            "user reference value")
   :icon 135
   :menuins '((1 (("use" 1) ("don't" 0))))

   (deviations-rec list :use-reference use-reference :reference reference :value value))

;*****************************************************************************************

;*****************************************************************************************
;********************  Versão Recursiva **************************************************
;*****************************************************************************************

(defmethod! relative-deviations ((list list) (use-reference number) (reference number) (value number) (min number) (max number))
  :doc
"This is the relative deviation function (see manual for details), with 6 different options for the reference value (third input).
1 -> the average value of the input list, which gives the original standard deviation as used in common statistics;
2 -> the first element of the list;
3 -> the smallest element of the list;
4 -> the greatest element of the list;
5 -> the mode ,i.e., the most frequent element of the list Ñ if not existing, the algorithm goes back to the average value.
The 6th option is a number to be entered by the user in the 4th input.
The minimal (5th input) and maximal (6th input) values must inform the algorithm the boundaries of the music or instrument.
The default values Ñ (2100) and (10800), respectively Ñ are preset for piano music.

Inputs (for both tools)
list: list of lists of values;
use-reference: if 1 (default), uses the reference value in the 3d input; if 0, uses the user-defined reference value in the
4th input;
reference: the reference from which the deviation will be calculated: 0 = average value of the input list; 1 = first
element; 2 = smallest element; 3 = greatest element; 4 = mode (or average if no mode available) (default);
value: a user-defined value from which the deviation will be calculated; only used if the use-reference input is on 0.
The two remaining inputs are only available in relative-deviations:
min: the minimal value for building the paradigmatic list.
max: the maximal value for building the paradigmatic list

Output (for both tools)
The absolute or relative deviation of each input list."

   :initvals (list dnotes 1 4 0 2100 10800)
   :indoc '("list of values"
            "if 1, uses the reference value entered in 3d input; if 0, uses the reference value in the 4th ;input"
            "reference value of the input list: 0 = average; 1 = first element; 2 = smallest element; 3 = ;greatest element; 4 = mode (or average if no mode)"            "user reference value "
            "minimal value"
            "maximal value")
  :icon 135
  :menuins '((1 (("use" 1) ("don't" 0))))

 (deviations-rel-rec list :use-reference use-reference :reference reference :value value :min min :max max))

;*****************************************************************************************************

;*****************************************************************************************
;********************  Versão Recursiva **************************************************
;*****************************************************************************************
(defmethod! deviation-between-two-lists ((list1 list) (list2 list))
  :doc
"Returns the divergence rate between the relative standard deviation of two lists of same length. "

   :initvals '('(6000 6400 6700) '(6000 6100 6700))
   :indoc '("list of values"
            "list of values")
  :icon 135

  (first (deviation-list-rec (list list1) (list list2))))

(defmethod! lists-deviation ((list1 list) (lists list))
  :doc
"Returns the deviation rate between a reference list (left input) and n number of lists (right input, in list of lists format). Lists should have the same number of arguments."
  :initvals '('(6000 6400 6700) ((6000 6100 6700) (6500 7000 7500)))
  :indoc '("list of values"
            "list of values")
  :icon 135
  :numouts 1
  (lists-deviation-aux list1 lists)
  )

;******************************************************************************************

;*****************************************************************************************
;********************  Versão Recursiva **************************************************
;*****************************************************************************************
(defmethod! range-filling-rate ((notes list) (range number))
   :doc
"Returns the percentage of the filling of a 'range' (or 'ambitus') entered by the user. Useful to
analyse the evolution of this filling in a sequence of lists. Default (8700): the range of the piano, in
midicents.

Input lists
list: list of values
range: range of values

Output lists
range filling rate"

  :initvals (list dnotes 8700)
   :indoc '("list of notes"
            "value of range")
   :icon 135
   :numouts 1

   (amb-rel-rec notes range)) ; se ligar no parametro ranger
;*****************************************************************************************

;*****************************************************************************************
;********************  Versão Recursiva **************************************************
;*****************************************************************************************
;bands-eval-4 ou how-many-notes/band ou bands-filling??

(defmethod! ranges-filling-density ((elements list) (ranges list))
   :doc
"Verifies how many elements of the elements list are in each of the given ranges.
Ranges are in the following format: ((lower-limit1 higher-limit1) (lower-limit2 higher-limit2) ...).
Output: A list with the number of elements in each of the given ranges."

   :initvals (list dnotes '((1200 2300) (2400 3500) (3600 4700) (4800 5900) (6000 7100) (7200 8300) (8400 9500) (9600 10700) (10800 11900)))
   :indoc '("list of elements"
            "ranges in which the elements will be checked")
   :icon 135
   :numouts 1

   (band-eval-4-rec elements ranges)) ;se ligar no parametro ranger
;******************************************************************************************


;*****************************************************************************************
;********************  Versão Recursiva **************************************************
;*****************************************************************************************
;bands-eval-3 ou which-bands-are-occupied ou active-bands??
(defmethod! active-ranges ((elements list) (ranges list))
   :doc
"Verifies which of the ranges contain any of the given elements.
Ranges are in the format ((lower-limit1 higher-limit1) (lower-limit2 higher-limit2)).
Output: a list with the index of the ranges which contain any data, beginning in 0."

   :initvals (list dnotes '((1200 2300) (2400 3500) (3600 4700) (4800 5900) (6000 7100) (7200 8300) (8400 9500) (9600 10700) (10800 11900)))
   :indoc '("list of elements"
            "ranges in which the elements will be checked")
   :icon 135
   :numouts 1

   (band-eval-3-rec elements ranges)) ;se ligar no parametro ranges
;*****************************************************************************************

;*****************************************************************************************
;********************  Versão Recursiva **************************************************
;*****************************************************************************************
(defmethod! density-rate ((list list) (step number) (remove-dups number))
  :doc
"Returns the density rate of the given list, taking step as the smallest possible interval between two elements.

Output lists
The density rate."

  :initvals (list dnotes 100 1)
  :indoc '("list of values"
           "unit for density evaluation"
           "in a non-zero value, remove the duplicates elements from the list")
  :icon 135
  :numouts 2
  :menuins '((2 (("use" 1) ("don't" 0))))

  (values (densidade-rec list :remove-dups remove-dups :step step)
          (densidade-abs-rec list :remove-dups remove-dups :step step)))
;****************************************************************************************

(defmethod! velocity-rate ((velocity list) (velo-parag number) (average number) (rounding number))
  :doc
 "Takes the velocity list of each input file and returns a mean (or mode) relative velocity rate.
Mean or mode are chosen through the 3d input option.

Output
[1] a list of lists of velocity rates
[2] a list of absolute average (or mode) velocity of each input list."

  :initvals (list dvelocities 127 1 2)
  :indoc '("velocity list"
            "paradigmatic value (divider)"
             "output choice (mode or average)"
              "round")
  :icon 135
  :menuins '((2 (("mode" 1) ("mean" 0))))
  :numouts 2
  (values (primeiro-par-velocity-rate velocity :velo-parag velo-parag :average average :rounding rounding)
          (segundo-par-velocity-rate velocity :average average :rounding rounding)))


;*****************************************************************************************
;********************  Versão Recursiva **************************************************
;*****************************************************************************************
(defmethod! rate ((value list) max (base number) (decimals number))
  :doc
"Returns the percentage of 'val' in 'max'. The user may enter a custom 'base' <default: 100> and a number of 'decimals' for results. "

  :initvals '('(25 50) 50 100 2)
  :indoc '("value (number or list)"
           "max (number or list)"
           "base"
           "decimals")
  :icon 135

  (taux-rec value max base decimals))

;*****************************************************************************************

;*****************************************************************************************
;********************  Versão Recursiva **************************************************
;*****************************************************************************************
(defmethod! linear-distribution-rate ((elements list) (step number) (remove-duplicates? number))
  :doc
"Evaluates the way the elements of a list are distributed, according to a linearity paradigm. 'step'
gives the unit base for calculation. Returns two values: the first one gives the linearity rate, where
the most regular the distribution (i.e. the more equidistant the elements of the list), the smaller the
rate; the second one gives the paradigm, that is, the exact interval it would have to exist between
each element to have the most regular distribution.

Output lists
[1] The linear distribution rate. Caution: (0.0) means maximum linearity, (1.0) means minimum linearity
[2] The paradigm."

  :initvals (list dnotes 100 1)
  :indoc '("list of elements"
           "step"
           "if YES (1) (default,  recommended), all duplicated input pitches are removed for analysis. If NO (0), they are not removed.")
  :icon 135
  :menuins '((2 (("use" 1) ("don't" 0))))
  :numouts 2

  (values (t-linearity-rec elements step remove-duplicates?)
          (itv-paradigm-rec elements step)))
;******************************************************************************************

;*****************************************************************************************
;********************  Versão Recursiva **************************************************
;*****************************************************************************************
(defmethod! list-stats ((list list))
  :doc
"Returns the minimal, maximal, average and mode value of the 'list' input.

Outputs (from left to right):
[1] the minimal value of list;
[2] the maximal value of list;
[3] the average of list;
[4] the mode of list, i.e., the most frequent element of the list Ñ if not existing, the algorithm goes back to the average value."

  :initvals (list dnotes)
  :indoc '("list of values")
  :icon 135
  :numouts 4

  (values ( primeiro-par-list-stats list) (segundo-par-list-stats list) (terceiro-par-list-stats list)
                (quarto-par-list-stats list)))
;*********************************************************************************************

;*****************************************************************************************
;********************  Versão Recursiva **************************************************
;*****************************************************************************************
(defmethod! relative-list-stats ((list list) (divisor number))
  :doc
"Divides the 'list' by 'divisor' and returns the minimal, maximal, average and mode value of the 'list'
input, relative values between 0 and 1.

Outputs (from left to right):
[1] the minimal value of list;
[2] the maximal value of list;
[3] the average of list;
[4] the mode of list, i.e., the most frequent element of the list Ñ if not existing, the algorithm goes back to the average value;"

  :initvals (list dvelocities 127)
  :indoc '("list of values"
           "divisor")
  :icon 135
  :numouts 4

  (values (primeiro-par-relative-list-stats list)
          (segundo-par-relative-list-stats list)
          (terceiro-par-relative-list-stats list)
          (quarto-par-relative-list-stats list)))
;*********************************************************************************************

;*****************************************************************************************
;********************  Versão Recursiva **************************************************
;*****************************************************************************************
(defmethod! list-index ((list list))
  :doc
"Returns a list of pairs with the format (position-in-list element) - beginning in 0."

  :initvals (list dvelocities)
  :indoc '("list of values")
  :icon 135

  (listindex-rec list))

;****************************************************************************************

;*****************************************************************************************
;********************  Versão Recursiva **************************************************
;*****************************************************************************************
(defmethod! list-weight ((list list))
  :doc
"Returns a list of pairs with the format (element number-of-occurences-in-list)"

  :initvals (list dCC4s)
  :indoc '("list of values")
  :icon 135

  (list-wght-rec list))

;*****************************************************************************************

;*****************************************************************************************
;********************  Versão Recursiva **************************************************
;*****************************************************************************************
(defmethod! how-many-elements? ((list list) (elements list) (output-format number))
  :doc
"Returns how many <elements> (percent) a <list> contains."

  :initvals (list '((6000 6400 6400 6700 8000)) '(6400 6900) 0)
  :indoc '("list"
           "elements"
           "0: the same as input. 1: notes (only works if the other inputs are midicents and using the ;reference C4 (midi-cent 6000)).")
  :icon 135

  (taux-elements-rec elements list output-format))
;****************************************************************************************


;*****************************************************************************************
;********************  Versão Recursiva **************************************************
;*****************************************************************************************
(defmethod! keep-duplicates ((lista list))
  :doc
"Returns a list with the duplicated elements of the list. Also removes no-duplicated elements."

  :initvals (list '((60 61 61 64 69 69 69)))
  :indoc '("list of lists")
  :icon 135

  (keep-dups-rec lista))     ;modif 26/04/2006 max. chama func recursiva
;*****************************************************************************************

;*****************************************************************************************
;*****************************************************************************************
;********************  Versão Recursiva **************************************************
;*****************************************************************************************
(defmethod! find-elements ((elements list) (list-of-lists list))
  :doc
 "Find the elements (in <list of elements to find>), in a <list-of-lists> of numbers or strings. Returns a list with the found element(s) and its (their) position (s).
  A single list for the input <list of lists> must be written as a list of one list, e.g. ((1 2 3))."

  :initvals '('(6 7) '((1 2 3 4) (5 7) (3 4 5) (1 4) (6 7) (4 5 5 7 8)))
  :indoc '("list of elements to find"
           "list of lists")
  :icon 135

  (resultadofindelements elements list-of-lists))
;*****************************************************************************************

;*****************************************************************************************
;********************  Versão Recursiva **************************************************
;*****************************************************************************************
(defmethod! mc->n-SOAL ((midicents list) (reference string))
  :doc
"Takes a midi-cent value or list of midi-cent values, and returns corresponding symbolic (ASCII) note names.
Symbolic note names follow standard notation with middle c (midicent 6000) being C3 if the value of reference (second input)
equals the string C3, or C4 if the value of reference equals the string C4.  Semitones are labeled with a '#'.
Quartertone flats are labeled with a '_', and quartertone sharps with a '+'.  Thus, C3 a quartertone sharp (midicent 6050),
would be labeled C+3.  Gradations smaller than a quartertone are expressed as the closest  quartertone + or - the remaining
cent value (i.e., midicent 5376 would be expressed as F#2-24)."

  :initvals '('(6000) "C4")
  :indoc '("a midicent or list of midicents"
           "reference: C3 or C4")
  :icon 135

  (midicents->notes-rec midicents reference))


;*****************************************************************************************

;*****************************************************************************************
;********************  Versão Recursiva **************************************************
;*****************************************************************************************
(defmethod! mc->pc-SOAL ((midicents list))
  :doc
"Converts midicents into pitch-classes (C = 0)."
  :initvals (list dnotes)
  :indoc '("list of midicents")
  :icon 135

  (midicents->pitch-classes-rec midicents))

;*****************************************************************************************


;*****************************************************************************************
;********************  Versão Recursiva **************************************************
;*****************************************************************************************

(defmethod! pitch-deviations ((midicents list) (onsets list) (use-reference number) (reference number)
               (value number) (min number) (max number) (choice number))

  :doc
"gives the relative dispersion rate of the input MIDI pitches sequence.
Through the last input, the user chooses what note of each onset will be used for analysis:
[1] the lowest note on each onset (midicent);
[2] the highest note on each onset (midicent);
[3] the average pitch on each onset (midicent);
[4] the pitches' mode on each onset (midicent), i.e., the most frequent pitch on each onset Ñ if not existing, the algorithm returns the average pitch, as in the [2] option.

Output:

A list of lists of the relative pitch-deviation of each MIDI file input."

   :initvals (list dnotes donsets 1 4 0 2100 10800 0)
   :indoc '("midicents"
            "onsets"
            "what-reference"
            "ref-for-deviation-calc.1"
            "ref-for-deviation-calc.2"
            "minimal value"
            "maximal value"
            "choice-for-input-pitches")
  :icon 131
  :menuins '((2 (("use" 1) ("don't" 0)))
             (3 (("average" 0) ("first" 1) ("smallest" 2) ("greatest" 3) ("mode" 4)))
            (7 (("average" 0) ("lowest" 1) ("highest" 2) ("mode" 3))))

 (pitch-dev midicents onsets :use-reference use-reference :reference reference :value value :min min :max max
                                      :escolha choice))

;if 1, uses the reference value entered in 4th input; if 0, uses the reference value in the 5th input (entrada 3)
;reference value of the input list: 0 = average; 1 = first element; 2 = smallest element; 3 = ;greatest element; 4 = mode (or average if no mode) (entrada 4)
;user reference value  (entrada 5)
;note choice (ultima entrada)
;*****************************************************************************************************


(defmethod! soalsubstitute ((midicents list) (procurado number) (substituto number))
  :doc
"In all lists of the midicents list of lists, substitutes the procurado value by substituto value"
  :initvals (list dnotes 6400 7200)
  :indoc '("midicents"
           "wanted number"
           "substitute number")
  :icon 130
  :numouts 1

  (procuraemlista midicents procurado substituto))

(defmethod! soalincluded? ((subconjunto list) (conjunto list))
  :doc
"Verifies whether the list subconjunto is included in each list of the conjunto list of lists"
  :initvals (list (first dnotes) '((5700 6000 6400 6700 7000) (6000 6600 6900) (5900 6000 6400 6700)))
  :indoc '("wanted list"
           "list of lists")
  :icon 130
  :numouts 2

  (values (verificainclusaonaslistas subconjunto conjunto)
          (calcula-porcentagens subconjunto conjunto)))


(defmethod! cognitive-sonance ((midicents list) (intervals list))

  :doc
"Sonance, a vector from cognitive  Òmaximum consonanceÓ to Òmaximum
dissonanceÓ, evaluates the contiguous dyads of the input file(s) on the
basis of a weighting of  their relative dissonance. See documentation for
the implemented cognitive model and other details.
Outputs:
[1] list of lists containing the sequence of each file intervals (integers
starting from 1 for minor second), from the lowest pitch up.
[2] the sonance weight of each on these intervals.
[3] average value of each [2] list, which is the sonance weight of each
input file."

  :initvals (list dnotes '(12 7 5 4 9 8 3 6 10 2 11 1))
  :indoc '("midicents"
           "intervals")
  :icon 130
  :numouts 3

  (let* ((entrada (entrada-sonance midicents))
         (intervalos (intervalos-sonance entrada))
         (oitavas (oitavas-sonance intervalos entrada))
         (tabela '((2 3 4 5 5 5 6 7 7 9 15 17)
                   (0 0.09 0.18 0.27 0.36 0.45 0.54 0.63 0.72 0.81 0.9 1)
                   (0 0 0.09 0.135 0.18 0.225 0.27 0.315 0.36 0.54 0.6 0.75)
                   (0 0 0 0 0 0 0 0 0 0.27 0.3 0.5)
                   (0 0 0 0 0 0 0 0 0 0 0 0.25)
                   (0 0 0 0 0 0 0 0 0 0 0 0)))
         (tabela (insere-inicio tabela intervals))
         (tabela-oitavas '(0 1 2 3 4))
         (quartoifaninhado (quarto-if-aninhado-sonance tabela tabela-oitavas oitavas intervalos)) ;estao no arquivo space.lisp
         (terceiroifaninhado (terceiro-if-aninhado-sonance tabela tabela-oitavas oitavas intervalos quartoifaninhado))
         (segundoifaninhado (segundo-if-aninhado-sonance tabela tabela-oitavas oitavas intervalos terceiroifaninhado))
         (primeiroifaninhado (primeiro-if-aninhado-sonance tabela tabela-oitavas oitavas intervalos segundoifaninhado)))
  (values entrada
          primeiroifaninhado
          (medialistadelistas primeiroifaninhado))))


(defmethod! soal-multilists-flat ((information list))
         :doc
"From embedded lists formatted (((onset)(data)...)))), extracts the 'data' and flats it. Intended to adapt the ouput of 'notes-per-onset' and 'velocity-per-onset functions, before connecting them to the segmentation-midi function. When outputs 1 and 2 merge all the MIDI files data, outputs 3 and 4 preserve the multi-list standard SOAL format ((...)). "
  :initvals (list '(((100 (795)) (62 (1570)) (69 (0)) (59 (0)) (65 (1620))) ((69 (0)) (82 (0)) (59 (75)) (35 (1500)))))
  :indoc '("data")
  :icon 131
  :numouts 4

  (let ((backup-information information))
  (values (flat (second (mat-trans (flat-once information))))
          (second (mat-trans (flat-once information)))
          (let ((resultado nil))
            (loop
             (setq resultado (insert-element (flat (second (mat-trans (flat-once (list (first information)))))) resultado))
             (setq information (rest information))
             (when (null information) (return resultado))))
          (let ((resultado nil))
            (loop
             (setq resultado (insert-element (second (mat-trans (flat-once (list (first backup-information))))) resultado))
             (setq backup-information (rest backup-information))
             (when (null backup-information) (return resultado))))))
)

(defmethod! segmentation-midi ((midicents list) (onsets list) (durations list) (velocities list) (percentual number) (option number) (operation-option number) (reference list) (time number) (use-option number) (chosen-value number))
         :doc
"Offers three ways to segment the MIDI file(s), to be selected in the 9th input pop-up
menu:
- through the scanning of the intervals of the list of values of one of the 4
basic parameters (midicents, onsets, durations, velocities;
- through the scanning of the intervals of one of the 'analysis-per-onset' values
output;
- according to a user-defined fixed duration in ms

Inputs:
Midicents: the 'midicents list of each MIDI file (from Multi-midi-reader 1st output);
Onsets: the 'onsets' of each MIDI file (from Multi-midi-reader 2nd output);
Durations: the 'durations' of each MIDI file (from Multi-midi-reader 3rd output);
Velocities: the 'velocities' of each MIDI file (from Multi-midi-reader 4th output);
Percentage: the percent value to be add upon the mode of the reference list;
Option (pop-up menu): to choose what basic parameter is to be used for the 1st way of segmentation;
Operation-option (pop-up menu): the operation used to get the reference value (mode, mean, lowest value, highest value, fixed user-defined value;
Reference: the output of a 'per-onset' function (2nd way of segmentation): a single list per file; may need the SOAL-Multilist-flat;
Time: the fixed duration for the 3rd way of segmentation;
Use-option: pop-up menu to select the segmentation main option (way) the algorithm will work with.
Chosen-value: if 'fixed user-defined value' is selected in the 7th input, the reference value is the value entered in this field. Caution: this value must be the same kind of the chosen segmentation parameter (e.g. integer if integer, relative if relative, milliseconds if milliseconds, etc).


Outputs:
[1] The original(s) midicents list(s), segmented according to the chosen criterium;
[2] The original(s) onsets list(s), segmented according to the same chosen criterium;
[3] The original(s) durations list(s), segmented according to the same chosen criterium;
[4] The original(s) velocities list(s), segmented according to the same chosen criterium;
[5] Informs the mode (eventually with the added percent) the algorithm used for the segmentation;
returns a null value in case of segmentation by 'fixed duration'"
  :initvals (list '((3000 3500 4000 4400 5000 6000 6600 7700) (3000)) '((0 10 10 30 35 35 45 80) (0)) '((50 40 100 10 20 40 40 50) (200)) '((40 40 50 70 69 39 100 127) (100)) 20 0 0 nil 100 1 1000)
  :indoc '("midicents"
           "onsets"
           "durations"
           "velocities"
           "the percent value to be add upon the mode of the reference list"
           "the basic parameter to be used for the 1st way of segmentation"
           "the operation to obtain the reference value"
           "the output of a 'per-onset' function (2nd way of segmentation) (may need the SOAL-Multilist-flat)"
           "the fixed duration for the 3rd way of segmentation"
           "pop-up menu to select the segmentation main option (way) the algorithm will work with"
           "the chosen value to be the reference value")
  :icon 131
  :menuins '((5 (("notes" 0) ("onsets" 1) ("durations" 2) ("velocities" 3)))
             (6 (("mode" 0) ("media" 1) ("lowest value" 2) ("highest value" 3) ("any value" 4)))
             (9 (("distances between the values of one of the input lists" 1) ("result of one of the 'per-onset' functions" 2) ("by fixed duration" 3))))
  :numouts 5

  ;esta no arquivo space.lisp
  (cond ((= use-option 1)
         (let* ((conjunto (list midicents onsets durations velocities))
                (naosegmentada (nth option conjunto))
                (referencia (multiplicacao-de-uma-lista-por-um-numero
                                             (cond ((= operation-option 0) (moda-lista-de-listas (om-abs (distancias-lista-de-listas naosegmentada))))
                                                   ((= operation-option 1) (flat (medialistadelistas (om-abs (distancias-lista-de-listas naosegmentada)))))
                                                   ((= operation-option 2) (min-lista-de-listas (om-abs (distancias-lista-de-listas naosegmentada))))
                                                   ((= operation-option 3) (max-lista-de-listas (om-abs (distancias-lista-de-listas naosegmentada))))
                                                   (t chosen-value))
                                             (+ (g-round (/ percentual 100) 2) 1)))
                (segmentada (segmenta naosegmentada referencia))
                (atributos-segmentados (segmenta-varias-igualmente segmentada (remove-position conjunto (+ option 1))))
                (saida (insere-posicao atributos-segmentados segmentada option)))

           (values (nth 0 saida)
                 (nth 1 saida)
                 (nth 2 saida)
                 (nth 3 saida)
                 referencia)))
        ((= use-option 2)
         (let* ((moda (multiplicacao-de-uma-lista-por-um-numero
                       (cond ((= operation-option 0) (moda-lista-de-listas (om-abs (distancias-lista-de-listas reference))))
                                                   ((= operation-option 1) (flat (medialistadelistas (om-abs (distancias-lista-de-listas reference)))))
                                                   ((= operation-option 2) (min-lista-de-listas (om-abs (distancias-lista-de-listas reference))))
                                                   ((= operation-option 3) (max-lista-de-listas (om-abs (distancias-lista-de-listas reference))))
                                                   (t chosen-value))
                       (+ (g-round (/ percentual 100) 2) 1)))
                (referencia (segmenta reference moda))
            (conjunto (list midicents onsets durations velocities))
            (intermediario (let ((resultado nil)
                                 (information (primeiro-par-notes-by-onsets midicents onsets)))
                             (loop
                              (setq resultado (insert-element (second (mat-trans (flat-once (list (first information))))) resultado))
                              (setq information (rest information))
                              (when (null information) (return resultado)))))
            (segmentado (segmenta-varias-baseado-referencia referencia intermediario))
            (saida (segmenta-varias-igualmente segmentado conjunto)))
        (values (nth 0 saida)
                 (nth 1 saida)
                 (nth 2 saida)
                 (nth 3 saida)
                 moda)))
        (t (let ((segmentado (organiza (percorre-varias midicents onsets durations velocities time))))
           (values (list (nth 0 segmentado))
                   (list (nth 1 segmentado))
                   (list (nth 2 segmentado))
                   (list (nth 3 segmentado)))))));a funcao percorre esta em time.lisp

(defmethod! soal-relative-entropy ((lista list))
        :doc
"Entropy is a way to measure the level of disorder or chaos of a list of values. The higher the entropy, the less predictive, the less 'patterned' the order of values. 'Relative-Entropy' matches the input lists' entropy and orders them from the most entropic one, weighted (1.0).
Input : list of lists
Output :
[1] Relative entropy of each list
[2] Absolute entropy of each list (in 'bits')"

  :initvals (list dnotes)
  :indoc '("input list of lists")
  :icon 131
  :numouts 2

  (let ((entropias (calcula-entropia lista)))
  (values (divisao-de-uma-lista-por-um-numero entropias (list-max entropias))
          entropias)))

(defmethod! save-soal-segmentation ((midicents list) (onsets list) (durations list) (velocities list) (namefile string))
            :doc
            "Transforms the lists of lists from the Segmentation-midi output into a number of
MIDI files, according to the segmentation result."

            :initvals (list dnotes donsets ddurations dvelocities "midifile")
            :indoc '("midicents"
                     "onsets"
                     "durations"
                     "velocities"
                     "start name midi files")
            :icon 131
            :numouts 1

            (let ((diretorio (om-choose-directory-dialog)))
              (escreve-arquivos midicents onsets durations velocities diretorio namefile))
)

;; (defmethod! c ((n number) (p number))
;;   :doc
;;   "Combinatory Function
;; Input :
;; [1] Number of elements
;; [2] Number of p-combinations
;; Output : Number of Combinations"
;;   :initvals (list 0 0)
;;   :indoc '("Number n"
;;            "Number p")
;;   :icon 131
;;   :numouts 1

;;   (if (< n 1)
;;       (values 0)
;;   (truncate (/ (fatoreal n) (* (fatoreal p) (fatoreal (- n p))))))
;; )

(defmethod! index-T-Total-Relations ((n number))
  :doc
  "Index T (Totalizacao das relacoes de uma densidade-numero).
Observacao: Para efetuar o calculo no patch, e necessario conferir o artigo “Densidade e linearidade na configuração de texturas musicais” (GENTIL-NUNES, 2003). Ver “Calculo do indice T” (pag. 02).
Input: Insira o valor numerico referente a “n” que se refere ao numero de relacoes de densidade/numero. Por default o input e zero.
Output: Total de Relacoes
Importante: Para inserir a densidade numero, alterar o valor de zero no topo do patch pelo valor a ser calculado."
  :initvals (list 0)
  :indoc '("Number n (densidade-numero)")
  :icon 131
  :numouts 1

  (truncate (/ (* n (- n 1)) 2))
)

;; (defmethod! index-i-Relations-of-Identity ((r number))
;;   :doc "Index i (Relacoes de Identidade)
;; Input : Numero de Componente Reais
;; Output : Relacoes de Identidade
;; "

;;   :initvals (list 0)
;;   :indoc '("Number r (numero de componentes reais)")
;;   :icon 131
;;   :numouts 1
;;   (loop for i from 1 to r sum (index-T-Total-Relations i))
;; )

(defmethod! index-c-Relations-of-Contrast ((t-num number) (i-num number))
  :doc "Index C (Relacoes de Contraste)
Observacao: Para efetuar os calculos no patch, e necessario conferir o artigo “Densidade e linearidade na configuracao de texturas musicais” (GENTIL-NUNES, 2003). Ver “Calculo do indice C” (pag. 04).

Input 1: Ligar o output do patch “INDEX-T-TOTAL-RELATIONS”
Input 2: Relacoes de identidade (i) (ver tabela da pag. 05). Inserir um “input” com o valor numerico referente ao somatorio (i).

Output: Relacao de contraste (retorno referente ao segundo valor da tabela disponivel na pag. 05, “pares i, c ”)."
  :initvals (list 0 0)
  :indoc '("Index T (totalizacao das relacoes de uma densidade-numero)"
           "Index i (Relacoes de identidade)")
  :icon 131
  :numouts 1
  (- t-num i-num)
)
