;----------------------------; Soal1.2 Multi-Midi-reader; Soal 3.1 Soal-Reader;----------------------------(in-package :om)(defmethod! soal-reader ((quant number) (self chord-seq)  &rest chord-seqs ):icon 127:initvals '( 5 '(nil)):numouts 6:doc "the first input is only used to Midifiles objects. The remaining inputs import midi, chord-seq or sdifiles objects.Converts Chord-seq objects or Sdif files (using 1MRK / 1TRC frames) into a symbolic description. The outputs are:  [1] Notes; [2] Onsets; [3] Durations; [4] Velocities; Click option-right-arrow to increment the number of Chord-seq inputs.if the inputs are Midifile, Converts Midifile objects into a symbolic description. The outputs are:  [1] Notes; [2] Onsets; [3] Durations; [4] Velocities; [5] Continuous Control #4; [6] Onsets Quantifications Analysis. The first input is a value in ms which rounds the onset and duration values of the input files. 1 means no rounding. The remaining inputs import MIDI files. Click option-right-arrow to increment the number of Midifile inputs. The last output gives information about the changes due to the quantification of the onsets. It is a list with three elements: the first is the length of the original onsets' list, the second is the length of the quantified onsets' list and the third is the ratio of the reduction, if any."    :indoc '("Quantification of onset and duration (in ms) of midifiles"           "Click option-right-arrow to increment the number of inputs.") ;Atribui a inputchord-seq os valores de chord-seqs da entrada.    (if (> (length chord-seqs) 0)    (setq inputchord-seq (cons self chord-seqs))    (setq inputchord-seq (list self)))  (values         (chord-seq-notes inputchord-seq)         (chord-seq-onsets-repetidos inputchord-seq)         (chord-seq-dur inputchord-seq)        (chord-seq-vel inputchord-seq)	  ))(defmethod! soal-reader ((quant number) (self sdiffile)  &rest sdiffiles );Atribui a inputsdiffiles os valores de sdiffiles da entrada.    (if (> (length sdiffiles) 0)    (setq inputsdiffiles (cons self sdiffiles))    (setq inputsdiffiles (list self)))  (values          (sdifchords inputsdiffiles)         (sdifonsets inputsdiffiles)         (sdifdurs inputsdiffiles)         (sdifvels inputsdiffiles)         nil         nil         ) )  (defmethod! soal-reader ((quant number) (self midiFile)  &rest midiFiles  );Atribui a inputMidi os valores de midiFile da entrada.    (if (> (length midiFiles) 0)    (setq inputMidi (cons self midiFiles))    (setq inputMidi (list self)))  (Values (multi-midi-notes inputMidi)          (multi-midi-onsets inputMidi quant)          (multi-midi-durs inputMidi quant)          (multi-midi-velos inputMidi)          (multi-midi-continuous-ctrl inputMidi)          (multi-midi-onsets-quant inputMidi quant)))  ;ainda a ser construido, estas listas poderiam ser de objetos do mesmo tipo, ou tipos distintos (defmethod! soal-reader ((quant number) (self list)  &rest lists )    (if (> (length lists) 0)       (setq inputlists (cons self lists))       (setq inputlists (list self)))     (list-extract inputlists)  )(defun list-extract (lists)   (if (= (length lists) 1)	(extract lists)	(cons (extract (first lists)) (list-extract (rest (first lists))))))     ; ultima alteracao aqui_______________---------------->>>>>>>>>>>(defun extract (lists)	(if (= (length lists) 1)				(soal-reader (first (first lists)))				(cons (soal-reader (first (first lists))) (extract (rest (first lists)))))			 );-----------------------------------------------------;-----------------------------------------------------; funcoes recursivas auxiliares a soal reader ;-----------------------------------------------------;-----------------------------------------------------(defun sdifchords(sdiffiles)"function that extracts the chords of a sdif file"  (if  (= (length sdiffiles) 1)                (list  (f->mc (flat (mapcar #'first (getsdifchords (first sdiffiles))))))                (cons  (f->mc (flat (mapcar #'first (getsdifchords (first sdiffiles))))) (sdifchords (rest sdiffiles)))))(defun sdifonsets(sdiffiles)"function that extracts the chords of a sdif file"  (if  (= (length sdiffiles) 1)                                (list  (om-round (om* (flat (mapcar #'second (getsdifchords (first sdiffiles)))) 1000)))                (cons  (om-round (om* (flat (mapcar #'second (getsdifchords (first sdiffiles)))) 1000)) (sdifonsets (rest sdiffiles)))))(defun sdifdurs(sdiffiles)"function that extracts the chords of a sdif file"  (if  (= (length sdiffiles) 1)                (list  (om-round (om* (flat (mapcar #'third (getsdifchords (first sdiffiles)))) 1000)))                (cons  (om-round (om* (flat (mapcar #'third (getsdifchords (first sdiffiles)))) 1000)) (sdifdurs (rest sdiffiles)))))(defun sdifvels(sdiffiles)"function that extracts the chords of a sdif file"  (if  (= (length sdiffiles) 1)                (list  (om-round (om* 10 (om-round  (om/ (flat (mapcar #'third (mapcar #'rest (getsdifchords (first sdiffiles))))) 0.0078125) 2))))                (cons  (om-round (om* 10 (om-round  (om/ (flat (mapcar #'third (mapcar #'rest (getsdifchords (first sdiffiles))))) 0.0078125) 2))) (sdifvels (rest sdiffiles)))))(defun chord-seq-notes (chord-seqs) "Function that extracts the notes from a chord-seq."     (let ((notes (mapcar #'LMidic  chord-seqs)))                    (if (= (length chord-seqs) 1)                                                (list (flat notes))                 (cons  (flat (first notes)) (chord-seq-notes (rest chord-seqs))))                   ))(defun chord-seq-onsets (chord-seqs) "Function that extracts the onsets from a chord-seq."     (let ((onsets (mapcar #'LOnset chord-seqs)))                    (if (= (length chord-seqs) 1)                                (list (flat (list  onsets)))                 (cons  (first onsets) (chord-seq-onsets (rest chord-seqs))))   ) )(defun chord-seq-onsets-corretos (chord-seqs)(if (= (length chord-seqs) 1)(list (mapcar #'length (LMidic (first chord-seqs))))(cons  (mapcar #'length (LMidic (first chord-seqs))) (chord-seq-onsets-corretos (rest chord-seqs)))))(defun chord-seq-onsets-repetidos  (chord-seqs) (setq lista_repetidos (chord-seq-onsets-corretos chord-seqs))(setq result (repetir-onset (chord-seq-onsets chord-seqs) lista_repetidos))(mapcar #'flat result))(defun repetir-onset (onsets lista_repetidos)(if (= (length onsets) 1)(list (repetir-lista  (first onsets )  (first  lista_repetidos)))(cons (repetir-lista  (first onsets )  (first  lista_repetidos)) (repetir-onset (rest onsets) (rest lista_repetidos)))))(defun repetir-lista (lista nvezes)(if (= (length nvezes) 1)(list (repeat-n (first lista ) (first  nvezes)))(cons  (repeat-n (first lista ) (first  nvezes)) (repetir-lista (rest lista) (rest nvezes)))))(defun chord-seq-dur (chord-seqs) "Function that extracts the durations from a chord-seq."     (let ((durs (mapcar #'LDur chord-seqs)))                   (if (= (length chord-seqs) 1)                (list (flat (list  durs)))                 (cons (flat (first durs)) (chord-seq-dur (rest chord-seqs))))   ) )(defun chord-seq-vel (chord-seqs) "Function that extracts the velocities from a chord-seq."     (let ((vels (mapcar #'LVel chord-seqs)))                    (if (= (length chord-seqs) 1)                (list (flat (list vels)))                 (cons (flat (first  vels)) (chord-seq-vel (rest chord-seqs))))   ) )(defmethod! multi-midi-reader((quant number) (midiFile midifile) &rest midiFiles)  :initvals '( 5 '(6000 7200))    :doc   "Converts Midifile objects into a symbolic description. The outputs are:  [1] Notes; [2] Onsets; [3] Durations; [4] Velocities; [5] Continuous Control #4; [6] Onsets Quantifications Analysis. The first input is a value in ms which rounds the onset and duration values of the input files. 1 means no rounding. The remaining inputs import MIDI files. Click option-right-arrow to increment the number of Midifile inputs. The last output gives information about the changes due to the quantification of the onsets. It is a list with three elements: the first is the length of the original onsets' list, the second is the length of the quantified onsets' list and the third is the ratio of the reduction, if any."    :indoc '("Quantification of onset and duration (in ms)"           "Click option-right-arrow to increment the number of Midifile inputs.")            :icon 128  :numouts 6;Atribui a inputMidi os valores de midiFile da entrada.    (if (> (length midiFiles) 0)    (setq inputMidi (cons midiFile midiFiles))    (setq inputMidi (list midiFile)))(Values (multi-midi-notes inputMidi)          (multi-midi-onsets inputMidi quant)          (multi-midi-durs inputMidi quant)          (multi-midi-velos inputMidi)          (multi-midi-continuous-ctrl inputMidi)          (multi-midi-onsets-quant inputMidi quant)))(defun multi-midi-notes(midiFiles)   "Function that extracts the notes from a midifile. The file information must be preprocessed with the function mf-info."        (let ((notes (mapcar #'first (OperaMidiFiles midiFiles))))       (if (< (first notes) 129)          (progn             (if (= (length midiFiles) 1)                (list (multiplicacao-de-uma-lista-por-um-numero notes 100))                 (cons (multiplicacao-de-uma-lista-por-um-numero notes 100) (multi-midi-notes (rest midiFiles)))))          (progn             (if (= (length midiFiles) 1)                (list notes)                 (cons notes (multi-midi-notes (rest midiFiles))))))))                   (defun multi-midi-onsets (midiFiles quant)   "Function that extracts the onsets from a midifile. The file information must be preprocessed with the function mf-info."      (let ((onsets (mapcar #'second (OperaMidiFiles midiFiles))))      (if (= (length midiFiles) 1)         (list (multiplicacao-de-uma-lista-por-um-numero (om-round (divisao-de-uma-lista-por-um-numero onsets quant)) quant))         (cons (multiplicacao-de-uma-lista-por-um-numero (om-round (divisao-de-uma-lista-por-um-numero onsets quant)) quant)               (multi-midi-onsets (rest midiFiles) quant)))))(defun multi-midi-durs (midiFiles quant)  "Function that extracts the durations from a midifile. The file information must be preprocessed with the function mf-info."  (let ((durs (mapcar #'third (OperaMidiFiles midiFiles))))     (if (= (length midiFiles) 1)        (list (multiplicacao-de-uma-lista-por-um-numero (om-round (divisao-de-uma-lista-por-um-numero durs quant)) quant))        (cons (multiplicacao-de-uma-lista-por-um-numero (om-round (divisao-de-uma-lista-por-um-numero durs quant)) quant)              (multi-midi-durs (rest midiFiles) quant)))))(defun multi-midi-velos (midiFiles)  "Function that extracts the velocities from a midifile. The file information must be preprocessed with the function mf-info."   (if (= (length midiFiles) 1)         (list (mapcar #'fourth (OperaMidiFiles midiFiles)))      (cons (mapcar #'fourth (OperaMidiFiles midiFiles)) (multi-midi-velos (rest midiFiles)))))(defun multi-midi-onsets-quant (midiFiles quant)  "Function that gives information about changes on the quantified onsets. The output is a list with three elements: the first is the length of the original onsets' list, the second is the length of the quantified onsets' list and the third is the ratio of the reduction."  (let ((length-quant-onstes 	 (length (remove-duplicates (midi-onsets (OperaMidiFiles midiFiles) quant) :from-end t)))        (length-not-quant-onsets (length (remove-duplicates (mapcar #'second (OperaMidiFiles midiFiles)) :from-end t))))     (if (= (length midiFiles) 1)          (list (list length-not-quant-onsets                    length-quant-onstes                    (rate length-quant-onstes length-not-quant-onsets 100 2)))        (cons (list length-not-quant-onsets                    length-quant-onstes                    (rate length-quant-onstes length-not-quant-onsets 100 2))               (multi-midi-onsets-quant (rest midiFiles) quant)))));Esta funcao eh original do antigo leitor midi(SOAL-MIDI-READER). (defun midi-onsets (mididata quant)  "Function that extracts the onsets from a midifile. The file information must be preprocessed with the function mf-info."  (let ((onsets (mapcar #'second mididata)))    (multiplicacao-de-uma-lista-por-um-numero (om-round (divisao-de-uma-lista-por-um-numero onsets quant)) quant)))(defun OperaMidiFiles(midiFiles)   (first (mf-info (first midiFiles))))                (defun multi-midi-continuous-ctrl (midiFiles)    (if (= (length midiFiles) 1)        (list (y-points(get-continuous-ctrl (first midiFiles) "FootController" nil 0 1) )) ;get-continuous-ctrl em CODE:Projects:2-MusicProject:MIDI:continuous control      (cons (y-points (get-continuous-ctrl (first midiFiles) "FootController" nil 0 1)) ;y-points em CODE:Projects:1-BasicProjects:Classes:bpf.lisp            (multi-midi-continuous-ctrl(rest midiFiles)))))