

;*******************************************************************************************
;**********************  Vers„o recursiva **************************************************
;*******************************************************************************************
(defun tot-dur-rec (durs onsets)
    "Cálculo do tempo total de um objeto. Vide tese p.484 e o patch tot-dur2. É a soma do maior onset com o match dele na lista de durações."

    (let* ((maior-lista (max-lista (first onsets))))
      (if (= (length onsets) 1)
      (list
      (+ maior-lista (elt (first durs) (position maior-lista (first onsets) :from-end t))))
      (cons
      (+ maior-lista (elt (first durs) (position maior-lista (first onsets) :from-end t)))
      (tot-dur-rec (rest durs) (rest onsets))))))

;*******************************************************************************************


;*******************************************************************************************
;** nova versao para file duration corrigindo o erro no algoritmo da versao anterior. 
;** seguido o algoritmo criado por Didier. 
;** max, 06/02/2007.
;*******************************************************************************************
;******ALGORITIMO***************************************************************************
;in: O(lista de onsets), D(lista das duracoes)
; 
;f1 = O1 +D1, F = f1
;
;repete ate o fim da lista:
;se( On = On-1 ) 
;   se( Dn < Dn-1 ) fn = 0
;   senao fn = ( Dn - F)
;         F = F + fn 
;senao fn = ( On - F ) + Dn
;      F = F + fn
;      se ( fn < 0 ) fn = 0
;
;*******************************************************************************************

(defun tot-duration (onset duration)
 ; "Cálculo do tempo total de um objeto." 
  (setq b (length onset))
  (setq f-durs (+ (first onset) (first duration)))
  (setq soma-f f-durs)
  
  (loop while (> b 1) do  

        (if (= (second onset) (first onset))
          (if (< (second duration) (first duration))
            (setq f-durs 0 )
            (setq f-durs (- (second duration) soma-f )))
          (setq f-durs (+ (second duration) (- (second onset) soma-f))))

        (if (< f-durs 0)
          (setq f-durs 0)) 

        (setq soma-f (+ soma-f f-durs))

        (setq b (- b 1))  
        (setq onset (rest onset))
        (setq duration (rest duration)))
  soma-f)

;--------------------recursividade

(defun tot-duration-rec (onsets durations)
    "Cálculo do tempo total de um objeto."
  
      (if (= (length onsets) 1)
        (list (tot-duration (first onsets) (first durations)))
        (cons (tot-duration (first onsets) (first durations))
               (tot-duration-rec (rest onsets) (rest  durations))))
)
(defun relative-duration (onset duration)
  "pegar o maximo da lista de duracoes e dividir os valores das duracoes por esse maximo"
  (let* (( aux (tot-duration-rec onset duration )))
    (let* (( maior (list-max aux)))
      (if (= maior 0)
        0
      (g-round-list (om/ aux maior) 2)))))


;----------------------------
; Events-Density
;---------------------------- 

(defun t-density-rec1 (onsets-list file-duration smaller-impulse)
    "Returns the density linearity. FORMULA: N-EVENTS / (file-duration / smaller-impulse)."
    
    (let ((aux (* 
		  (/ 
		      (length (first onsets-list)) 
		      (/ file-duration smaller-impulse))
		  1.0)))
           
      (if ( = (length onsets-list) 1)
         (progn
            (if (> aux 1.0)
	       (list 1.0)
	       (list (g-round aux 2))))
         (progn
            (if (> aux 1.0)
	       (cons 1.0 (t-density-rec (rest onsets-list) file-duration smaller-impulse))
	       (cons (g-round aux 2) (t-density-rec (rest onsets-list) file-duration smaller-impulse)))))))

;;------------------------------------------------------------------------
 
;----------------------------
; Events-Density- com small e duration rec
;---------------------------- 

(defun t-density-rec (onsets-list file-duration smaller-impulse)
    "Returns the density linearity. FORMULA: N-EVENTS / (file-duration / smaller-impulse)."
    
    (let ((aux (* 
		  (/ 
		      (length (first onsets-list)) 
		      (/ (first file-duration) (first smaller-impulse)))
		  1.0)))
           
      (if ( = (length onsets-list) 1)
         (progn
            (if (> aux 1.0)
	       (list 1.0)
	       (list (g-round aux 2))))
         (progn
            (if (> aux 1.0)
	       (cons 1.0 (t-density-rec (rest onsets-list) (rest file-duration) (rest smaller-impulse)))
	       (cons (g-round aux 2) (t-density-rec (rest onsets-list) (rest file-duration) (rest smaller-impulse))))))))

;;------------------------------------------------------------------------
 
;----------------------------
; Events-Density
; Segundo algoritmo passado por Didier
; Funcoes que calculam a densidade segundo o seguinte algoritmo:
; meio = duracao total do arquivo/menor duracao
; retorna quantidade de eventos de onsets sem repeticao/meio
; Created by Max and Hildegard at 2007-08-10
; 
;---------------------------- 

(defun t-density-new-rec (onsets-list duration-list smaller-impulse)
    "Returns the density linearity. FORMULA: N-EVENTS / (file-duration / smaller-impulse)."
    
    (if (= (length onsets-list) 1) 
    (list (t-density-new  (first onsets-list) (first duration-list) smaller-impulse))
    (cons (t-density-new (first onsets-list) (first duration-list) smaller-impulse) (t-density-new-rec (rest onsets-list) (rest duration-list) smaller-impulse))

      )
)


(defun t-density-new (onsets-list duration-list smaller-impulse)
    "Returns the density linearity. FORMULA: N-EVENTS / (file-duration / smaller-impulse)."
    
    (if (= (length onsets-list) 1)
      (list 0)
      (if (= smaller-impulse 0)
      (list (g-round (/ (length (remove-duplicates onsets-list)) (/ (tot-duration onsets-list duration-list) (ppi onsets-list duration-list))) 2))
      (list (g-round (/ (length (remove-duplicates onsets-list)) (/ (tot-duration onsets-list duration-list) smaller-impulse)) 2)))))

;;------------------------------------------------------------------------

;----------------------------
;Relative-Span
;Criadas por Hildegard em 13/6/2008
;----------------------------

(defun t-span-relative-duration (onsets-list durations-list number)
  "Returns the relative duration in relation to the all files"
  
  (let* ((durations (tot-duration-rec onsets-list durations-list)) 
        (divisor (cond ((= number 0) (list-max durations))
                        (t number))))
    (divisao-de-uma-lista-por-um-numero durations divisor)
  )
)

(defun segunda-saida-relative-span (onsets-list durations-list)
  "retorna uma lista com ((a duracao do maior arquivo) (uma lista contendo as duracoes dos arquivos de entrada em sequencia))"
  (let* ((durations (tot-duration-rec onsets-list durations-list))
         (maior (list-max durations)))
    (list (list maior) (list durations))
    )
)


;----------------------------
; Time-Linearity
;---------------------------- 

(defun t-linearity-rec (onsets-list step use-remove-duplicates)
    "T-lin."
    (let* ((onsets-ordenado (ordem-crescente (cond ((= use-remove-duplicates 0) (first onsets-list))
                                                   (t (remove-duplicates (first onsets-list) :from-end t)))))
	   (tamanho-onsets  (length onsets-ordenado))
	   (dpl (* 
		   (if (= tamanho-onsets 1)
                     0
                     (/ 
		       (- (list-max onsets-ordenado)
			  (list-min onsets-ordenado))
		       (- (* tamanho-onsets step) step)))
		   step))
	   (ecmax-l (reduce #'+
		        (multiplicacao-de-uma-lista-por-um-numero 
			    (spliter 1 (- tamanho-onsets 2))
			    (- dpl step))))
	   (var-aux (reduce #'+
		       (lista-absoluta  
		          (mapcar #'- onsets-ordenado 
		               (dx->x-soal (list-min onsets-ordenado)
			              (make-list (- tamanho-onsets 1) :initial-element dpl)))))))
      
           (if (= (length onsets-list) 1)
              (list (g-round
	                (/ var-aux
	                   (teste-zero-retorna-1 ecmax-l))
	               2))
              (cons (g-round
	                (/ var-aux
	                   (teste-zero-retorna-1 ecmax-l))
	               2)
                    (t-linearity-rec (rest onsets-list) step use-remove-duplicates)))))

;;-----------------------------------------------------------

;----------------------------
; Pitch-Direction
;---------------------------- 

(defun p-direction-rec (midicents-list onsets-list)
  "PITCH-DIRECTION gives the object’s directional profile, i.e. the global direction (descending and/or ascending) of 
the object’s sequence of events. This outlines a kind of kynetic analysis, by observing the deviation rate between the 
higher and lower pitches of the first and last events, respectively. Both rates are divided by the actual range of the 
object. The more unidirectional the global pitch profile (as a scale-object would be), the higher the weight. The output 
can be relative, resulting in a negative value if the global direction is descending. But in most of cases, for analytic 
purposes, it is better to choose the absolute output option, as it is a better indicator of the rate, that is the 
strength, of the pitch-directionality. The formula is the following :
pd = {[(hZ-hA) + (gZ - gA)] / 2} / (H - G) 
where h and g are respectively the highest and lowest pitches of the object’s first (A) and last (Z) events, and H and G 
are respectively the highest and lowest pitches of the whole object."

  (let* ((notas-por-onset (notesByOnset (first midicents-list) (first onsets-list)))
         (hZ (list-max (second (first (last notas-por-onset)))))
         (hA (list-max (second (first notas-por-onset))))
         (gZ (list-min (second (first (last notas-por-onset)))))
         (gA (list-max (second (first notas-por-onset))))
         (H (list-max (first midicents-list)))
         (G (list-min (first midicents-list))))

    (if (= (length midicents-list) 1)
       (list (g-round (* 
                       (if (= H G)
                         0
                       (/ (/ (+ (- hZ hA) 
                                  (- gZ gA)) 
                               2) 
                            (- H G)))
                         1.0)
                      2))
       (cons (g-round (* 
                       (if (= H G)
                         0
                       (/ (/ (+ (- hZ hA) 
                                  (- gZ gA)) 
                               2) 
                            (- H G)))
                         1.0)
                      2)
             (p-direction-rec (rest midicents-list) (rest onsets-list))))))
              

;;-------------------------------------------------------

;----------------------------
; Amplitude-Deviation
;---------------------------- 

(defun v-env-rec (velocities)
  "Applies to MIDI velocities basically the same algorithm as for PITCH-DEVIATION, but it is added an evaluation of 
the direction and slope rate of the attack and decay curves. This is obtained by measuring the velocity differences 
between the two first (attack) and two last (decay) events. The stronger the attack portion of the file (e.g. sfp), 
the higher the positive value of the attack algorithm. The softer this attack, the higher the negative value. And 
reversely for the decay. Then the absolute sum of this two data is added to the relative deviation global value."
  (let* ((list-vel (first velocities))
         (dev (first (deviations-rel list-vel :use-reference 1 :reference 4 :value 0 :min 1 :max 127)))
         (att (attack list-vel))
         (ext (extction list-vel)))
    (if (= (length velocities) 1)
        (if (= (length list-vel) 1) 
            (list (list 0))
            (list (list
                   dev
                   (x-append att ext)
                   (g-round (+ (abs dev) (/ (sqrt (+ (abs att) (abs ext))) 10)) 2))))
        (if (= (length list-vel) 1)
            (cons (list 0) (v-env-rec (rest velocities)))
            (cons  (list
                    dev
                   (x-append att ext)
                   (g-round (+ (abs dev) (/ (sqrt (+ (abs att) (abs ext))) 10)) 2))  
                   (v-env-rec (rest velocities))))))) 
        
   
(defun LastValueRec (velocities)
  (let* ((list-vel (first velocities))
           (dev (first (deviations-rel list-vel :use-reference 1 :reference 4 :value 0 :min 1 :max 127)))
           (att (attack list-vel))
           (ext (extction list-vel)))
      (if (= (length velocities) 1)    
          (if (= (length list-vel) 1) 
              (list 0)
              (list (list (g-round (+ (abs dev) (/ (sqrt (+ (abs att) (abs ext))) 10)) 2)))) 
          (if (= (length list-vel) 1) 
              (cons 0 (LastValueRec (rest velocities)))
              (cons (list (g-round (+ (abs dev) (/ (sqrt (+ (abs att) (abs ext))) 10)) 2)) 
                    (LastValueRec (rest velocities)))))))    
                      
;;------------------------------------------------------------------------

(defun avt-dernier (lista)
  "Retorna o penultimo elemento da lista"
  (if (= (length lista) 1)
    0
  (first (last (butlast lista)))))

(defun extction (lista)
  ""
  (g-round 
   (/ (* (- (avt-dernier lista) (first (last lista))) 
         -1) 
      252.0) 
   2))

(defun attack (lista)
  ""
  (if (= (length lista) 1)
    0
  (g-round (/ (- (first lista) (second lista))
              252.0) 
           2)))


;--------------------------
; funcoes usadas em harmonicity-per-onset
;--------------------------

(defun harmonicity-rel-per-onset (notes-onsets range use-virtual-fundamental? user-fundamental threshold round-harmonic-object? transpose-pitches? remove-duplicates?)
  (primeira-saida-harmonicity (last-elem (mat-trans notes-onsets)) :listout 1
                                      :range range
                                      :use-virtual-fundamental use-virtual-fundamental?
                                      :user-fundamental user-fundamental
                                      :threshold threshold
                                      :use-round-midicents round-harmonic-object?
                                      :use-transpoct transpose-pitches?
                                      :use-remove-duplicates remove-duplicates?))

(defun harmonicity-rel-per-onset-rec (notes-onsets range use-virtual-fundamental? user-fundamental threshold round-harmonic-object? transpose-pitches? remove-duplicates?)
    (if (= (length notes-onsets) 1)
         (list (harmonicity-rel-per-onset (first notes-onsets) range use-virtual-fundamental? user-fundamental threshold round-harmonic-object? transpose-pitches? remove-duplicates? ))
          (cons (harmonicity-rel-per-onset (first notes-onsets) range use-virtual-fundamental? user-fundamental threshold round-harmonic-object? transpose-pitches? remove-duplicates?)
                 (harmonicity-rel-per-onset-rec (rest notes-onsets) range use-virtual-fundamental? user-fundamental threshold round-harmonic-object? transpose-pitches? remove-duplicates?))
      )
)

(defun deviation-per-onset (notes-onsets range use-virtual-fundamental? user-fundamental threshold round-harmonic-object? transpose-pitches? remove-duplicates?)
  (segunda-saida-harmonicity (last-elem (mat-trans notes-onsets)) :range range
                                      :use-virtual-fundamental use-virtual-fundamental?
                                      :user-fundamental user-fundamental
                                      :threshold threshold
                                      :use-round-midicents round-harmonic-object?
                                      :use-transpoct transpose-pitches?
                                      :use-remove-duplicates remove-duplicates?)
)

(defun deviation-per-onset-rec (notes-onsets range use-virtual-fundamental? user-fundamental threshold round-harmonic-object? transpose-pitches? remove-duplicates?)
  (if (= (length notes-onsets) 1)
         (list (deviation-per-onset (first notes-onsets) range use-virtual-fundamental? user-fundamental threshold round-harmonic-object? transpose-pitches? remove-duplicates? ))
          (cons (deviation-per-onset (first notes-onsets) range use-virtual-fundamental? user-fundamental threshold round-harmonic-object? transpose-pitches? remove-duplicates?)
                 (deviation-per-onset-rec (rest notes-onsets) range use-virtual-fundamental? user-fundamental threshold round-harmonic-object? transpose-pitches? remove-duplicates?))
))

(defun weight-per-onset (notes-onsets range use-virtual-fundamental? user-fundamental threshold round-harmonic-object? remove-duplicates?)
(terceira-saida-harmonicity (last-elem (mat-trans notes-onsets)) :listout 1
                                      :range range
                                      :use-virtual-fundamental use-virtual-fundamental?
                                      :user-fundamental user-fundamental
                                      :threshold threshold
                                      :use-round-midicents round-harmonic-object?
                                      :use-remove-duplicates remove-duplicates?)
)

(defun weight-per-onset-rec (notes-onsets range use-virtual-fundamental? user-fundamental threshold round-harmonic-object? remove-duplicates?)
(if (= (length notes-onsets) 1)
         (list (weight-per-onset (first notes-onsets) range use-virtual-fundamental? user-fundamental threshold round-harmonic-object? remove-duplicates? ))
          (cons (weight-per-onset (first notes-onsets) range use-virtual-fundamental? user-fundamental threshold round-harmonic-object? remove-duplicates?)
                 (weight-per-onset-rec (rest notes-onsets) range use-virtual-fundamental? user-fundamental threshold round-harmonic-object? remove-duplicates?))
))

(defun fundamental-per-onset (notes-onsets use-virtual-fundamental? user-fundamental threshold remove-duplicates?)
(quarta-saida-harmonicity  (last-elem (mat-trans notes-onsets)) :use-virtual-fundamental  use-virtual-fundamental?
                                      :user-fundamental user-fundamental
                                      :threshold  threshold
                                      :use-remove-duplicates  remove-duplicates?)
)

(defun fundamental-per-onset-rec (notes-onsets use-virtual-fundamental? user-fundamental threshold remove-duplicates?)
(if (= (length notes-onsets) 1)
         (list (fundamental-per-onset (first notes-onsets) use-virtual-fundamental? user-fundamental threshold remove-duplicates? ))
          (cons (fundamental-per-onset (first notes-onsets) use-virtual-fundamental? user-fundamental threshold remove-duplicates?)
                 (fundamental-per-onset-rec (rest notes-onsets) use-virtual-fundamental? user-fundamental threshold remove-duplicates?)))
)

(defun coincidents-harmonics-per-onset (notes-onsets range use-virtual-fundamental? user-fundamental threshold round-harmonic-object? remove-duplicates?)
(quinta-saida-harmonicity (last-elem (mat-trans notes-onsets)) :listout 1
                                     :range range
                                     :use-virtual-fundamental  use-virtual-fundamental?
                                     :user-fundamental user-fundamental
                                     :threshold  threshold
                                     :use-round-midicents round-harmonic-object?
                                     :use-remove-duplicates  remove-duplicates?)
)

(defun coincidents-harmonics-per-onset-rec (notes-onsets range use-virtual-fundamental? user-fundamental threshold round-harmonic-object? remove-duplicates?)
(if (= (length notes-onsets) 1)
         (list (coincidents-harmonics-per-onset (first notes-onsets) range use-virtual-fundamental? user-fundamental threshold round-harmonic-object? remove-duplicates? ))
          (cons (coincidents-harmonics-per-onset (first notes-onsets) range use-virtual-fundamental? user-fundamental threshold round-harmonic-object? remove-duplicates?)
                 (coincidents-harmonics-per-onset-rec (rest notes-onsets) range use-virtual-fundamental? user-fundamental threshold round-harmonic-object? remove-duplicates?)))
)

;--------------------------
; funcoes usadas em cognitive-sonance-per-onset
;--------------------------

(defun entrada-sonance-per-onset (notes-onsets)
  (if (= (length notes-onsets) 1)
  (list (entrada-sonance (last-elem (mat-trans (first notes-onsets)))))
    (cons (entrada-sonance (last-elem (mat-trans (first notes-onsets))))
          (entrada-sonance-per-onset (rest notes-onsets))))
)

(defun intervalos-sonance-per-onset (entrada)
  (if (= (length entrada) 1)
  (list (intervalos-sonance (first entrada)))
    (cons (intervalos-sonance (first entrada))
          (intervalos-sonance-per-onset (rest entrada))))
)

(defun oitavas-sonance-per-onset (entrada intervalos)
  (if (= (length entrada) 1)
  (list (oitavas-sonance (first entrada) (first intervalos)))
    (cons (oitavas-sonance (first entrada) (first intervalos))
          (oitavas-sonance-per-onset (rest entrada) (rest intervalos))))
)

(defun quarto-if-aninhado-sonance-per-onset (tabela tabela-oitavas oitavas intervalos)
  (if (= (length oitavas) 1)
  (list (quarto-if-aninhado-sonance tabela tabela-oitavas (first oitavas) (first intervalos)))
    (cons (quarto-if-aninhado-sonance tabela tabela-oitavas (first oitavas) (first intervalos))
          (quarto-if-aninhado-sonance-per-onset tabela tabela-oitavas (rest oitavas) (rest intervalos)))
))

(defun terceiro-if-aninhado-sonance-per-onset (tabela tabela-oitavas oitavas intervalos quartoifaninhado)
  (if (= (length oitavas) 1)
  (list (terceiro-if-aninhado-sonance-per-onset-aux tabela tabela-oitavas (first oitavas) (first intervalos) (first quartoifaninhado)))
    (cons (terceiro-if-aninhado-sonance-per-onset-aux tabela tabela-oitavas (first oitavas) (first intervalos) (first quartoifaninhado))
          (terceiro-if-aninhado-sonance-per-onset tabela tabela-oitavas (rest oitavas) (rest intervalos) (rest quartoifaninhado)))
))

(defun terceiro-if-aninhado-sonance-per-onset-aux (tabela tabela-oitavas oitavas intervalos quartoifaninhado)
  (if (= (length oitavas) 1)
  (list (terceiro-if-aninhado-sonance-rec tabela tabela-oitavas (first oitavas) (first intervalos) (first quartoifaninhado)))
    (cons (terceiro-if-aninhado-sonance-rec tabela tabela-oitavas (first oitavas) (first intervalos) (first quartoifaninhado))
          (terceiro-if-aninhado-sonance-per-onset-aux tabela tabela-oitavas (rest oitavas) (rest intervalos) (rest quartoifaninhado)))
))

(defun segundo-if-aninhado-sonance-per-onset (tabela tabela-oitavas oitavas intervalos terceiroifaninhado)
  (if (= (length oitavas) 1)
  (list (segundo-if-aninhado-sonance-per-onset-aux tabela tabela-oitavas (first oitavas) (first intervalos) (first terceiroifaninhado)))
    (cons (segundo-if-aninhado-sonance-per-onset-aux tabela tabela-oitavas (first oitavas) (first intervalos) (first terceiroifaninhado))
          (segundo-if-aninhado-sonance-per-onset tabela tabela-oitavas (rest oitavas) (rest intervalos) (rest terceiroifaninhado)))
))

(defun segundo-if-aninhado-sonance-per-onset-aux (tabela tabela-oitavas oitavas intervalos terceiroifaninhado)
  (if (= (length oitavas) 1)
  (list (segundo-if-aninhado-sonance-rec tabela tabela-oitavas (first oitavas) (first intervalos) (first terceiroifaninhado)))
    (cons (segundo-if-aninhado-sonance-rec tabela tabela-oitavas (first oitavas) (first intervalos) (first terceiroifaninhado))
          (segundo-if-aninhado-sonance-per-onset-aux tabela tabela-oitavas (rest oitavas) (rest intervalos) (rest terceiroifaninhado)))
))

(defun primeiro-if-aninhado-sonance-per-onset (tabela tabela-oitavas oitavas intervalos segundoifaninhado)
  (if (= (length oitavas) 1)
  (list (primeiro-if-aninhado-sonance-per-onset-aux tabela tabela-oitavas (first oitavas) (first intervalos) (first segundoifaninhado)))
    (cons (primeiro-if-aninhado-sonance-per-onset-aux tabela tabela-oitavas (first oitavas) (first intervalos) (first segundoifaninhado))
          (primeiro-if-aninhado-sonance-per-onset tabela tabela-oitavas (rest oitavas) (rest intervalos) (rest segundoifaninhado)))
))

(defun primeiro-if-aninhado-sonance-per-onset-aux (tabela tabela-oitavas oitavas intervalos segundoifaninhado)
  (if (= (length oitavas) 1)
  (list (primeiro-if-aninhado-sonance-rec tabela tabela-oitavas (first oitavas) (first intervalos) (first segundoifaninhado)))
    (cons (primeiro-if-aninhado-sonance-rec tabela tabela-oitavas (first oitavas) (first intervalos) (first segundoifaninhado))
          (primeiro-if-aninhado-sonance-per-onset-aux tabela tabela-oitavas (rest oitavas) (rest intervalos) (rest segundoifaninhado)))
))

(defun media-lista-de-listas-per-onset (lista)
  (if (= (length lista) 1)
      (list (medialistadelistas (first lista)))
    (cons (medialistadelistas (first lista))
          (media-lista-de-listas-per-onset (rest lista))))
)

;--------------------------
; funcoes usadas em spatial-linearity-per-onset
;--------------------------

(defun s-linearity-rec-per-onset (notes-onsets use-remove-duplicates)
  (if (= (length notes-onsets) 1)
      (list (s-linearity-rec-per-onset-aux (last-elem (mat-trans (first notes-onsets))) use-remove-duplicates))
    (cons (s-linearity-rec-per-onset-aux (last-elem (mat-trans (first notes-onsets))) use-remove-duplicates)
          (s-linearity-rec-per-onset (rest notes-onsets) use-remove-duplicates)))
)

(defun s-linearity-rec-per-onset-aux (notes-onsets use-remove-duplicates)
  (if (= (length notes-onsets) 1)
      (list (s-linearity-rec notes-onsets :use-remove-duplicates use-remove-duplicates))
    (cons (s-linearity-rec notes-onsets :use-remove-duplicates use-remove-duplicates)
          (s-linearity-rec-per-onset-aux (rest notes-onsets) use-remove-duplicates))))

(defun itv-paradigm-rec-per-onset (notes-onsets)
  (if (= (length notes-onsets) 1)
      (list (itv-paradigm-rec (last-elem (mat-trans (first notes-onsets))) 100))
    (cons (itv-paradigm-rec (last-elem (mat-trans (first notes-onsets))) 100)
          (itv-paradigm-rec-per-onset (rest notes-onsets))))
)

;----------------------------
; density by onset relative
;---------------------------- CRIAR UMA NOVA FUNCAO QUE EXTRAIA A DENSIDADE ABS E RELAT DE NOTES-BY-ONSET - MAX


;------------------patch: dens-by-onset----------------------------
(defun density-by-onset (notes-onsets)    
"relative density by onset. relative density vector"
      ;(if (= (length (second (first (last-elem (mat-trans notes-onsets))))) 1)
       ;(list 0)
      ( s-density-unit-rec (last-elem (mat-trans notes-onsets)) :use-remove-duplicates 1))
;)
       

(defun density-rel-by-onset-rec (notes-onsets)
    
  
     (if (= (length notes-onsets) 1)
       
          (list (density-by-onset (first notes-onsets)  ))
          (cons (density-by-onset (first notes-onsets))
                 (density-rel-by-onset-rec (rest notes-onsets)))
      )
)

;------------------patch: avarage-dens-by-onset----------------------------

(defun average-density-rel-by-onset (dens-onsets)
 "calcula a media da densidade relativa por onset. "

     ;(g-round (media (density-rel-by-onset dens-onsets) ) 2)
      (g-round (media (density-by-onset dens-onsets) ) 2)
  
)

(defun average-density-rel-by-onset-rec (dens-onsets)
    
     (if (= (length dens-onsets) 1)

          (list (average-density-rel-by-onset (first dens-onsets)  ))
          (cons (average-density-rel-by-onset (first dens-onsets))
                 (average-density-rel-by-onset-rec (rest dens-onsets)))
      )
)
;------------------patch: avarage-dens-by-onset----------------------------
(defun weighted-av-dens-rel-by-onset (av-dens-onsets)

     (let* ((aux (average-density-rel-by-onset-rec av-dens-onsets))
           (maximo (max-lista aux)))
       (if (= maximo 0)
         0
     (g-round-list (om/ aux maximo) 2)))

)

;==============density by onset absolute

;------------------patch: dens-by-onset----------------------------

(defun density-abs-onset (number-onsets)
"calcula a densidade absoluta dos onsets de uma lista"
       (if (= (length number-onsets) 1)
         (list (list 0))
       (last-elem (mat-trans number-onsets)))
)

(defun density-abs-onset-rec (number-onsets)
"calcula a densidade absoluta dos onsets recursivo"
     (if (= (length number-onsets) 1)

          (list (density-abs-onset (first number-onsets)  ))
          (cons (density-abs-onset (first number-onsets))
                 (density-abs-onset-rec (rest number-onsets)))
     )
)

;------------------patch: avarage-dens-by-onset----------------------------

(defun average-density-abs-onset-rec (number-onsets)
"calcula media da densidade absoluta dos onsets "
     (if (= (length number-onsets) 1)

          (list (g-round (media (listadelistasdenumeros->listadenumeros (density-abs-onset (first number-onsets)))) 2) )
          (cons (g-round (media (listadelistasdenumeros->listadenumeros (density-abs-onset (first number-onsets)))) 2)
                 (average-density-abs-onset-rec (rest number-onsets)))
     )
)

;------------------patch: avarage-dens-by-onset----------------------------

(defun weighted-av-dens-abs-onset (av-dens-onsets)

     (let* ((aux (average-density-abs-onset-rec av-dens-onsets))
           (maximo (max-lista aux)))
       (if (= maximo 0)
         0
     (g-round-list (om/ aux maximo) 2)))

)

;---------------------------------------------------------------------
;usadas na funcao segmentation-midi
;---------------------------------------------------------------------

(defun organiza (lista)
  (organiza-aux lista '() '() '() '())
)

(defun organiza-aux (lista midicents onsets durations velocities)
  (if (= (length lista) 1)
    (list (concatenate 'list midicents (first (first lista))) (concatenate 'list onsets (second (first lista))) (concatenate 'list durations (third (first lista))) (concatenate 'list velocities (fourth (first lista))))
    (let ((midicents (first (first lista)))
          (onsets (second (first lista)))
          (durations (third (first lista)))
          (velocities (fourth (first lista))))
          
      (organiza-aux (rest lista) midicents onsets durations velocities)))
)

(defun percorre-varias (midicents onsets durations velocities segmentador)
  (if (= (length midicents) 1)
      (list (percorre (first midicents) (first onsets) (first durations) (first velocities) segmentador))
    (cons (percorre (first midicents) (first onsets) (first durations) (first velocities) segmentador)
          (percorre-varias (rest midicents) (rest onsets) (rest durations) (rest velocities) segmentador)))
)

(defun percorre (midicents onsets durations velocities segmentador)
  (cond ((or (null midicents) (null onsets) (null durations) (null velocities)) nil)
        ((= (length midicents) 1) (percorre-um-aux midicents onsets durations velocities segmentador '() '() '() '()))
  	(t (percorre-aux midicents onsets durations velocities segmentador '() '() '() '() '() '() '() '() 0 (first onsets))))
)

(defun percorre-aux (midicents onsets durations velocities segmentador midicents-atual onsets-atual durations-atual velocities-atual midicents-final onsets-final durations-final velocities-final soma first-onset)
  (cond ((or (null midicents) (null onsets) (null durations) (null velocities))
         (if (and (null midicents-atual) (null onsets-atual) (null durations-atual) (null velocities-atual))
             (list midicents-final onsets-final durations-final velocities-final)
           (list (insert-element midicents-atual midicents-final) 
           	(insert-element onsets-atual onsets-final) (insert-element durations-atual durations-final) 
           	(insert-element velocities-atual velocities-final))))
        
        ((> (+ soma (first durations)) segmentador)
        (percorre-aux midicents onsets (insere-inicio (rest durations) (- (first durations) (- segmentador soma)))  
         velocities segmentador '() '() '() '() 
         (insert-element (insert-element (first midicents) midicents-atual) midicents-final) 
         (insert-element (insert-element (- (first onsets) first-onset) onsets-atual) onsets-final) 
         (insert-element (insert-element (- segmentador soma) durations-atual) durations-final) 
         (insert-element (insert-element (first velocities) velocities-atual) velocities-final) 
         0 (first onsets)))
        
        ((= (+ soma (first durations)) segmentador)
        (if (null midicents-atual) 
        (percorre-aux (rest midicents) (rest onsets) (rest durations) (rest velocities) segmentador  '() '() '() '() 
        (insert-element (list (first midicents)) midicents-final) (insert-element (list (first onsets)) onsets-final) 
        (insert-element (list (first durations)) durations-final) (insert-element (list (first velocities)) velocities-final) 
        0 (first onsets))
         (percorre-aux (rest midicents) (rest onsets) (rest durations) (rest velocities) segmentador  '() '() '() '() 
         (insert-element (insert-element (first midicents) midicents-atual) midicents-final) 
         (insert-element (insert-element (- (first onsets) first-onset) onsets-atual) onsets-final) 
         (insert-element (insert-element (first durations) durations-atual) durations-final) 
         (insert-element (insert-element (first velocities) velocities-atual) velocities-final) 0 (first onsets))))
        
        (t (percorre-aux (rest midicents) (rest onsets) (rest durations) (rest velocities) segmentador 
        (insert-element (first midicents) midicents-atual) (insert-element (- (first onsets) first-onset) onsets-atual) 
        (insert-element (first durations) durations-atual) (insert-element (first velocities) velocities-atual) 
        midicents-final onsets-final durations-final velocities-final (+ soma (first durations)) first-onset)))
)

(defun percorre-um-aux (midicents onsets durations velocities segmentador midicents-final onsets-final durations-final velocities-final)
  (if (> (first durations) segmentador) 
      (percorre-aux midicents (list 0) (list (- (first durations) segmentador)) velocities segmentador '() '() '() '() 
                    (insert-element midicents midicents-final) (insert-element (list 0) onsets-final) 
                    (insert-element (list segmentador) durations-final) 
                    (insert-element velocities velocities-final) 0 0)
    (if (= (first durations) segmentador) 
        (list (insert-element midicents midicents-final) (insert-element (list 0) onsets-final) 
              (insert-element (list segmentador) durations-final) (insert-element velocities velocities-final))
      (list (insert-element midicents midicents-final) (insert-element (list 0) onsets-final) 
            (insert-element durations durations-final) (insert-element velocities velocities-final))
		)
	)
)