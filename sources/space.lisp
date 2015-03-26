
;*********************************   Funcoes Recusivas ************************************
;******************************************************************************************

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;DIACHORNIC ANALYSIS
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;----------------------------
; Time-Linearity
;----------------------------


(defun itv-paradigm-rec (midicents-list step)
    "Cálculo do intervalo paradigmático. Cf. tese p. 477."
    (if (= (length midicents-list) 1)
      (if (= (length (first midicents-list)) 1)
        (list 0)
      (list (om-round (* (* 
                          (if (= (length (first midicents-list)) 1)
                           0
                          (/ 
                          (soalambitus (first midicents-list)) 
                          (- (* (length (first midicents-list)) 
                                step) 
                             step))) 
                       step) 
                    1.0) 2)))
     (cons (om-round (* (* 
                         (if (= (length (first midicents-list)) 1)
                           0
                         (/ 
                        (soalambitus (first midicents-list)) 
                        (- (* (length (first midicents-list)) 
                              step) 
                           step))) 
                      step) 
                   1.0) 2) 
              (itv-paradigm-rec (rest midicents-list) step))))

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;ACHORNIC ANALYSIS
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;********************************************************************************************************
;**** FunÁıes especiais recursivas usadas para spatial-analisys *****************************************
;----------------------------------------------------------------------------------
;FunÁıes feitas por AndrÈ para a funÁ„o  spatial-analysis recursivo
(defun primeiro-par-spatial-analysis (midicents range registers register-weight remove-duplicates? )

     (let* ((midicents-aux (ordem-crescente (cond ((= remove-duplicates? 0) (first midicents))
	                                   (t (remove-duplicates (first midicents) :from-end t))))))

       (if (= (length midicents) 1)
       (list (list
             (soalambitus-relative  midicents-aux range)
             (soalambitus  midicents-aux)
             (registration-general  midicents-aux registers)
             (register-weight-general  midicents-aux registers register-weight)
             (asq-general  midicents-aux range registers register-weight)))
       (cons (list
             (soalambitus-relative  midicents-aux range)
             (soalambitus  midicents-aux)
             (registration-general  midicents-aux registers)
             (register-weight-general  midicents-aux registers register-weight)
             (asq-general  midicents-aux range registers register-weight))
           (primeiro-par-spatial-analysis (rest midicents) range registers register-weight remove-duplicates?)))))

(defun segundo-par-spatial-analysis (midicents range registers register-weight remove-duplicates? )
    (let* ((midicents-aux (ordem-crescente (cond ((= remove-duplicates? 0) (first midicents))
	                                   (t (remove-duplicates (first midicents) :from-end t))))))
           (if (= (length midicents) 1)
             (list (asq-general  midicents-aux range registers register-weight))
             (cons (asq-general  midicents-aux range registers register-weight)
                   (segundo-par-spatial-analysis (rest midicents) range registers register-weight remove-duplicates? )))))

(defun terceiro-par-spatial-analysis (midicents range remove-duplicates?)
    (let* ((midicents-aux (ordem-crescente (cond ((= remove-duplicates? 0) (first midicents))
	                                   (t (remove-duplicates (first midicents) :from-end t))))))
           (if (= (length midicents) 1)
             (list (soalambitus-relative  midicents-aux range))
             (cons (soalambitus-relative  midicents-aux range)
                   (terceiro-par-spatial-analysis (rest midicents) range remove-duplicates? )))))

(defun quarto-par-spatial-analysis (midicents  remove-duplicates?)
    (let* ((midicents-aux (ordem-crescente (cond ((= remove-duplicates? 0) (first midicents))
	                                   (t (remove-duplicates (first midicents) :from-end t))))))
           (if (= (length midicents) 1)
             (list (soalambitus  midicents-aux))
             (cons (soalambitus  midicents-aux)
                   (quarto-par-spatial-analysis (rest midicents)  remove-duplicates? )))))

(defun quinto-par-spatial-analysis (midicents registers remove-duplicates?)
    (let* ((midicents-aux (ordem-crescente (cond ((= remove-duplicates? 0) (first midicents))
	                                   (t (remove-duplicates (first midicents) :from-end t))))))
           (if (= (length midicents) 1)
             (list (register-fill-number  midicents-aux registers))
             (cons (register-fill-number  midicents-aux registers)
                   (quinto-par-spatial-analysis (rest midicents) registers remove-duplicates? )))))

(defun sexto-par-spatial-analysis (midicents registers remove-duplicates?)
    (let* ((midicents-aux (ordem-crescente (cond ((= remove-duplicates? 0) (first midicents))
	                                   (t (remove-duplicates (first midicents) :from-end t))))))
           (if (= (length midicents) 1)
             (list (registration-general  midicents-aux registers))
             (cons (registration-general  midicents-aux registers)
                   (sexto-par-spatial-analysis (rest midicents) registers remove-duplicates? )))))

(defun setimo-par-spatial-analysis (midicents registers remove-duplicates?)
    (let* ((midicents-aux (ordem-crescente (cond ((= remove-duplicates? 0) (first midicents))
	                                   (t (remove-duplicates (first midicents) :from-end t))))))
           (if (= (length midicents) 1)
             (list (register-fill  midicents-aux registers))
             (cons (register-fill  midicents-aux registers)
                   (setimo-par-spatial-analysis (rest midicents) registers remove-duplicates? ))))) 
 
(defun setimo-par-piano-spatial-analysis (midicents registers remove-duplicates?)
    (if (= (length midicents) 1)
             (list (subtracao-de-uma-lista-por-um-numero (first (setimo-par-spatial-analysis (list (first midicents)) registers remove-duplicates?)) 3))
             (cons (subtracao-de-uma-lista-por-um-numero (first (setimo-par-spatial-analysis (list (first midicents)) registers remove-duplicates?)) 3)
                   (setimo-par-piano-spatial-analysis (rest midicents) registers remove-duplicates?)))) 
 
(defun oitavo-par-spatial-analysis (midicents registers remove-duplicates?)
    (let* ((midicents-aux (ordem-crescente (cond ((= remove-duplicates? 0) (first midicents))
	                                   (t (remove-duplicates (first midicents) :from-end t))))))
           (if (= (length midicents) 1)
             (list (low-bound-general  midicents-aux registers))
             (cons (low-bound-general  midicents-aux registers)
                   (oitavo-par-spatial-analysis (rest midicents) registers remove-duplicates? )))))

(defun oitavo-par-piano-spatial-analysis (midicents registers remove-duplicates?) ;copia da oitavo-par-spatial-analysis 
    (let* ((midicents-aux (ordem-crescente (cond ((= remove-duplicates? 0) (first midicents))
	                                   (t (remove-duplicates (first midicents) :from-end t))))))
           (if (= (length midicents) 1)
             (list (- (low-bound-general  midicents-aux registers) 3)) ;difere nesta subitracao
             (cons (- (low-bound-general  midicents-aux registers) 3)
                   (oitavo-par-spatial-analysis (rest midicents) registers remove-duplicates? )))))

(defun oitavo-par-piano-spatial-analysis2 (setimo-par)
     (if (= (length setimo-par) 1)
       (list (first (first setimo-par)))
       (cons (first (first setimo-par)) (oitavo-par-piano-spatial-analysis2 (rest setimo-par)))))

(defun nono-par-spatial-analysis (midicents registers remove-duplicates?)
    (let* ((midicents-aux (ordem-crescente (cond ((= remove-duplicates? 0) (first midicents))
	                                   (t (remove-duplicates (first midicents) :from-end t))))))
           (if (= (length midicents) 1)
             (list (high-bound-general  midicents-aux registers))
             (cons (high-bound-general  midicents-aux registers)
                   (nono-par-spatial-analysis (rest midicents) registers remove-duplicates? )))))

(defun nono-par-piano-spatial-analysis (midicents registers remove-duplicates?) ; copia da nono-par-piano-spatial-analisys
    (let* ((midicents-aux (ordem-crescente (cond ((= remove-duplicates? 0) (first midicents))
	                                   (t (remove-duplicates (first midicents) :from-end t))))))
           (if (= (length midicents) 1)
             (list (- (high-bound-general  midicents-aux registers) 3));difere nesta subitracao
             (cons (- (high-bound-general  midicents-aux registers) 3)
                   (nono-par-spatial-analysis (rest midicents) registers remove-duplicates? )))))

(defun nono-par-piano-spatial-analysis2 (setimo-par)
     (if (= (length setimo-par) 1)
       (last (first setimo-par))
       (cons (first (last (first setimo-par))) (nono-par-piano-spatial-analysis2 (rest setimo-par))))) 

(defun decimo-par-spatial-analysis (midicents registers register-weight remove-duplicates?)
    (let* ((midicents-aux (ordem-crescente (cond ((= remove-duplicates? 0) (first midicents))
	                                   (t (remove-duplicates (first midicents) :from-end t))))))
           (if (= (length midicents) 1)
             (list (register-weight-general  midicents-aux registers register-weight))
             (cons (register-weight-general  midicents-aux registers register-weight)
                   (decimo-par-spatial-analysis (rest midicents) registers register-weight remove-duplicates? )))))

;-------------------------------------------------------------------------------------------



;--------------------------------
;  Spatial Fillig Analysis
;--------------------------------


;****************************************************************************************
;********************   versao recursiva s-density-rec  *********************************
;****************************************************************************************
;Vers„o Recursiva adaptada por AndrÈ

(defun s-density-rec (midicents-list &key (use-remove-duplicates 1))
    "Cálculo da densidade espacial."
    (let ((midicents-aux (ordem-crescente (cond ((= use-remove-duplicates 0) (first midicents-list))
                                             (t (remove-duplicates (first midicents-list) :from-end t))))))

     (if (= (length midicents-list) 1)
         (list (g-round
	        (*
	         (/
		   (length midicents-aux)
		   (/
		        (+ (soalambitus midicents-aux) 100)
		         100))
	             1.0)
	         2))
         (cons (g-round
	        (*
	         (/
		   (length midicents-aux)
		   (/
		        (+ (soalambitus midicents-aux) 100)
		         100))
	             1.0)
	         2)

            (s-density-rec (rest midicents-list) :use-remove-duplicates use-remove-duplicates )))))

;---------------------------------------------------------------------------
;density-onset
;Uma adaptacao que verifica se o tamanho da lista de notas e 1, para que caso seja, essa funcao retorne 0 como densidade.
;
;Max and Hildegard in 21/8/2007
;------------------------------------------------------------------------- 
(defun s-density-unit-rec (midicents-list &key (use-remove-duplicates 1))
    "Calculo da densidade espacial."
    (let ((midicents-aux (ordem-crescente (cond ((= use-remove-duplicates 0) (first midicents-list))
                                             (t (remove-duplicates (first midicents-list) :from-end t))))))

     (if (= (length midicents-list) 1)
       (list  (if (= (length (first midicents-list)) 1)
                0
               (g-round
                (*
                 (/
                  (length midicents-aux)
                  (/
                   (+ (soalambitus midicents-aux) 100)
                   100))
                 1.0)
                2)))
       (cons  (if (= (length (first midicents-list)) 1)
                0
               (g-round
                (*
                 (/
                  (length midicents-aux)
                  (/
                   (+ (soalambitus midicents-aux) 100)
                   100))
                 1.0)
                2))
       (s-density-unit-rec (rest midicents-list) :use-remove-duplicates use-remove-duplicates )))))

;; ---------------------------------------------------------------------
;Created by Max and Hildegard in 2007-08-30
(defun s-number-density (notes &key (use-remove-duplicates 1))
  "Calculo da densidade por Marcilio"
  
  (if (= (length notes) 1)
    0
  (g-round (/ (length notes) (/ (soalambitus notes) 100)) 2)))

(defun s-number-density-rec (notes &key (use-remove-duplicates 1))
  "Versao recursiva da s-number-density"

 
  (if (= (length notes) 1)
    (list (s-number-density (first notes) :use-remove-duplicates use-remove-duplicates))
    (cons (s-number-density (first notes) :use-remove-duplicates use-remove-duplicates)
          (s-number-density-rec (rest notes) :use-remove-duplicates use-remove-duplicates))

    )
)



;; ---------------------------------------------------------------------
;**********************************************************************************************

;*************************************************************************************
;********************   vers„o recursiva *********************************************
;*************************************************************************************
;Vers„o recursiva adaptada por AndrÈ
(defun s-linearity-rec (midicents-list &key (step 100) (use-remove-duplicates 1))
   "Cálculo da linearidade espacial."
(if (null midicents-list) nil 
(let* ((midicents-aux (ordem-crescente (cond ((= use-remove-duplicates 0) (first midicents-list))
                                                 (t (remove-duplicates (first midicents-list) :from-end t)))))
           (pre-lin (reduce #'+ (mapcar #'abs (mapcar #'- midicents-aux (if (= (length (first midicents-list)) 1) midicents-aux (paradigm-list-constructor midicents-aux step))))))
	   (ecart (if (= (length midicents-aux) 1) 0 (* (max-ecart midicents-aux step) 1.0))))

         (if (= (length (first midicents-list)) 1)
	       (list  (if (= 0.0 ecart)
          (g-round (/ pre-lin 1) 2)
          (g-round (/ pre-lin ecart) 2)))
              (cons  (if (= 0.0 ecart)
          (g-round pre-lin 2)
         (g-round (/ pre-lin ecart) 2))
	  (s-linearity-rec (rest midicents-list) :use-remove-duplicates use-remove-duplicates )) 
           ))))



;modificada por Carol --> com a remocao das duplicatas, com a ordenacao da mais grave a mais aguda e sem o teste da divisao por zero
;(defun s-linearity (midicents &key (step 100) (use-remove-duplicates 1))
;    "Cálculo da linearidade espacial."
;    (let* ((midicents-aux (ordem-crescente (cond ((= use-remove-duplicates 0) midicents)
;                                                 (t (remove-duplicates midicents :from-end t)))))
;          (pre-lin (reduce #'+ (mapcar #'abs (mapcar #'- midicents-aux (paradigm-list-constructor midicents-aux step)))))
;	   (ecart (* (max-ecart midicents-aux step) 1.0)))
;      (if (= 0 ecart)
;          (g-round (/ pre-lin 1) 2)
; 
;         (g-round (/ pre-lin ecart) 2))))


;****************************************************************************************

;****************************************************************************************

;APAGAR ISSO DEPOIS
;(let* ((chord-aux (ordem-crescente (cond ((= use-remove-duplicates 0) chord)
;                                           (t (remove-duplicates chord :from-end t))))))
;    (cond ((< (length chord-aux) 2) nil)
;          (t (let* ((v-fund (cond ((/= user-fundamental 0) user-fundamental)
;                                  ((= use-virtual-fundamental 0) (min-lista chord-aux)) 
;                                  (t (virtual-fundamental chord-aux threshold))))
;                    (deviation (e-deviation chord-aux v-fund range use-transpoct use-round-midicents))
;                    (weight (harmonic-weight chord-aux v-fund range use-round-midicents listout))
;                    (harmonicity-value (g-round (/ (+ deviation (cond ((atom weight) weight) 
;                                                                      (t (first weight)))) 2) 2)))
;               (list harmonicity-value deviation (first weight) v-fund (second weight))))))

;****************************************************************************************
(defun primeira-saida-harmonicity (chord &key (listout 1) (range 16) (use-virtual-fundamental 1) (user-fundamental  0) 
                       (threshold 3500) (use-round-midicents 1) (use-transpoct 1) (use-remove-duplicates 1))
             
     (let* ((chord-aux (ordem-crescente (cond ((= use-remove-duplicates 0) (first chord))         
                                  (t (remove-duplicates (first chord) :from-end t))))))
     (if (= (length chord) 1)       
       (if (= (length (first chord)) 1)
         (list 0)
        (list (cond ((< (length chord-aux) 2) nil)
                 (t (let* ((v-fund (cond ((/= user-fundamental 0) user-fundamental)
                                  ((= use-virtual-fundamental 0) (min-lista chord-aux)) 
                                  (t (virtual-fundamental chord-aux threshold))))
                    (deviation (e-deviation chord-aux v-fund range use-transpoct use-round-midicents))
                    (weight (harmonic-weight chord-aux v-fund range use-round-midicents listout)))
                    (g-round (/ (+ deviation (cond ((atom weight) weight) 
                                                  (t (first weight)))) 2) 2))))))
       (if (= (length (first chord)) 1)
         (cons 0 (primeira-saida-harmonicity (rest chord) :listout listout
                                                :range range
                                                :use-virtual-fundamental  use-virtual-fundamental
                                                :user-fundamental user-fundamental
                                                :threshold  threshold
                                                :use-round-midicents use-round-midicents
                                                :use-transpoct  use-transpoct
                                                :use-remove-duplicates  use-remove-duplicates))
       (cons (cond ((< (length chord-aux) 2) nil)
                 (t (let* ((v-fund (cond ((/= user-fundamental 0) user-fundamental)
                                  ((= use-virtual-fundamental 0) (min-lista chord-aux)) 
                                  (t (virtual-fundamental chord-aux threshold))))
                    (deviation (e-deviation chord-aux v-fund range use-transpoct use-round-midicents))
                    (weight (harmonic-weight chord-aux v-fund range use-round-midicents listout)))
                    (g-round (/ (+ deviation (cond ((atom weight) weight) 
                                                  (t (first weight)))) 2) 2))))
   
          (primeira-saida-harmonicity (rest chord) :listout listout
                                                :range range
                                                :use-virtual-fundamental  use-virtual-fundamental
                                                :user-fundamental user-fundamental
                                                :threshold  threshold
                                                :use-round-midicents use-round-midicents
                                                :use-transpoct  use-transpoct
                                                :use-remove-duplicates  use-remove-duplicates))))))

(defun segunda-saida-harmonicity (chord &key (range 16) (use-virtual-fundamental 1) (user-fundamental  0) 
                       (threshold 3500) (use-round-midicents 1) (use-transpoct 1) (use-remove-duplicates 1))
    
     (let* ((chord-aux (ordem-crescente (cond ((= use-remove-duplicates 0) (first chord))         
                                  (t (remove-duplicates (first chord) :from-end t))))))  
   
       (if (= (length chord) 1) 
            (if (= (length (first chord)) 1)
              (list 0)
         (list (cond ((< (length chord-aux) 2) nil)
                 (t (let* ((v-fund (cond ((/= user-fundamental 0) user-fundamental)
                                      ((= use-virtual-fundamental 0) (min-lista chord-aux)) 
                                      (t (virtual-fundamental chord-aux threshold)))))
                (e-deviation chord-aux v-fund range use-transpoct use-round-midicents))))))
                 
            (if (= (length (first chord)) 1)
              (cons 0 (segunda-saida-harmonicity (rest chord) 
                                         :range range
                                         :use-virtual-fundamental  use-virtual-fundamental
                                         :user-fundamental user-fundamental
                                         :threshold  threshold
                                         :use-round-midicents use-round-midicents
                                         :use-transpoct  use-transpoct
                                         :use-remove-duplicates  use-remove-duplicates))
        (cons (cond ((< (length chord-aux) 2) nil)
                 (t (let* ((v-fund (cond ((/= user-fundamental 0) user-fundamental)
                                      ((= use-virtual-fundamental 0) (min-lista chord-aux)) 
                                      (t (virtual-fundamental chord-aux threshold)))))
                (e-deviation chord-aux v-fund range use-transpoct use-round-midicents))))

          (segunda-saida-harmonicity (rest chord) 
                                         :range range
                                         :use-virtual-fundamental  use-virtual-fundamental
                                         :user-fundamental user-fundamental
                                         :threshold  threshold
                                         :use-round-midicents use-round-midicents
                                         :use-transpoct  use-transpoct
                                         :use-remove-duplicates  use-remove-duplicates))))))

 (defun terceira-saida-harmonicity (chord &key (listout 1) (range 16) (use-virtual-fundamental 1) (user-fundamental  0) 
                       (threshold 3500) (use-round-midicents 1) (use-remove-duplicates 1))
     (let* ((chord-aux (ordem-crescente (cond ((= use-remove-duplicates 0) (first chord))         
                                  (t (remove-duplicates (first chord) :from-end t))))))
     (if (= (length chord) 1)       
       (if (= (length (first chord)) 1)
         (list 0)
        (list (cond ((< (length chord-aux) 2) nil)
                 (t (let* ((v-fund (cond ((/= user-fundamental 0) user-fundamental)
                                  ((= use-virtual-fundamental 0) (min-lista chord-aux)) 
                                  (t (virtual-fundamental chord-aux threshold))))
                    (weight (harmonic-weight chord-aux v-fund range use-round-midicents listout)))
                    (first weight))))))
       
       (if (= (length (first chord)) 1)
         (cons 0 (terceira-saida-harmonicity (rest chord) :listout listout
                                                :range range
                                                :use-virtual-fundamental  use-virtual-fundamental
                                                :user-fundamental user-fundamental
                                                :threshold  threshold
                                                :use-round-midicents use-round-midicents
                                                :use-remove-duplicates  use-remove-duplicates))
        (cons (cond ((< (length chord-aux) 2) nil)
                 (t (let* ((v-fund (cond ((/= user-fundamental 0) user-fundamental)
                                  ((= use-virtual-fundamental 0) (min-lista chord-aux)) 
                                  (t (virtual-fundamental chord-aux threshold))))
                    (weight (harmonic-weight chord-aux v-fund range use-round-midicents listout)))
                    (first weight))))

          (terceira-saida-harmonicity (rest chord) :listout listout
                                                :range range
                                                :use-virtual-fundamental  use-virtual-fundamental
                                                :user-fundamental user-fundamental
                                                :threshold  threshold
                                                :use-round-midicents use-round-midicents
                                                :use-remove-duplicates  use-remove-duplicates))))))

(defun quarta-saida-harmonicity (chord &key  (use-virtual-fundamental 1) (user-fundamental  0) 
                       (threshold 3500)  (use-remove-duplicates 1))
             
     (let* ((chord-aux (ordem-crescente (cond ((= use-remove-duplicates 0) (first chord))         
                                  (t (remove-duplicates (first chord) :from-end t))))))
     (if (= (length chord) 1)       
       (if (= (length (first chord)) 1)
         (list 0)
        (list (cond ((< (length chord-aux) 2) nil)
                 (t  (cond ((/= user-fundamental 0) user-fundamental)
                            ((= use-virtual-fundamental 0) (min-lista chord-aux)) 
                            (t (virtual-fundamental chord-aux threshold)))))))
          (if (= (length (first chord)) 1)
            (cons 0 (quarta-saida-harmonicity (rest chord) 
                                                :use-virtual-fundamental  use-virtual-fundamental
                                                :user-fundamental user-fundamental
                                                :threshold  threshold
                                                :use-remove-duplicates  use-remove-duplicates))
        (cons (cond ((< (length chord-aux) 2) nil)
                 (t  (cond ((/= user-fundamental 0) user-fundamental)
                            ((= use-virtual-fundamental 0) (min-lista chord-aux)) 
                            (t (virtual-fundamental chord-aux threshold)))))

          (quarta-saida-harmonicity (rest chord) 
                                                :use-virtual-fundamental  use-virtual-fundamental
                                                :user-fundamental user-fundamental
                                                :threshold  threshold
                                                :use-remove-duplicates  use-remove-duplicates))))))



(defun quinta-saida-harmonicity (chord &key (listout 1) (range 16) (use-virtual-fundamental 1) (user-fundamental  0) 
                       (threshold 3500) (use-round-midicents 1) (use-remove-duplicates 1))
             
     (let* ((chord-aux (ordem-crescente (cond ((= use-remove-duplicates 0) (first chord))         
                                  (t (remove-duplicates (first chord) :from-end t))))))
     (if (= (length chord) 1)       
       (if (= (length (first chord)) 1)
         (list (list 0))
        (list (cond ((< (length chord-aux) 2) nil)
                 (t (let* ((v-fund (cond ((/= user-fundamental 0) user-fundamental)
                                  ((= use-virtual-fundamental 0) (min-lista chord-aux)) 
                                  (t (virtual-fundamental chord-aux threshold))))
                    (weight (harmonic-weight chord-aux v-fund range use-round-midicents listout)))
                    (second weight))))))
       (if (= (length (first chord)) 1)
         (cons (list 0) (quinta-saida-harmonicity (rest chord) :listout listout
                                                :range range
                                                :use-virtual-fundamental  use-virtual-fundamental
                                                :user-fundamental user-fundamental
                                                :threshold  threshold
                                                :use-round-midicents use-round-midicents
                                                :use-remove-duplicates  use-remove-duplicates))
        (cons (cond ((< (length chord-aux) 2) nil)
                 (t (let* ((v-fund (cond ((/= user-fundamental 0) user-fundamental)
                                  ((= use-virtual-fundamental 0) (min-lista chord-aux)) 
                                  (t (virtual-fundamental chord-aux threshold))))
                    (weight (harmonic-weight chord-aux v-fund range use-round-midicents listout)))
                    (second weight))))

          (quinta-saida-harmonicity (rest chord) :listout listout
                                                :range range
                                                :use-virtual-fundamental  use-virtual-fundamental
                                                :user-fundamental user-fundamental
                                                :threshold  threshold
                                                :use-round-midicents use-round-midicents
                                                :use-remove-duplicates  use-remove-duplicates))))))



;***************************  Funcoes auxiliares *********************************************************
;*********************************************************************************************************
;Essas funcoes sao chamadas nas funcoes recursivas acima.


;; ---------------------------------------------------------------------
;; Sonance
(defun lbc (notas-midics)
    "Function that gives the critical pitch for each tone. The function (after Parncutt 1988:85) is described in Guigue 1996 : 481-482."
    (let* ((notas (midicent2midi notas-midics))
	   (aux1 (subtracao-de-uma-lista-por-um-numero
		    (divisao-de-uma-lista-por-um-numero notas 5)
		    12.4))
	   (aux2 (divisao-de-um-numero-por-uma-lista 5
		   (soma-de-uma-lista-por-um-numero
		       (mapcar #'/ aux1  (raiz-quadrada-recursiva
			       (soma-de-uma-lista-por-um-numero
				   (mapcar #'* aux1 aux1)
				   44)))
		       1))))
     (multiplicacao-de-uma-lista-por-um-numero
	 (g-round-list
	     (multiplicacao-de-uma-lista-por-um-numero (mapcar #'+ notas aux2) 100)
	     0
	     100)
	 100)))

;; questão a ser considerada na otimização: lbc e qlbc podem ser feitas de uma vez só???

(defun qlbc (notas-midics)
     "Function that gives the quarter of the distance resulting from lbc."
    (let* ((notas (midicent2midi notas-midics))
	   (aux1 (subtracao-de-uma-lista-por-um-numero
		    (divisao-de-uma-lista-por-um-numero notas 5)
		    12.4))
	   (aux2 (raiz-quadrada-recursiva
		   (soma-de-uma-lista-por-um-numero
		       (mapcar #'* aux1 aux1)
		       44)))
	   (aux3 (divisao-de-uma-lista-por-um-numero
		   (divisao-de-um-numero-por-uma-lista 5
		       (soma-de-uma-lista-por-um-numero
			   (mapcar #'/ aux1 aux2)
			   1))
		   4)))
       (multiplicacao-de-uma-lista-por-um-numero
	 (g-round-list
	     (multiplicacao-de-uma-lista-por-um-numero (mapcar #'+ notas aux3) 100)
	     0
	     100)
	 100)))

;-----------------------------------------------------------------------------------------------------------------


;; Ambitus - general
;; No need for piano specific functions


(defun soalambitus (midics)
    "Retorna o âmbito"
    (- (first (ordem-decrescente midics))
       (first (ordem-crescente midics))))

(defun soalambitus-relative (midicents range)
    "Retorna o âmbito relativo para qualquer instrumento."
    (g-round 
	(/ (soalambitus midicents) 
	   (* 1.0 range))
	2))

;feita por Carol
(defun preenche-ambito (ambito intervalo)
  "Preenche o ambito. Recebe uma lista 'ambito' que eh na forma (low high) e um numero 'intervalo'
e retorna uma lista que inicia em low e termina em high, e os elementos intermediarios possuem
uma diferenca de 'intervalo' entre os adjacentes."
  (let* ((low (first ambito))
         (high (second ambito)))
    (cond ((> low high) nil)
          (t (cons low
                   (preenche-ambito (list (+ low intervalo) high) intervalo))))))

;----------------------------------------------------------------------------------------------
(defun max-ecart (midicents step)
    "Cf. tese p. 477."
    (let ((aux1 (- (itv-paradigm midicents step) 100))
	  (aux2 (reverse (constroi-lista (- (length midicents) 2)))))
      (reduce #'+ (max-ecart-aux aux1 aux2))))

(defun max-ecart-aux (number list)
    "Recursão."
    (cond ((null list) nil)
	  (t (cons (* number (first list))
		   (max-ecart-aux number (rest list))))))

(defun constroi-lista (number) ; spliter e' melhor! (mais completo - mas ainda falta o step).
    "constroi uma lista de 1 ate' number."
    (let ((aux1 1)
	  (aux2 nil))
	(do ((aux1.1 aux1 (+ 1 aux1.1)))
	    ((= aux1.1 (+ 1 number)))
	  (setf aux2 (cons aux1.1 aux2)))
      aux2))

   (defun itv-paradigm (midicents step)
    "Cálculo do intervalo paradigmático. Cf. tese p. 477."
    (om-round (* (*
                    (/
                       (soalambitus midicents)
                       (- (* (length midicents)
                             step)
                          step))
                    step)
                 1.0) 2))

   (defun paradigm-list-constructor (midicents step)
    "Constrói uma lista a partir do valor mais baixo e do intervalo paradigmatico."
    (let ((aux (first (ordem-crescente midicents)))
	  (paradigm-interval (make-list (- (length midicents) 1) :initial-element (itv-paradigm midicents     step))))
      (cons aux
            (mapcar #'(lambda (x) (incf aux x)) paradigm-interval))))


;----------------------------------------------------------------------------------------------

;; ---------------------------------------------------------------------
;; Register

;; Piano specific
(defun register-fill-piano (midicents)
    "Recebe uma lista de midicents e retorna uma lista indicando o número de notas em cada registro."
    (list (length (reg-3? midicents))
	  (length (reg-2? midicents))
	  (length (reg-1? midicents))
	  (length (reg0? midicents))
	  (length (reg1? midicents))
	  (length (reg2? midicents))
	  (length (reg3? midicents))))

(defun reg-3? (lista)
    "Retorna os elementos da lista que pertencem ao registro -3."
    (cond ((null lista) nil)
	  ((and (<= (first lista) (highest-note -3))
		(>= (first lista) (lowest-note -3)))
	      (cons (first lista) (reg-3? (rest lista))))
	  (t (reg-3? (rest lista)))))

(defun reg-2? (lista)
    "Retorna os elementos da lista que pertencem ao registro -2."
    (cond ((null lista) nil)
	  ((and (<= (first lista) (highest-note -2))
		(>= (first lista) (lowest-note -2)))
	     (cons (first lista) (reg-2? (rest lista))))
	  (t (reg-2? (rest lista)))))

(defun reg-1? (lista)
    "Retorna os elementos da lista que pertencem ao registro -1."
    (cond ((null lista) nil)
	  ((and (<= (first lista) (highest-note -1))
		(>= (first lista) (lowest-note -1)))
	     (cons (first lista) (reg-1? (rest lista))))
	  (t (reg-1? (rest lista)))))

(defun reg0? (lista)
    "Retorna os elementos da lista que pertencem ao registro 0."
    (cond ((null lista) nil)
	  ((and (<= (first lista) (highest-note 0))
		(>= (first lista) (lowest-note 0)))
	     (cons (first lista) (reg0? (rest lista))))
	  (t (reg0? (rest lista)))))

(defun reg1? (lista)
    "Retorna os elementos da lista que pertencem ao registro 1."
    (cond ((null lista) nil)
	  ((and (<= (first lista) (highest-note 1))
		(>= (first lista) (lowest-note 1)))
	     (cons (first lista) (reg1? (rest lista))))
	  (t (reg1? (rest lista)))))

(defun reg2? (lista)
    "Retorna os elementos da lista que pertencem ao registro 2."
    (cond ((null lista) nil)
	  ((and (<= (first lista) (highest-note 2))
		(>= (first lista) (lowest-note 2)))
	     (cons (first lista) (reg2? (rest lista))))
	  (t (reg2? (rest lista)))))

(defun reg3? (lista)
    "Retorna os elementos da lista que pertencem ao registro ."
    (cond ((null lista) nil)
	  ((and (<= (first lista) (highest-note 3))
		(>= (first lista) (lowest-note 3)))
	      (cons (first lista) (reg3? (rest lista))))
	  (t (reg3? (rest lista)))))

(defun filled-registers-piano (midicents)
    "Recebe uma lista de midicents e retorna uma lista com os registros do piano que estão ocupados."
    (let ((aux (positivo? (register-fill-piano midicents))))
	(filled-registers-piano-aux aux)))

(defun filled-registers-piano-aux (lista)
    "Função auxiliar para filled-registers-piano. Recursão é feita aqui"
    (g-round 
	(remove 0 (mapcar #'* lista guigue-piano-regs))))

(defun low-bound-piano (midicents)
    "Retorna o registro mais grave onde existe uma nota."
    (first (ordem-crescente (filled-registers-piano midicents))))

(defun high-bound-piano (midicents)
    "Retorna o registro mais agudo onde existe uma nota."
    (first (ordem-decrescente (filled-registers-piano midicents))))

(defun registration-piano (midicents)
    "Filling rate of the registers, compared to their total number."
    (g-round
	(/ (length (filled-registers-piano midicents)) 7.0)
	2))

(defun register-weight-piano (midicents)
    "The relative register quality weight. Soma dos pesos de todos os registros do objeto."
    (let ((aux (positivo? (register-fill-piano midicents))))
	(register-weight-piano-aux aux)))

(defun register-weight-piano-aux (lista)
    "Função auxiliar para register-weight-piano. Recursão é feita aqui."
    (g-round
	(reduce #'+ 
	    (mapcar #'* lista guigue-piano-regs-weights))
	2))

(defun asq (midicents)
    "Relative average spatial quality - i.e. the average value of soalambitus-relative for the piano and register-weight-piano."
    (g-round
	(/ 
	    (+ (soalambitus-relative midicents 8700)
	       (register-weight-piano midicents))
	   2)
       2))


;; General (register)

(defun belong-to-register? (midicent registers)
    "Função retorna uma lista indicando o registro onde a nota se encontra."
    (cond ((null registers) nil)
	  ((and (>= midicent (first (first registers)))
		(<= midicent (second (first registers))))
	     (cons 1 (belong-to-register? midicent (rest registers))))
	  (t (cons 0 (belong-to-register? midicent (rest registers))))))

(defun register-filling (midicents registers)
    "Retorna uma lista de listas indicando em que registro a nota se encontra."
    (cond ((null midicents) nil)
	  (t (cons (belong-to-register? (first midicents) registers)
		   (register-filling (rest midicents) registers)))))

;;alterada por Carol -> retorna os registros variando de -3 a 3
(defun register-fill (midicents registers)
    "Retorna os registros ocupados pela lista de midicents."
    (let ((aux-for-register (register-filling midicents registers)))
	(ordem-crescente
	    (remove-if-not #'numberp	; Isto é para retirar algum nil que possa aparecer na lista (um midicent que esteja fora do registro, p.e.)
		(remove-duplicates 
                    ;O ERRO SE ENCONTRA AQUI!!!!!!!!
		    ;(subtracao-de-uma-lista-por-um-numero (register-fill-aux aux-for-register) 3))))))
                     (register-fill-aux aux-for-register)))))) 


(defun piano-register-fill (midicents registers)
    "Retorna os registros ocupados pela lista de midicents."
    (let ((aux-for-register (register-filling midicents registers)))
	(ordem-crescente
	    (remove-if-not #'numberp	; Isto é para retirar algum nil que possa aparecer na lista (um midicent que esteja fora do registro, p.e.)
		(remove-duplicates 
                    ;O ERRO SE ENCONTRA AQUI!!!!!!!!
		    (subtracao-de-uma-lista-por-um-numero (register-fill-aux aux-for-register) 3))))))
                     

(defun register-fill-aux (lista)
    "Função que retorna os registros ocupados."
    (cond ((null lista) nil)
	  (t (cons 
		  (position 1 (first lista))
		  (register-fill-aux (rest lista))))))

(defun register-fill-number (midicents registers)
    "Retorna o número de notas em cada registro."
    (let* ((filled-regs (register-fill-aux (register-filling midicents registers)))
	   (filled-regs-ordenado (ordem-crescente (remove-if-not #'numberp filled-regs))))
       (reverse 
	   (register-fill-number-aux
	       (contador filled-regs-ordenado) registers))))

(defun register-fill-number-aux (list registers)
    "Recursão é feita aqui."    
    (let ((aux-regs nil))
	(do ((aux 0 (+ aux 1)))
	    ((= aux (length registers)))
	  (if (assoc aux list)
	      (setf aux-regs (cons (second (assoc aux list)) aux-regs))
	      (setf aux-regs (cons 0 aux-regs))))
       aux-regs))

(defun register-weight-general (midicents registers registers-weights)
    "peso de cada registro."
    (let* ((filled-regs (register-fill-aux (register-filling midicents registers)))
	   (filled-regs-ordenado (ordem-crescente (remove-if-not #'numberp filled-regs))))
       (register-weight-general-aux
	   (reverse 
	       (registers-with-note
		   (contador filled-regs-ordenado) registers)) registers-weights)))

(defun register-weight-general-aux (lista registers-weights)
    "Função auxiliar para register-weight-piano. Recursão é feita aqui."
    (g-round
	(reduce #'+ 
	    (mapcar #'* lista registers-weights))
	2))

(defun registers-with-note (list registers)
    "Recursão é feita aqui."    
    (let ((aux-regs nil))
	(do ((aux 0 (+ aux 1)))
	    ((= aux (length registers)))
	  (if (assoc aux list)
	      (setf aux-regs (cons 1 aux-regs))
	      (setf aux-regs (cons 0 aux-regs))))
       aux-regs))

(defun registration-general (midicents registers)
    "Filling rate of the registers, compared to their total number."
    (g-round
	(/ (length (register-fill midicents registers)) (length registers))
	2))

(defun low-bound-general (midicents registers)
    "Retorna o registro mais grave onde existe uma nota."
    (first (ordem-crescente (register-fill midicents registers))))

(defun high-bound-general (midicents registers)
    "Retorna o registro mais grave onde existe uma nota."
    (first (ordem-decrescente (register-fill midicents registers))))

(defun piano-low-bound (midicents registers)
    "Retorna o registro mais grave onde existe uma nota."
    (first (ordem-crescente (piano-register-fill midicents registers))))

(defun piano-high-bound (midicents registers)
    "Retorna o registro mais grave onde existe uma nota."
    (first (ordem-decrescente (piano-register-fill midicents registers))))

(defun asq-general (midicents range registers registers-weights)
    "Relative average spatial quality - i.e. the average value of soalambitus-relative for the piano and register-weight-piano."
    (g-round
	(/ 
	      (+ (soalambitus-relative midicents (* 1.0 range))
		 (register-weight-general midicents registers registers-weights))
	     2)
	 2))
;; ---------------------------------------------------------------------




(defun virtual-fundamental (chord &optional (threshold 3500))
  "Calcula a fundamental virtual de um acorde, uma fundamental (oitava da original
   em cujo escopo da serie harmonica as notas do acorde se encaixarao, caso estas
   estejam muito proximas da fundamental real (nota mais grave) do acorde. O threshold
   eh um valor opcional a ser entrado pelo usuario como limite inferior a partir do
   qual a fundamental nao eh mais transposta. Quando a fundamental real se aproxima
   do threshold ela eh gradualmente menos transposta, e, sendo inferior a este, nunca
   o eh."

;;  "Algoritmo usado para achar a fundamental virtual:
;;
;;se (fundamentalReal <threshold)
;;	retorna fundamentalReal;
;;senao
;;	D = segundaNota – fundamentalReal;
;;	fundamentalVirtual = fundamentalReal;
;;	se (D <1200)
;;		fundamentalVirtual = fundamentalReal –1200;
;;	se ( (D<700) and (fundamentalReal > (threshold + 1200))
;;		fundamental Virtual = fundamentalReal – 2400;
;;	retorna fundamentalVirtual."

;; min-lista defined in aux.lisp

  (let ((d (- (min-lista (remove (min-lista chord) chord))
              (min-lista chord)))
        (real-fund (min-lista chord)))
    (cond ((< real-fund threshold) real-fund)
          ((and (< d 700) (< real-fund (+ threshold 1200))) (- real-fund 1200)) ;; real-fund na oitava do threshold
          ((< d 700) (- real-fund 2400))
          ((< d 1200) (- real-fund 1200))
          (t real-fund))))

(defun e-deviation (chord fundamental &optional (range 16) (transpoct 1) (round 1))
  "Calcula o desvio entre as notas do chord e um objeto harmonico paradigmatico
   criado a partir da serie harmonica da fundamental fornecida. O parametro range
   determina quantos harmonicos da serie devem ser levados em consideracao e transpoct
   determina se as notas do acorde superiores ao harmonico mais alto do ohp serao
   transpostas para o ambito do mesmo e round determina se as notas da serie
   serao arredondadas para os semitons exatos (escala temperada) (para ambos 0
   nao executa e 1 executa)."
  (let* ((pho (pho fundamental range round))
         (transpocted-chord (cond ((= transpoct 0) chord)
                                  (t (transpoct-notes chord (max-lista pho))))))
    (g-round (e-proportion transpocted-chord pho) 2)))

 (defun e-proportion (chord pho)
  "Calcula a proporcao entre o numero de notas que nao sao comuns a chord e pho e
   o numero de notas de chord."
  (let ((chord-notes (- (length chord) 1))                     ;; -1 para nao levar em conta
        (diff-notes (- (length (intersection chord pho)) 1)))  ;; a fundamental, presente em ambos.
    (float (/ (- chord-notes diff-notes) chord-notes))))

 (defun PHO (fundamental &optional (range 16) (round 1))
  "Constroi o Objeto Harmonico Paradigmatico. Este objeto eh a serie harmonica da
   fundamental(midicents) fornecida com o numero de harmonicos dado pelo parametro
   range. O parametro round define se o algoritmo arredondara as notas para os semitons mais
   proximos."
   ;; Sugestao futura: deixar o grau de arredondamento (semitom, quarto de tom,...) em aberto
  (cond ((null fundamental) nil)
        ((= round 0) (f->mc-soal (soal-harm-series (mc->f-soal fundamental) range)))
        (t (round-midicents (f->mc-soal (soal-harm-series (mc->f-soal fundamental) range))))))

  (defun soal-harm-series (fundamental range)
  "Constroi a serie harmonica da fundamental fornecida em Hz com um numero range
   especificando a quantidade de harmonicos"
  (cond ((null fundamental) nil)
        ((null range) fundamental)
        (t (multiplicacao-de-uma-lista-por-um-numero (spliter 1 range) fundamental))))

  (defun transpoct-notes (chord max)
  "Transpoe as notas do acorde (lista chord) que sao mais agudas (altas) do que max
   ate a oitava imediatamente abaixo do max"
   (cond ((null chord) nil)
         ((> (first chord) max) (transpoct-notes (cons (- (first chord) 1200)
                                                       (rest chord)) max))
         (t (cons (first chord)
                  (transpoct-notes (rest chord) max)))))

(defun harmonic-weight (chord fundamental &optional (range 16) (round 1) (listout 0))
  "Cacula o peso relativo (de 0 a 1) das posicoes dos harmonicos coincidentes entre o acorde e a
   serie harmonica da fundamental fornecida, no ambito do acorde. O parametro range funciona como
   um referencial, indicando o numero de harmonicos da serie considerados para o objeto harmonico
   paradigmatico. Round indica se as notas da serie devem ser arredondadas para a escala temperada
   e listout em 1 indica que no output havera tambem a lista dos harmonicos coincidentes."
  (let* ((ph-object (intelligent-PHO fundamental (max-lista chord) round))
         (coincident-harmonics (index-lista-de-lista (intersection chord ph-object) ph-object 1))
         (weight (g-round (/ (media coincident-harmonics) (* range 2)) 2)))
    (cond ((and (= listout 0) (> weight 1)) 1)
          ((= listout 0) weight)
          ((> weight 1) (list 1 (ordem-crescente coincident-harmonics)))
          (t (list weight (ordem-crescente coincident-harmonics))))))

(defun intelligent-PHO (fundamental max &optional (round 1))
  "Constroi um objeto harmonico paradigmatico composto pela serie harmonica da fundamental (midicents) com um numero de
   harmonicos suficiente para que o ultimo harmonico seja o imediatamente superior ao parametro max (midicents). Round
   em zero determina o nao-arredondamento dos midicents das notas."
  (cond ((null fundamental) nil)
        ((> fundamental max) nil)
        ((= round 0) (f->mc-soal (soal-harm-series-with-max (mc->f-soal fundamental) (mc->f-soal max))))
        (t (round-midicents (f->mc-soal (soal-harm-series-with-max (mc->f-soal fundamental) (mc->f-soal max)))))))

(defun soal-harm-series-with-max (fundamental max)
  "Constroi a serie harmonica da fundamental fornecida em Hz com um numero de harmonicos
   suficiente para que o ultimo seja o primeiro imediatamente superior a max."
  (cond ((null fundamental) nil)
        (t (soal-harm-series-wm-aux fundamental max))))

(defun soal-harm-series-wm-aux (fundamental max &optional (atual 1))
  "Usada em harm-series-with-max, gera a serie harmonica, usando o parametro atual como contador."
  (cond ((>= (* atual fundamental) max) (multiplicacao-de-uma-lista-por-um-numero (spliter 1 atual) fundamental))
        (t (soal-harm-series-wm-aux fundamental max (+ atual 1)))))

;; ---------------------------------------------------------------------
;Criadas por Hildegard para uso pelo metodo Sonance

(defun intervalos-sonance (list)
  (if (= (length list) 1)
    (list (intervalos-sonance-aux (first list)))
    (cons (intervalos-sonance-aux (first list)) (intervalos-sonance (rest list)))))

(defun intervalos-sonance-aux (list)
  (let ((modulo (if (null list) 0
                       (mod (first list) 12)))) 
  (cond ((null list) nil)
     ((= modulo 0) (cons 12.0 (intervalos-sonance-aux (rest list)))) 
    (t (cons modulo (intervalos-sonance-aux (rest list)))))))

(defun oitavas-sonance (intervalos entrada)
  (cond ((or (null intervalos) (null entrada)) nil)
    ((and (= (length intervalos) 1) (= (length entrada) 1))
      (list (oitavas-sonance-aux (first intervalos) (first entrada))))
    (t (cons (oitavas-sonance-aux (first intervalos) (first entrada)) (oitavas-sonance (rest intervalos) (rest entrada))))))

(defun oitavas-sonance-aux (intervalos entrada)
  (cond ((or (null intervalos) (null entrada)) nil)
        ((= (first intervalos) 12) (cons (- (truncate (/ (first entrada) 12)) 1) (oitavas-sonance-aux (rest intervalos) (rest entrada))))
        (t (cons (truncate (/ (first entrada) 12)) (oitavas-sonance-aux (rest intervalos) (rest entrada))))))

(defun entrada-sonance (list)
"Pega uma lista, ordena-a em ordem decrescente, extrai a distância entre os valores do resultado da ordenacão e divide essa última por 100.
É usada no método Sonance.
Ex.: (4000 5500 5000) -> (5500 5000 4000) -> (500 1000) -> (5 10) -> (10 5)"
    (cond ((null list) nil)
          ((and (= (length (first list)) 1) (= (length list) 1)) (list (list 0)))
          ((and (= (length (first list)) 1) (not (= (length list) 1))) (cons (list 0) (entrada-sonance (rest list))))  
          (t (cons (inverso (divisao-de-uma-lista-por-um-numero (diferenca-elementos-adjacentes (ordem-decrescente (remove-duplicates (first list)))) 100))
          (entrada-sonance (rest list))))))


(defun primeiro-if-aninhado-sonance (tabela oitavasconstantes oitavasresultantes intervalos resultadoifaninhadoanterior)
"Calcula a quarta coluna da matriz de ifs aninhados resultantes da entrada do usuario"
  (cond ((or (null oitavasresultantes) (null intervalos)) nil)
        ((and (= (length oitavasresultantes) 1) (= (length intervalos) 1))
         (list (primeiro-if-aninhado-sonance-rec tabela oitavasconstantes (first oitavasresultantes) (first intervalos) (first resultadoifaninhadoanterior))))
        (t (cons (primeiro-if-aninhado-sonance-rec tabela oitavasconstantes (first oitavasresultantes) (first intervalos) (first resultadoifaninhadoanterior))
                 (primeiro-if-aninhado-sonance tabela oitavasconstantes (rest oitavasresultantes) (rest intervalos) (rest resultadoifaninhadoanterior))))))

(defun primeiro-if-aninhado-sonance-rec (tabela oitavasconstantes oitavasresultantes intervalos resultadoifaninhadoanterior)
"Calcula a quarta coluna da matriz de ifs aninhados resultantes da entrada do usuario"
     (cond ((or (null oitavasresultantes) (null intervalos)) nil)
           ((and (= (length oitavasresultantes) 1) (= (length intervalos) 1)) 
            (primeiro-if-aninhado-sonance-aux tabela oitavasconstantes (first oitavasresultantes) (first intervalos) (first resultadoifaninhadoanterior)))
           (t (cons 
               (first (primeiro-if-aninhado-sonance-aux tabela oitavasconstantes (first oitavasresultantes) (first intervalos) (first resultadoifaninhadoanterior))) 
               (primeiro-if-aninhado-sonance-rec tabela oitavasconstantes (rest oitavasresultantes) (rest intervalos) (rest resultadoifaninhadoanterior))
               ))))

(defun primeiro-if-aninhado-sonance-aux (tabela oitavasconstantes entradaoitavasresultantes entradaintervalos entradaresultadoifaninhadoanterior)
    (if (and (= entradaintervalos (elt (elt tabela 0) 11)) (= entradaoitavasresultantes (first oitavasconstantes))) 
       (list (elt (elt tabela 2) 11)) 
         (if (and (= entradaintervalos (elt (elt tabela 0) 11)) (= entradaoitavasresultantes (second oitavasconstantes)))
           (list (elt (elt tabela 3) 11))
           (if (and (= entradaintervalos (elt (elt tabela 0) 11)) (= entradaoitavasresultantes (third oitavasconstantes)))
             (list (elt (elt tabela 4) 11))
             (if (and (= entradaintervalos (elt (elt tabela 0) 11)) (= entradaoitavasresultantes (fourth oitavasconstantes)))
               (list (elt (elt tabela 5) 11))
               (if (and (= entradaintervalos (elt (elt tabela 0) 10)) (= entradaoitavasresultantes (first oitavasconstantes)))
                 (list (elt (elt tabela 2) 10))
                 (if (and (= entradaintervalos (elt (elt tabela 0) 10)) (= entradaoitavasresultantes (second oitavasconstantes)))
                   (list (elt (elt tabela 3) 10))
                   (if (and (= entradaintervalos (elt (elt tabela 0) 10)) (= entradaoitavasresultantes (third oitavasconstantes)))
                     (list (elt (elt tabela 4) 10))
                     (list entradaresultadoifaninhadoanterior)))))))))


(defun segundo-if-aninhado-sonance (tabela oitavasconstantes oitavasresultantes intervalos resultadoifaninhadoanterior)
"Calcula a quarta coluna da matriz de ifs aninhados resultantes da entrada do usuario"
  (cond ((or (null oitavasresultantes) (null intervalos)) nil)
        ((and (= (length oitavasresultantes) 1) (= (length intervalos) 1))
         (list (segundo-if-aninhado-sonance-rec tabela oitavasconstantes (first oitavasresultantes) (first intervalos) (first resultadoifaninhadoanterior))))
        (t (cons (segundo-if-aninhado-sonance-rec tabela oitavasconstantes (first oitavasresultantes) (first intervalos) (first resultadoifaninhadoanterior))
                 (segundo-if-aninhado-sonance tabela oitavasconstantes (rest oitavasresultantes) (rest intervalos) (rest resultadoifaninhadoanterior))))))


(defun segundo-if-aninhado-sonance-rec (tabela oitavasconstantes oitavasresultantes intervalos resultadoifaninhadoanterior)
"Calcula a quarta coluna da matriz de ifs aninhados resultantes da entrada do usuario"
     (cond ((or (null oitavasresultantes) (null intervalos)) nil)
           ((and (= (length oitavasresultantes) 1) (= (length intervalos) 1)) 
            (segundo-if-aninhado-sonance-aux tabela oitavasconstantes (first oitavasresultantes) (first intervalos) (first resultadoifaninhadoanterior)))
           (t (cons 
               (first (segundo-if-aninhado-sonance-aux tabela oitavasconstantes (first oitavasresultantes) (first intervalos) (first resultadoifaninhadoanterior))) 
               (segundo-if-aninhado-sonance-rec tabela oitavasconstantes (rest oitavasresultantes) (rest intervalos) (rest resultadoifaninhadoanterior))
               ))))

(defun segundo-if-aninhado-sonance-aux (tabela oitavasconstantes entradaoitavasresultantes entradaintervalos entradaresultadoifaninhadoanterior)
    (if (and (= entradaintervalos (elt (elt tabela 0) 9)) (= entradaoitavasresultantes (first oitavasconstantes))) 
       (list (elt (elt tabela 2) 9)) 
         (if (and (= entradaintervalos (elt (elt tabela 0) 9)) (= entradaoitavasresultantes (second oitavasconstantes)))
           (list (elt (elt tabela 3) 9))
           (if (and (= entradaintervalos (elt (elt tabela 0) 9)) (= entradaoitavasresultantes (third oitavasconstantes)))
             (list (elt (elt tabela 4) 9))
             (if (and (= entradaintervalos (elt (elt tabela 0) 8)) (= entradaoitavasresultantes (first oitavasconstantes)))
               (list (elt (elt tabela 2) 8))
               (if (and (= entradaintervalos (elt (elt tabela 0) 8)) (= entradaoitavasresultantes (second oitavasconstantes)))
                 (list (elt (elt tabela 3) 9))
                 (if (and (= entradaintervalos (elt (elt tabela 0) 7)) (= entradaoitavasresultantes (first oitavasconstantes)))
                   (list (elt (elt tabela 2) 7))
                   (if (and (= entradaintervalos (elt (elt tabela 0) 7)) (= entradaoitavasresultantes (second oitavasconstantes)))
                     (list (elt (elt tabela 3) 7))
                     (list entradaresultadoifaninhadoanterior)))))))))


(defun terceiro-if-aninhado-sonance (tabela oitavasconstantes oitavasresultantes intervalos resultadoifaninhadoanterior)
"Calcula a quarta coluna da matriz de ifs aninhados resultantes da entrada do usuario"
  (cond ((or (null oitavasresultantes) (null intervalos)) nil)
        ((and (= (length oitavasresultantes) 1) (= (length intervalos) 1))
         (list (terceiro-if-aninhado-sonance-rec tabela oitavasconstantes (first oitavasresultantes) (first intervalos) (first resultadoifaninhadoanterior))))
        (t (cons (terceiro-if-aninhado-sonance-rec tabela oitavasconstantes (first oitavasresultantes) (first intervalos) (first resultadoifaninhadoanterior))
                 (terceiro-if-aninhado-sonance tabela oitavasconstantes (rest oitavasresultantes) (rest intervalos) (rest resultadoifaninhadoanterior))))))


(defun terceiro-if-aninhado-sonance-rec (tabela oitavasconstantes oitavasresultantes intervalos resultadoifaninhadoanterior)
"Calcula a quarta coluna da matriz de ifs aninhados resultantes da entrada do usuario"
     (cond ((or (null oitavasresultantes) (null intervalos)) nil)
           ((and (= (length oitavasresultantes) 1) (= (length intervalos) 1)) 
            (terceiro-if-aninhado-sonance-aux tabela oitavasconstantes (first oitavasresultantes) (first intervalos) (first resultadoifaninhadoanterior)))
           (t (cons 
               (first (terceiro-if-aninhado-sonance-aux tabela oitavasconstantes (first oitavasresultantes) (first intervalos) (first resultadoifaninhadoanterior))) 
               (terceiro-if-aninhado-sonance-rec tabela oitavasconstantes (rest oitavasresultantes) (rest intervalos) (rest resultadoifaninhadoanterior))
               ))))

(defun terceiro-if-aninhado-sonance-aux (tabela oitavasconstantes entradaoitavasresultantes entradaintervalos entradaresultadoifaninhadoanterior)
    (if (and (= entradaintervalos (elt (elt tabela 0) 6)) (= entradaoitavasresultantes (first oitavasconstantes))) 
       (list (elt (elt tabela 2) 6)) 
         (if (and (= entradaintervalos (elt (elt tabela 0) 6)) (= entradaoitavasresultantes (second oitavasconstantes)))
           (list (elt (elt tabela 3) 6))
           (if (and (= entradaintervalos (elt (elt tabela 0) 5)) (= entradaoitavasresultantes (first oitavasconstantes)))
             (list (elt (elt tabela 2) 5))
             (if (and (= entradaintervalos (elt (elt tabela 0) 5)) (= entradaoitavasresultantes (second oitavasconstantes)))
               (list (elt (elt tabela 3) 5))
               (if (and (= entradaintervalos (elt (elt tabela 0) 4)) (= entradaoitavasresultantes (first oitavasconstantes)))
                 (list (elt (elt tabela 2) 4))
                 (if (and (= entradaintervalos (elt (elt tabela 0) 4)) (= entradaoitavasresultantes (second oitavasconstantes)))
                   (list (elt (elt tabela 3) 4))
                   (if (and (= entradaintervalos (elt (elt tabela 0) 3)) (= entradaoitavasresultantes (first oitavasconstantes)))
                     (list (elt (elt tabela 2) 3))
                     (list entradaresultadoifaninhadoanterior)))))))))


(defun quarto-if-aninhado-sonance (tabela oitavasconstantes oitavasresultantes intervalos)
"Calcula a quarta coluna da matriz de ifs aninhados resultantes da entrada do usuario"
  (cond ((or (null oitavasresultantes) (null intervalos)) nil)
        ((and (= (length oitavasresultantes) 1) (= (length intervalos) 1))
         (list (quarto-if-aninhado-sonance-rec tabela oitavasconstantes (first oitavasresultantes) (first intervalos))))
        (t (cons (quarto-if-aninhado-sonance-rec tabela oitavasconstantes (first oitavasresultantes) (first intervalos))
                 (quarto-if-aninhado-sonance tabela oitavasconstantes (rest oitavasresultantes) (rest intervalos))))))


(defun quarto-if-aninhado-sonance-rec (tabela oitavasconstantes oitavasresultantes intervalos)
  (cond ((or (null oitavasresultantes) (null intervalos)) nil)
           ((and (= (length oitavasresultantes) 1) (= (length intervalos) 1)) 
            (quarto-if-aninhado-sonance-aux tabela oitavasconstantes (first oitavasresultantes) (first intervalos)))
           (t (cons 
               (first (quarto-if-aninhado-sonance-aux tabela oitavasconstantes (first oitavasresultantes) (first intervalos))) 
               (quarto-if-aninhado-sonance-rec tabela oitavasconstantes (rest oitavasresultantes) (rest intervalos))))
     ))

(defun quarto-if-aninhado-sonance-aux (tabela oitavasconstantes entradaoitavasresultantes entradaintervalos)
    (if (and (= entradaintervalos (elt (elt tabela 0) 3)) (= entradaoitavasresultantes (second oitavasconstantes))) 
       (list (elt (elt tabela 3) 3)) 
         (if (and (= entradaintervalos (elt (elt tabela 0) 2)) (= entradaoitavasresultantes (first oitavasconstantes)))
           (list (elt (elt tabela 2) 2))
           (if (and (= entradaintervalos (elt (elt tabela 0) 2)) (= entradaoitavasresultantes (second oitavasconstantes)))
             (list (elt (elt tabela 3) 2))
             (if (and (= entradaintervalos (elt (elt tabela 0) 1)) (= entradaoitavasresultantes (first oitavasconstantes)))
               (list (elt (elt tabela 2) 1))
               (list 0))))))
;---------------------------------------------------------------------
;usadas na funcao soalsubstitute
;---------------------------------------------------------------------


(defun procuraemlista (list procurado substituto)
  (if (= (length list) 1)
    (passalista (first list) procurado substituto)
    (cons (passalista (first list) procurado substituto)
          (procuraemlista (rest list) procurado substituto))))

(defun passalista (list procurado substituto)
  (if (= (length list) 1)
    (list (substituinumero (first list) procurado substituto))
    (cons (substituinumero (first list) procurado substituto)
          (passalista (rest list) procurado substituto))))

(defun substituinumero (numero procurado substituto)
  (if (= numero procurado)
    substituto
    numero))

;---------------------------------------------------------------------
;usada na funcao soalincluded?
;---------------------------------------------------------------------

(defun verificainclusaonaslistas (subconjunto conjunto)
  (cond ((or (null subconjunto) (null conjunto)) nil)
        ((= (length conjunto) 1)
         (list (included? subconjunto (first conjunto))))
        (t (cons (included? subconjunto (first conjunto))
                 (verificainclusaonaslistas subconjunto (rest conjunto))))))

(defun calcula-porcentagens (subconjunto conjunto) 
  (if (= (length conjunto) 1)
      (list (calcula-porcentagens-lista-simples subconjunto (first conjunto)))
    (cons (calcula-porcentagens-lista-simples subconjunto (first conjunto))
          (calcula-porcentagens subconjunto (rest conjunto))))
)

(defun calcula-porcentagens-lista-simples (subconjunto conjunto) 
  (if (= (length subconjunto) 1)
      (list (calcula-porcentagem-numero (first subconjunto) conjunto))
    (cons (calcula-porcentagem-numero (first subconjunto) conjunto)
          (calcula-porcentagens-lista-simples (rest subconjunto) conjunto)))
)

(defun calcula-porcentagem-numero (numero conjunto)
  (g-round (/ (count-soal numero conjunto) (length conjunto)) 2)
)

;---------------------------------------------------------------------
;usadas na funcao segmentation-midi
;---------------------------------------------------------------------

(defun segmenta (lista numeros)
  (if (= (length lista) 1)
      (list (segmenta-aux '() '() (first lista) (abs (g-round (first numeros) 2))))
    (cons (segmenta-aux '() '() (first lista) (abs (g-round (first numeros) 2)))
          (segmenta (rest lista) (rest numeros))))
)

(defun segmenta-aux (final inserindo removendo numero)
  (cond ((null removendo) final)
        ((= (length removendo) 1) (insert-element (insert-element (first removendo) inserindo) final))
        ((> (abs (- (first removendo) (second removendo))) numero)
         (segmenta-aux (insert-element (insert-element (first removendo) inserindo) final) '() (rest removendo) numero))
        (t (segmenta-aux final (insert-element (first removendo) inserindo) (rest removendo) numero))))

(defun segmenta-igual (parametro acao) 
  (segmenta-igual-aux parametro acao '() '())
)

(defun segmenta-igual-aux (parametro removendo final inserindo)
  (cond ((null parametro) final)
        ((= (length inserindo) (length (first parametro)))
         (segmenta-igual-aux (rest parametro) removendo (insert-element inserindo final) '()))
        (t (segmenta-igual-aux  parametro (rest removendo) final (insert-element (first removendo) inserindo))))
)

(defun segmenta-varias-igualmente (parametro naosegmentadas)
  (if (= (length naosegmentadas) 1)
      (list (segmenta-varias-igualmente-aux parametro (first naosegmentadas)))
    (cons (segmenta-varias-igualmente-aux parametro (first naosegmentadas))
          (segmenta-varias-igualmente parametro (rest naosegmentadas))))
)

(defun segmenta-varias-igualmente-aux (parametro naosegmentada)
  (if (= (length naosegmentada) 1)
      (list (segmenta-igual (first parametro) (first naosegmentada)))
    (cons (segmenta-igual (first parametro) (first naosegmentada))
          (segmenta-varias-igualmente-aux (rest parametro) (rest naosegmentada))))
)

(defun segmenta-varias-baseado-referencia (referencia intermediario)
  (if (= (length intermediario) 1)
      (list (segmenta-baseado-referencia '() (first referencia) (first intermediario))) ;o list foi posto na fase de testes, verificar se sera retirado
    (cons (segmenta-baseado-referencia '() (first referencia) (first intermediario))
          (segmenta-varias-baseado-referencia (rest referencia) (rest intermediario))))
)

(defun segmenta-baseado-referencia (lista referencia intermediario)
  (cond ((null referencia) (remove-position lista (length lista)))
        ((= (length (first referencia)) 1) 
         (if (equal '(nil) (last lista))
         (segmenta-baseado-referencia (insert-element nil (insert-element (first intermediario) (remove-position lista (length lista)))) (rest referencia) (rest intermediario))
           (segmenta-baseado-referencia (insert-element nil (insert-element (first intermediario) lista)) (rest referencia) (rest intermediario))))
        (t 
         (let ((resultado (segmenta-baseado-referencia-nao-unitaria lista (first referencia) intermediario)))
         (segmenta-baseado-referencia (insert-element nil (first resultado)) (rest referencia) (second resultado)))))
)

(defun segmenta-baseado-referencia-nao-unitaria (lista referencia intermediario) 
  (cond ((null referencia) (list lista intermediario))
        (t (if (equal '(nil) (last lista))
      (segmenta-baseado-referencia-nao-unitaria (insert-element (first intermediario) (remove-position lista (length lista))) (rest referencia) (rest intermediario))
    (segmenta-baseado-referencia-nao-unitaria (insert-element (concatenate 'list (first (last lista)) (first intermediario)) (remove-position lista (length lista))) (rest referencia) (rest intermediario)))))
)


