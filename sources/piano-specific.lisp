;; Piano specific algorithms (k v q);; ---------------------------------------------------------------------;; functions y1, y2 e y3(defun y1 (number)  "Funcao que calcula o numero de parciais gerado por uma nota.   Input: midi number.    Output: numeros de parciais presentes."   (+    (+ dy1 (* *cy1* number))    (+ (* by1 (square number))       (* ay1 (cube number)))))(defun y2 (number)  "Funcao que calcula o rang(??) dos parciais  de mais forte intesidade relativa.   Input: midi number.    Output: ??."   (+    (+     (+ (* ay2 (cube number)) (* by2 (square number)))    (* cy2 number))   dy2))(defun y3 (number)  "Funcao que calcula a taxa de amortecimento/declinio de uma nota tocada no piano.   Input: midi number.   output: taxa de amortecimento (em segundos)."   (+    (+ dy3 (* cy3 number))    (+ (* by3 (square number))       (* ay3 (cube number)))));; ---------------------------------------------------------------------;; ---------------------------------------------------------------------;; k(defun k (midicent)  "Funcao que calcula qualidade timbral de uma nota em funcao de sua altura absoluta.    O resultado e' relativo 'a nota La0 que possui qualidade timbral maxima (1.0).   Input: midicent   Output: qualidade timbral relativa."   (let ((midi-aux (midicent2midi midicent)))       (k-aux midi-aux)))(defun k-aux (midi-aux)    "Fun��o auxiliar para k. Recurs�o � feita aqui."       (cond ((null midi-aux) nil)	     ((< (first midi-aux) 61)		 (cons (/ 		      (+ 			  (y1 (first midi-aux)) 			  (y2 (first midi-aux)) 			  (y3 (first midi-aux))) 		      k1)		  (k-aux (rest midi-aux))))	  (t (cons (/ 		      (+ (y1 (first midi-aux)) 			 k2 			 (y3 (first midi-aux))) 		      k1)		   (k-aux (rest midi-aux))))))(defun k-average (midicents)    "Fun��o que define a m�dia dos k's a partir dos midicents."    (/ (reduce #'+ (k midicents))       (length midicents)));; ---------------------------------------------------------------------;; ---------------------------------------------------------------------;; v(defun v (velo)  "Funcao que calcula ...   Input: lista de velocidade midi    output: ??"  (cond ((null velo) nil)        ((= (first velo) 0) (cons 0.0 (v (rest velo))))	(t (cons 		(+ v1 		   (* v2 (first velo)) 		   (square (* v3 (first velo))))	        (v (rest velo))))))(defun v-mode-piano (velos)    "Retorna a velocidade mais frequente na lista. Privilegia a intensidade mais forte, pois os    eventos mais fortes tem mais incidencia no timbre do que os mais fracos."       (let* ((duplas (contador velos))	   (mode-results (v-mode-aux duplas)))       (if (= (second (first mode-results)) 1)     ; verifica se no resultado a velocidade mais freq�ente � �nica	   (media velos)                           ; caso seja amodal o retorno de v-mode-aux sera ((yz 1)) 	   (first (first mode-results)))))         ; em caso de valores iguais o maior e' o valido - vide doc (defun v-mode-aux (duplas)    "Fun��o auxiliar para v-mode."    (cond ((null (second duplas)) (list (first duplas)))	  ((< (second (first duplas))               (second (second duplas)))                 (v-mode-aux (remove (first duplas) duplas)))        	  ((> (second (first duplas))               (second (second duplas)))                 (v-mode-aux (remove (second duplas) duplas)))       ;ate aqui remove as velocicades - frequentes      	            (t (if (< (first (first duplas)) (first (second duplas)))  ;caso tenha + de 1 vel com mesma freq                 (v-mode-aux (remove (first duplas) duplas))         ;fica na lista a de vel maior                 (v-mode-aux (remove (second duplas) duplas))))  ));; ---------------------------------------------------------------------;; ---------------------------------------------------------------------;; q(defun q-aux (midicent velo)    "Fun��o que calcula q. As entradas s�o duas listas de igual tamanho uma com     os midicents e a outra com as velocidades."    (if (> (length velo) 1)	(q-aux-midicents-e-velos-iguais midicent velo)        (q-aux-velo midicent velo)))(defun q-aux-velo (midicent velo)    "Fun��o que implementa o bypass para uma lista de velicidades de tamanho 1."    (cond ((null midicent) nil)	  (t (cons (/		      (* (first (k (list (first midicent))))			 (first (v (list (first velo)))))		     2)		 (q-aux-velo (rest midicent) velo)))))(defun q-aux-midicents-e-velos-iguais (midicent velo)    "Fun��o que calcula q quando as listas de entrada possuem o mesmo tamanho."    (cond ((null midicent) nil)          (t (cons (/		      (* (first (k (list (first midicent)))) 		      (first (v (list (first velo))))) 		    2) 		  (q-aux-midicents-e-velos-iguais (rest midicent) (rest velo))))))    (defun sum-q (midicents velos pedal rounding)    "Calcula o q de um objeto. Vide f�rmula na p�gina 182 da tese."    (let* ((aux  (q-aux midicents velos))	   (soma (* (g-round		      (/ 			  (+ 			      (* (reduce #'max aux) 				 (sqrt (length midicents)))			     (- (reduce #'+ aux)				(reduce #'max aux)))			(length midicents))		    rounding)		(ped pedal))))      (if (> soma 1.0)	  1.0	  soma)))(defun q-gaps (midicents velos)    "Average value of intervallic gaps between adjacent q values."    (let ((aux (q-aux midicents velos)))	(abs 	    (/		(reduce #'+ (mapcar #'abs (diferenca-elementos-adjacentes aux)))		(length aux)))));; ---------------------------------------------------------------------;; ---------------------------------------------------------------------;; ped(defun ped (midi-ped)      "Conventions:         una corda = 32     a due corde = 48    ordinario = 64    1/4 Ped = 80    1/3 Ped = 86    1/2 Ped = 96    Ped+UC simult = 96    3/4 Ped = 112    Ped = 127 or 128         Suggested Midi controller #4. Sost. pedal = Ped (for the selected notes)."  (g-round midi-ped 1 64))   ;; --------------------------------------------------------------------- 