;; Auxiliary functions used in SOAL

;; ---------------------------------------------------------------------
;; Mathematical functions

(defun square (number)
  "This function squares a number"
  (* number number))

(defun cube (number)
  "O cubo de um numero"
  (* number number number))

(defun g-round (number &optional (decimals 0) (divisor 1))
  "Rounds a number. The optional parameters allow the user to choose the number of
   decimals of the result and to specify a divisor to the original number before the round."
  (float (/ (round (* number (expt 10 decimals)) divisor) (expt 10 decimals))))

(defun g-round-list (list &optional (decimals 2) (divisor 1))
  "Arredonda os numeros de dentro da lista com o numero decimals de casas decimais e
   os dividindo antes com o divisor"
  (cond ((null list) nil)
        ((= decimals 0) (cons (truncate (g-round (first list) decimals divisor))
                              (g-round-list (rest list) decimals divisor)))
        (t (cons (g-round (first list) decimals divisor)
                 (g-round-list (rest list) decimals divisor)))))

(defun round-midicents (midicents)
  "Arredonda o valor midicents que pode ser um atomo ou uma lista"
  (cond ((atom midicents) (* 100 (g-round midicents 0 100)))
        (t (multiplicacao-de-uma-lista-por-um-numero (g-round-list midicents 0 100) 100))))

(defun mediazeroparaum (lista)
    "Soma todos os elementos da lista e divide pelo tamanho da lista."
    "Caso essa lista tenha apenas um elemento, o resultado e zero."
    (if (= (length lista) 1)
      0.0
    (/
	(reduce #'+ lista)
	(* (length lista) 1.0))))

(defun media (lista)
  "Soma todos os elementos da lista e divide pelo tamanho da lista."
  (g-round (/
	(reduce #'+ lista)
	(* (length lista) 1.0)) 2))

(defun medialistadelistas (lista)
  "Recebe uma lista de listas e retorna a media de cada lista"
  (if (= (length lista) 1)
    (list (list (media (first lista))))
    (cons (list (media (first lista))) (medialistadelistas (rest lista)))))

(defun lista-absoluta (lista)
    "Função que retorna todos os números da lista em seu valor absoluto (i.e. positivo)"
    (mapcar #'abs lista))

(defun divisao-de-uma-lista-por-um-numero (lista numero)
    "Divide uma lista de números por um número qualquer."
    (cond ((null lista) nil)
	  (t (cons (g-round (/ (first lista) numero) 2)
		   (divisao-de-uma-lista-por-um-numero (rest lista) numero)))))

(defun divisao-de-um-numero-por-uma-lista (numero lista)
    "Constroi uma lista que eh a divisao de um numero por cada elemento da lista fornecida."
    (cond ((null lista) nil)
	  (t (cons (/ numero (first lista))
		   (divisao-de-um-numero-por-uma-lista numero (rest lista))))))

(defun subtracao-de-uma-lista-por-um-numero (lista numero)
    "Subtrai uma lista de números de um número qualquer."
    (cond ((null lista) nil)
	  (t (cons (- (first lista) numero)
		   (subtracao-de-uma-lista-por-um-numero (rest lista) numero)))))
;;feita por Carol
(defun subtracao-de-um-numero-por-uma-lista (numero lista)
    "Subtrai um numero qualquer de uma lista de números."
    (cond ((null lista) nil)
	  (t (cons (- numero (first lista))
		   (subtracao-de-um-numero-por-uma-lista numero (rest lista))))))

(defun soma-de-uma-lista-por-um-numero (lista numero)
    "Soma uma lista de números com um número qualquer."
    (cond ((null lista) nil)
	  (t (cons (+ (first lista) numero)
		   (soma-de-uma-lista-por-um-numero (rest lista) numero)))))

(defun multiplicacao-de-uma-lista-por-um-numero (lista numero)
    "Multiplica uma lista de números por um número qualquer."
    (cond ((null lista) nil)
	  (t (cons (* (first lista) numero)
		   (multiplicacao-de-uma-lista-por-um-numero (rest lista) numero)))))

(defun raiz-quadrada-recursiva (lista)
    "Extrai a raiz quadrada de todos os elementos da lista."
    (cond ((null lista) nil)
	  (t (cons (sqrt (first lista))
		   (raiz-quadrada-recursiva (rest lista))))))

(defun teste-zero-retorna-1 (numero)
    "Função que faz o teste zero (para usar em divisões). Caso o resultado seja zero, ela retorna 1."
    (if (= numero 0)
	1
	numero))
;; ---------------------------------------------------------------------

;; ---------------------------------------------------------------------
;; Convertions

(defun midicent2midi (lista)
    "Converts a list of midicents to a list of midi numbers."
    (cond ((null lista) nil)
	  (t (cons
		  (/ (first lista) 100)
		  (midicent2midi (rest lista))))))

(defun midi2midicent (lista)
    "Converts a list of midi numbers to a list of midicents."
    (cond ((null lista) nil)
	  (t (cons
		  (* (first lista) 100)
		  (midi2midicent (rest lista))))))

(defun mc->f-soal (midicents)
  "Transforma um valor ou uma lista de valores em midicents no equivalente em frequencia (Hz)"
  (cond ((null midicents) nil)
        ((atom midicents) (mc->f-equation midicents))
        (t (cons (mc->f-equation (first midicents))
                 (mc->f-soal (rest midicents))))))

(defun f->mc-soal (frequency)
  "Transforma um valor ou uma lista de valores em frequencia (Hz) no equivalente em midicents"
  (cond ((null frequency) nil)
        ((atom frequency) (f->mc-equation frequency))
        (t (cons (f->mc-equation (first frequency))
                 (f->mc-soal (rest frequency))))))


(defun mc->f-equation (midicents)
  "Equacao utilizada na funcao mc->f-soal, que faz a conversao de um valor midicent para
   seu equivalente em frequencia (Hz)"
  (g-round (* 261.6255653005986 (expt 2 (/ (- midicents 6000) 1200))) 13))


(defun f->mc-equation (frequency)
  "Equacao utilizada na funcao f->mc-soal, que faz a conversao de um valor em frequencia (Hz)
   para seu equivalente em midicent"
  (round (+ 6000 (* 1200 (log (/ frequency 261.6255653005986) 2)))))

(defun midi-or-midicents->midicents (notes)
  "This function verifies if the input is in midi or midicents. The output will always be in midicents."
  (if (> (first notes) 128)
    notes
    (multiplicacao-de-uma-lista-por-um-numero notes 100)))

(defun midi-or-midicents->midi (notes)
  "This function verifies if the input is in midi or midicents. The output will always be in midi."
  (if (<= (first notes) 128)
    notes
    (divisao-de-uma-lista-por-um-numero notes 100)))

;; ---------------------------------------------------------------------


;; ---------------------------------------------------------------------
;; Manipulações de listas

(defun diferenca-elementos-adjacentes (lista)    ; equivalente à função epw::x->dx
    "Cria uma lista onde cada elemento é a diferença entre um elemento da lista e seu
    sucessor imediato."
    (cond ((not (numberp (second lista))) nil)
	  (t (cons (- (first lista)
		      (second lista))
		  (diferenca-elementos-adjacentes (rest lista))))))

(defun distancias-lista-de-listas (lista)
  (if (= (length lista) 1)
      (if (= (length (first lista)) 1)
          (list (list 0))
        (list (diferenca-elementos-adjacentes (first lista))))
      (if (= (length (first lista)) 1)
          (cons (list 0) (distancias-lista-de-listas (rest lista)))
        (cons (diferenca-elementos-adjacentes (first lista))
          (distancias-lista-de-listas (rest lista)))))
)

(defun contador (lista)     ; Função utilizada em v-mode  (para otimizar verificar a função list-weight em stats e a função primitva count)
    "Cria uma lista onde cada posição da lista é uma sublista  da forma <x y>, onde y
    indica quantas vezes o elemento x ocorre na lista original."
    (cond ((null lista) nil)
	  (t (cons
		  (list (first lista) (count-soal (first lista) lista))
		  (contador (remove-soal (first lista) lista '()))))))

;;----------------------------------------------------------------------------------------
;;count-soal (funciona com numeros e strings) para usar no contador de list-weigth
;;criada por Carol
(defun count-soal (elemento lista)
  "Conta um elemento na lista e retorna esta quantidade. Funciona com numeros e strings"
  (length (elementos-repetidos elemento lista '())))

;;criada por Carol
(defun elementos-repetidos (elemento lista lista-nula)
  "Retorna uma lista com o 'elemento' repetido pelo numero de vezes e, que ele aparece na lista.
No parametro 'lista-nula' eh para colocar '() "
  (cond ((null lista) lista-nula)
        ((equal elemento (first lista))
         (elementos-repetidos elemento (rest lista) (acrescenta (first lista) lista-nula)))
        (t (elementos-repetidos elemento (rest lista) lista-nula))))

;;criada por Carol
(defun acrescenta (elemento lista)
  "Acrescenta o elemento a lista"
  (cond ((null lista) (list elemento))
        (t (cons (first lista)
                 (acrescenta elemento (rest lista))))))

;--------------------------------------------------------------------
;;remove-soal (funciona com numeros e strings) para usar no contador de list-weigth
;;criada por Carol
(defun remove-soal (elemento lista lista-nula)
  "Retorna uma lista sem o elementos que sejam iguais a 'elemento'."
  (cond ((null lista) lista-nula)
        ((not (equal elemento (first lista)))
         (remove-soal elemento (rest lista) (acrescenta (first lista) lista-nula)))
        (t (remove-soal elemento (rest lista) lista-nula))))

;--------------------------------------------------------------------

(defun desfaz-duplas (duplas)
    "Função que recebe uma lista de duplas <x,y> e retorna uma lista com todos os x da lista original."
    (cond ((null duplas) nil)
	  (t (cons (first (first duplas))
		   (desfaz-duplas (rest duplas))))))

(defun associador (list1 list2) ; um tipo de mat-trans
    "Função que recebe duas listas de mesmo tamanho e retorna uma lista onde as sublistas sao os elementos de mesmo índice nas listas."
    (cond ((null list1) nil)
	  (t (cons (list (first list1) (first list2))
		   (associador (rest list1) (rest list2))))))

(defun sort* (seq pred &key key)
    "Definição de sort não destrutivo - atua sobre uma cópia da entrada. Norvig p.312."
    (sort (copy-seq seq) pred :key key))

(defun ordem-decrescente (lista) ; esta função serve para simular g-max => (first (ordem-decrescente <lista>))
    "Função que recebe uma lista de números e a coloca em ordem decrescente."
    (sort* lista #'>))

(defun ordem-crescente (lista) ; esta função serve para simular g-min => (first (ordem-crescente <lista>))
    "Função que recebe uma lista de números e a coloca em ordem crescente."
    (sort* lista #'<))

(defun positivo? (lista)
    "Função que indica se cada posição na lista é um número positivo e maior que zero.
    1 = positivo. 0 = 0 ou negativo."
    (cond ((null lista) nil)
	  ((>= (first lista) 1)
	      (cons 1 (positivo? (rest lista))))
	  (t (cons 0 (positivo? (rest lista))))))

(defun max-lista (lista)
    "Função que retorna o maior número de uma lista."
    (cond ((eq (length lista) 1) (first lista))
	  (t (max (first lista)
		  (max-lista (rest lista))))))

(defun max-lista-de-listas (lista)
  (if (= (length lista) 1)
      (list (max-lista (first lista)))
    (cons (max-lista (first lista))
          (max-lista-de-listas (rest lista))))
)

(defun flatten (input &optional accumulator)
    "Return a flat list of the atoms in the input. Ex: (flatten '((a) (b (c) d))). Norvig, p.329."
    (cond ((null input) accumulator)
	((atom input) (cons input accumulator))
	(t (flatten (first input)
		    (flatten (rest input) accumulator)))))

(defun spliter (inicio fim) ; alterada por Nazareno e Ana Carolina
  "Cria uma lista de números de <inicio> até <fim>."
  (cond ((> inicio fim) nil)
        ((= fim inicio) (list fim))
        (t (cons inicio (spliter (+ inicio 1) fim)))))

(defun min-lista (lista)
    "Função que retorna o menor número de uma lista."
    (cond ((eq (length lista) 1) (first lista))
	  (t (min (first lista)
		  (min-lista (rest lista))))))

(defun min-lista-de-listas (lista)
  (if (= (length lista) 1)
      (list (min-lista (first lista)))
    (cons (min-lista (first lista))
          (min-lista-de-listas (rest lista))))
)

(defun index-lista (numero lista &optional (start 0))
  "Retorna o indice do numero na lista. O parametro start indica qual eh a primeira posicao"
  (cond ((null lista) nil)
        ((= numero (first lista)) start)
        (t (index-lista numero (rest lista) (+ start 1)))))

(defun index-lista-de-lista (lista lista-pesquisada &optional (start 0))
  "Retorna uma lista onde cada elemento corresponde ao indice do elemento da lista
   na lista-pesquisada. Start determina o indice inicial."
  (cond ((null lista) nil)
        (t (cons (index-lista (first lista) lista-pesquisada start)
                 (index-lista-de-lista (rest lista) lista-pesquisada start)))))

;;criada por Luana e Carol
(defun build-list (element count)
  "Constroi uma lista com o elemento element repetido count vezes."
  (cond ((= count 0) nil)
  (t (cons element
           (build-list element (- count 1))))))

;;---------------------------------------------------------------------------------
;;Funcoes que compoem a group-list
;;---------------------------------------------------------------------------------

(defun group-list-soal (list segm lecture)
  "Cria uma lista de listas agrupando os elementos originais segundo a forma da
   lista no parametro segm. Lecture informa se a leitura da lista serah linear
   ('lin ou 1) ou circular (circ ou qualquer outra coisa).
Ex.: ? (group-list-soal '(1 2 3 4 5 6) '(2 3 1) lin)
     ((1 2) (3 4 5) (6))
     ? (group-list-soal '(1 2 3 4 5 6) '(2 3 4) lin)
     ((1 2) (3 4 5) (6))
     ? (group-list-soal '(1 2 3 4 5 6) '(2 3 4) circ)
     ((1 2) (3 4 5) (6 1 2 3)) "

  (cond ((eq (first segm) nil) nil)
        ((or (eq lecture 'lin) (eq lecture 1)) (group-list-lin list segm))
        (t (group-list-circ list segm 0))))


(defun group-list-lin (list segm)
  "Faz o group-list-soal com lecture lin"
  (if (eq segm nil) nil
    (cons (group-segment list (first segm))
          (group-list-lin (nthcdr (first segm) list) (rest segm)))))

(defun group-list-circ (original segm offset)
  "Faz o group-list-soal com lecture circ. Usa o parametro original para guardar
   a lista original e poder percorre-la circularmente e o parametro offset
   para guardar a posicao atual na lista."
  (if (eq segm nil) nil
    (cons (group-segment-circular original (first segm) offset)
          (group-list-circ original (rest segm) (if (> (+ offset (first segm)) (length original))
                                                  (mod (+ offset (first segm)) (length original))
                                                  (+ offset (first segm))) ))))

(defun group-segment (list segm)
  "Make a list with the first 'segm' elements of a given list"
  (cond ((eq list nil) nil)
        ((eq segm 0) nil)
        (t (cons (first list) (group-segment (rest list) (- segm 1))))))

(defun group-segment-circular (original segm offset)
  "Make a list with the first 'segm' elements of a given list"
  (cond ((eq original nil) nil)
        ((eq (nthcdr offset original) nil) (group-segment-circular original segm 0)) ;; offset = 0 -> cabeca da lista
        ((<= segm 0) nil)
        (t (cons (first (nthcdr offset original))
                 (group-segment-circular original (- segm 1) (+ offset 1))))))

;;---------------------------------------------------------------------------------


;;---------------------------------------------------------------------------------
;; Listas e Series
;;---------------------------------------------------------------------------------

(defun dx->x-soal (start dxs)
  "Constructs a list of numbers from <start> with the consecutives intervals  of <dxs>.
For example (dx->x-soal  0  ‘(0 4 5 9 6 2 3 3))  will return  ? (0 0 4 9 18 24 26 29 32)
and (dx->x-soal  8  ‘(0 4 5 9 6 2 3 3)) will return  ?(8 8 12 17 26 32 34 37 40)"
  (cons start (dx->x-without-start start dxs)))

(defun dx->x-without-start (x dxs)
  "Usado por dx->x-soal. Faz a mesma coisa que ele sem incluir na lista retornada o start."
  (cond ((eq dxs nil) nil)
        (t (cons (+ (first dxs) x)
                 (dx->x-without-start (+ x (first dxs)) (rest dxs))))))

;;------------------------------------------------------------------------
;;criadas por Carol

(defun insert-position (lista-de-posicoes list-of-lists)
  "Insert each element of lista-de-posicoes in each list of list-of-lists."
  (cond ((equal lista-de-posicoes nil) nil)
        (t (cons (insert-element (first lista-de-posicoes) (first list-of-lists))
                 (insert-position (rest lista-de-posicoes) (rest list-of-lists))))))

(defun list-of-positions (length-of-list initial)
  "Builds a list with elements from initial to length-of-list - 1."
  (cond ((equal initial length-of-list) nil)
        (t (cons initial
                 (list-of-positions length-of-list (+ initial 1))))))

(defun insert-element (element list)
  "Insert an element in a list."
  (cond ((equal list nil) (cons element nil))
        (t (cons (first list)
                 (insert-element element (rest list))))))

(defun unit-list (list)
  "Informs if a list has a unique element."
  (cond ((equal (second list) nil) t)
	(t nil)))

;;------------------------------------------------------------------------
;Criadas por Hildegard

(defun listadelistasdenumeros->listadenumeros (lista)
"Como o nome sugere, transforma uma lista de listas de numeros em uma unica lista de numeros. As listas internas podem ter qualquer tamanho ou podem nem ser listas."
   (if (= (length lista) 1)
     (first lista)
     (if (or (equal (type-of (first lista)) 'fixnum) (equal (type-of (first lista)) 'double-float))
       (cons (first lista) (listadelistasdenumeros->listadenumeros (rest lista)))
       (if (= (length (first lista)) 1)
        (cons (first (first lista)) (listadelistasdenumeros->listadenumeros (rest lista)))
         (cons (first (first lista)) (listadelistasdenumeros->listadenumeros (cons (rest (first lista)) (rest lista))))))))

(defun inverso (list)
"Retorna os elementos da lista na ordem inversa à que entraram. Ex.: (1 2 3 4 5) vira (5 4 3 2 1)"
      (cond ((null list) nil)
            ((= (length list) 1) (list (first list)))
        (t (cons (first (last list)) (inverso (remove-position list (length list)))))))

(defun remove-position (lista posicao)
  "Remove um elemento da lista dada a posicao que ele ocupa na mesma"
  (remove-position-aux lista posicao 1))

(defun remove-position-aux (lista posicao ordemexecucao)
  (cond ((null lista) nil)
        ((= ordemexecucao posicao) (rest lista))
        (t (cons (first lista) (remove-position-aux (rest lista) posicao (+ ordemexecucao 1))))))

(defun multiplicar-listas-mesmo-tamanho (lista1 lista2)
  (cond ((or (null lista1) (null lista2)) nil)
        ((not (= (length lista1) (length lista2))) nil)
        (t (cons (g-round (* (first lista1) (first lista2)) 2) (multiplicar-listas-mesmo-tamanho (rest lista1) (rest lista2)
)))))

(defun insere-inicio (lista elemento)
  (inverso (insert-element elemento (inverso lista))))

(defun remove-duplicates-list-of-lists (lista)
  (if (= (length lista) 1)
      (list (remove-duplicates (first lista)))
    (cons (remove-duplicates (first lista))
          (remove-duplicates-list-of-lists (rest lista)))))

(defun somatorio (lista)
  (if (= (length lista) 1)
      (first lista)
    (+ (first lista) (somatorio (rest lista))))
)

(defun insere-posicao (lista elemento posicao)
  (insere-posicao-aux lista elemento posicao 0)
)

(defun insere-posicao-aux (lista elemento posicao indice)
  (if (= indice posicao)
      (insere-inicio lista elemento)
    (insere-inicio (insere-posicao-aux (rest lista) elemento posicao (+ indice 1)) (first lista))))

(defun fact-tail1 (number acc)
  (if (= number 1)
      (values acc)
      (progn (fact-tail1 (- number 1) (* number acc)))))

(defun fatoreal-tail (number)
  "The fatorial function with tails recursion"
  (fact-tail1 number 1))

(defun fatoreal (number)
  (if (< number 1)
      (values 1)
    (* number (fatoreal (- number 1)))))




