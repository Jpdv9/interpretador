#lang eopl


;; Jean Paul Davalos Valencia
;; 1832375
;;
;; Wilson Andrés Mosquera Zapata
;; 2182116

;;=========================================================================
;;=========================================================================

;;Gramatica

;;Valores denotados: Texto + Número + Booleano + ProcVal
;;Valores expresado: Texto + Número + Booleano + ProcVal

;; <programa> :=  <expresion>
;;               un-programa (exp)
;; <expresion> := <numero>
;;               numero-lit (num)
;;            := "\""<texto> "\""
;;               texto-lit (txt)
;;            := <identificador>
;;               var-exp (id)
;;            := (<expresion> <primitiva-binaria> <expresion>)
;;               primapp-bin-exp (exp1 prim-binaria exp2)
;;            := <primitiva-unaria> (<expresion>)
;;               primapp-un-exp (prim-unaria exp)

;; <primitiva-binaria> :=  + (primitiva-suma)
;;  :=  ~ (primitiva-resta)      
;;  :=  / (primitiva-div)
;;  :=  * (primitiva-multi)
;;  :=  concat (primitiva-concat)
;;  := > (primitiva-mayor)
;;  := < (primitiva-menor)
;;  := >= (primitiva-mayor-igual)
;;  := <= (primitiva-menor-igual)
;;  := != (primitiva-diferente)
;;  := == (primitiva-comparador-igual)

;; <primitiva-unaria> :=  longitud (primitiva-longitud)
;;            :=  add1 (primitiva-add1)
;;            :=  sub1 (primitiva-sub1)
;;            := neg (primitiva-negacion-booleana)

;;=========================================================================
;;=========================================================================

;; Especificación Léxica

(define scanner-spec-simple-interpreter
  '((white-sp
     (whitespace) skip)
    (comment
     ("%" (arbno (not #\newline))) skip)
    (identifier
     ("@" letter (arbno (or letter digit "_")))  symbol)
    (numero
     (digit (arbno digit)) number)
    (numero
     ("-" digit (arbno digit)) number)
    (decimal
    (digit (arbno digit) "." digit (arbno digit)) number)
    (decimal
     ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (texto
      ("_" (arbno letter) "_") string)
    ;;(boolean
     ;;("#t" "#f") boolean)
    ))

;;=========================================================================
;;=========================================================================

;; Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (expression) a-program)
    (expression (numero) numero-lit)
    (expression (identifier) var-exp)
    (expression (texto) texto-lit)
    (expression ("(" expression primitive-bin expression ")") primapp-bin-exp)
    (expression (primitive-un "(" expression ")") primapp-un-exp)

   


    ;; primitivas binarias
    (primitive-bin ("+") primitiva-suma)
    (primitive-bin ("~") primitiva-resta)
    (primitive-bin ("/") primitiva-div)
    (primitive-bin ("*") primitiva-multi)
    (primitive-bin ("concat") primitiva-concat)
    (primitive-bin (">") primitiva-mayor)
    (primitive-bin ("<") primitiva-menor)
    (primitive-bin (">=") primitiva-mayor-igual)
    (primitive-bin ("<=") primitiva-menor-igual)
    (primitive-bin ("!=") primitiva-diferente)
    (primitive-bin ("==") primitiva-comparador-igual)

    ;; primitivas unarias
    (primitive-un ("longitud") primitiva-longitud)
    (primitive-un ("add1") primitiva-add1)
    (primitive-un ("sub1") primitiva-sub1)
    (primitive-un ("neg") primitiva-negacion-booleana)))


;;=========================================================================
;;=========================================================================

;; Definición de tipos de datos para la gramática

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;; Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;;=========================================================================
;;=========================================================================

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (numero-lit (datum) datum) 
      (texto-lit (txt) txt) 
      (var-exp (id) (buscar-variable env id))
      (primapp-bin-exp (exp1 prim-bin exp2) 
                       (let ((val1 (eval-expression exp1 env))
                             (val2 (eval-expression exp2 env)))
                         (cond
                           [(eq? prim-bin '+) (+ val1 val2)]
                           [(eq? prim-bin '(-) (if (number? val2) (- val1 val2) (eopl:error "Subtraction requires numeric values")))]
                           [(eq? prim-bin '(/) (/ val1 val2))]
                           [(eq? prim-bin '(*)) (* val1 val2)]
                           [(eq? prim-bin 'concat) (string-append val1 val2)]
                           [(eq? prim-bin '> ) (> val1 val2)]
                           [(eq? prim-bin '< ) (< val1 val2)]
                           [(eq? prim-bin '>=) (>= val1 val2)]
                           [(eq? prim-bin '<=) (<= val1 val2)]
                           [(eq? prim-bin '!=) (not (= val1 val2))]
                           [(eq? prim-bin '==) (= val1 val2)]
                           [else (eopl:error "Unknown binary operator" prim-bin)])))
      (primapp-un-exp (prim-un exp)
                      (let ((val (eval-expression exp env)))
                        (cond
                          [(eq? prim-un 'longitud) (if (string? val) (string-length val) (eopl:error "Length requires a string"))]
                          [(eq? prim-un 'add1) (if (number? val) (+ val 1) (eopl:error "add1 requires a number"))]
                          [(eq? prim-un 'sub1) (if (number? val) (- val 1) (eopl:error "sub1 requires a number"))]
                          [(eq? prim-un 'neg) (not val)]
                          [else (eopl:error "Unknown unary operator" prim-un)]))))))




;;=========================================================================
;;=========================================================================
;; Interpretador
(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))


;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env))))))


;;=========================================================================
;;=========================================================================


;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))


;;=========================================================================
;;=========================================================================

;Ambientes


;; Ambiente inicial
(define init-env
  (lambda ()
    (extend-env
     '(@a @b @c @d @e)
     (list 1 2 3 "hola" "FLP")
     (empty-env))))



;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?))
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expression?))
                                   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))


;; Buscar-variable
(define buscar-variable
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        ("Error, la variable no existe"))
      (extended-env-record (syms vals old-env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (buscar-variable old-env sym)))) 
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names))) 
                                         (if (number? pos)
                                             (closure (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env) 
                                             (buscar-variable old-env sym)))))))


;;=========================================================================
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;;=========================================================================

;;=========================================================================
;;=========================================================================









