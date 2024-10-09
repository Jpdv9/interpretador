#lang eopl


;; Jean Paul Davalos Valencia
;; 1832375
;;
;; Wilson Andrés Mosquera Zapata
;; 2182116

;; https://github.com/Jpdv9/interpretador

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
;;            := Si <expresion> "{" <expresion>  "}" "sino" "{" <expresion> "}"
;;               condicional-exp (test-exp true-exp false-exp)
;;            := declarar ({<identificador> = <expresion> ';' }*)) { <expresion> }
;;               variableLocal-exp (ids exps cuerpo)
;;            := procedimiento (<identificador>*(',') ) "{" <expresion> "}"
;;               procedimiento-ex (ids cuerpo)
;;            := evaluar expresion (expresion *(","))    finEval
;;               <app-exp(exp exps)>
;;            := letrec {<identificador> ({<identificador}*(,)) = <expresion>}* in {<expresion>}
;;               <letrec-exp (proc-names idss bodies letrec-body)

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

;; Define la especificación del escáner para un intérprete simple
(define scanner-spec-simple-interpreter
  '(
    ;; Manejo de espacios en blanco
    (white-sp
     (whitespace) skip) 

    ;; Manejo de comentarios.
    (comment
     ("%" (arbno (not #\newline))) skip)

    ;; Manejo de identificadores
    (identificador
     ("@" letter (arbno (or letter digit "_"))) symbol)

    ;; Manejo de números: enteros positivos
    (numero
     (digit (arbno digit)) number) 

    ;; Manejo de números: enteros negativos
    (numero
     ("-" digit (arbno digit)) number) 

    ;; Manejo de números: números decimales positivos
    (numero
     (digit (arbno digit) "." digit (arbno digit)) number) 

    ;; Manejo de números: números decimales negativos
    (numero
     ("-" digit (arbno digit) "." digit (arbno digit)) number)

    ;; Manejo de texto: cadenas de caracteres encerradas entre comillas dobles
    (texto
      ("\"" (arbno letter) "\"") string)
    ))



;;=========================================================================
;;=========================================================================

;; Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (expression) a-program)
    (expression (numero) numero-lit)
    (expression (identificador) var-exp)
    (expression (texto) texto-lit)
    (expression ("(" expression primitive-bin expression ")") primapp-bin-exp)
    (expression (primitive-un "(" expression ")") primapp-un-exp)

    ;; IF
    (expression ( "Si" expression "{" expression "}" "sino" "{" expression "}" ) condicional-exp)

    ;; Variable Local
    (expression ( "declarar" "(" (arbno identificador "=" expression ";") ")" "{" expression "}") variableLocal-exp)

    ;; Procedimiento
    (expression ("procedimiento" "(" (separated-list identificador "," )  ")" "{" expression "}") procedimiento-exp)
    
    ;; Evaluar
    (expression ("evaluar" expression "(" (separated-list expression ",") ")" "finEval") app-exp)

    
    ;; Let Recursivo
    (expression ("letrec" (arbno "[" identificador "(" (separated-list identificador ",") ")" "=" expression "]") "in" expression) letrec-exp)

    
    ;; Primitivas binarias
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

    ;; Primitivas unarias
    (primitive-un ("longitud") primitiva-longitud)
    (primitive-un ("add1") primitiva-add1)
    (primitive-un ("sub1") primitiva-sub1)
    (primitive-un ("neg") primitiva-negacion-booleana)))


;;=========================================================================
;;=========================================================================

;; Definición de tipos de datos para la gramática
(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

;; Función para mostrar los tipos de datos definidos
(define show-the-datatypes
  (lambda () 
    (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;; Función que combina el escáner y el analizador para procesar cadenas de texto
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

      ; Caso 1: Número.
      (numero-lit (num) num)

      ; Caso 2: Texto.
      (texto-lit (txt) txt)

      ; Caso 4: Expresion de variable.
      (var-exp (id) (buscar-variable env id))

      ; Caso 5: Aplicación de operador binario.
      (primapp-bin-exp (exp1 prim-bin exp2)
                       ( apply-primitive-bin  prim-bin
                                        (list (eval-expression exp1 env)
                                              (eval-expression exp2 env))))

      ; Caso 6: Aplicacion de operador unario.
      (primapp-un-exp (prim-un exp)
                      ( apply-primitive-un prim-un
                                       (list (eval-expression exp env))))

      ; Caso 7: Expresión condicional.
      (condicional-exp (test-exp true-exp false-exp)        
                         (if (true-value? (eval-expression test-exp env))
                             (eval-expression true-exp env)
                             (eval-expression false-exp env)))
      
      ; Caso 8: Expresión de la variable local.
      (variableLocal-exp (ids exps cuerpo)
        (let* ((vals (map (lambda (e) (eval-expression e env)) exps))
               (nuevo-env (extend-env ids vals env)))
          (eval-expression cuerpo nuevo-env)))

      ; Caso 9: Expresión del procedimiento.
      (procedimiento-exp (ids cuerpo)
                         (cerradura ids cuerpo env))

      ; Caso 10: Aplicación del procedimiento.
      (app-exp (proc-exp args)
           (let* ((proc (eval-expression proc-exp env)) 
                  (arg-vals (map (lambda (arg) (eval-expression arg env)) args))
                  (new-env (extend-env (proc-val-ids proc) arg-vals (proc-val-env proc))))
             (eval-expression (proc-val-body proc) new-env)))

      ; Caso 11: Let Recursivo.
      (letrec-exp (proc-names idss bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))
      )))



;; Función para aplicar operaciones primitivas binarias
(define apply-primitive-bin
  (lambda (prim args)
    (cases primitive-bin prim
      ;; Operaciones binarias
      (primitiva-suma () (+ (car args) (cadr args)))                     ;; Suma de dos argumentos
      (primitiva-resta () (- (car args) (cadr args)))                    ;; Resta de dos argumentos
      (primitiva-multi () (* (car args) (cadr args)))                    ;; Multiplicación de dos argumentos
      (primitiva-div () (/ (car args) (cadr args)))                      ;; División de dos argumentos
      (primitiva-concat () (string-append (car args) (cadr args)))       ;; Concatenación de cadenas

      ;; Comparaciones binarias
      (primitiva-mayor () (valor-verdad? (if (> (car args) (cadr args)) 1 0)))  ;; Mayor que
      (primitiva-menor () (valor-verdad? (if (< (car args) (cadr args)) 1 0)))  ;; Menor que
      (primitiva-mayor-igual () (valor-verdad? (if (>= (car args) (cadr args)) 1 0)))  ;; Mayor o igual que
      (primitiva-menor-igual () (valor-verdad? (if (<= (car args) (cadr args)) 1 0)))  ;; Menor o igual que
      (primitiva-comparador-igual () (valor-verdad? (if (= (car args) (cadr args)) 1 0)))  ;; Igual a
      (primitiva-diferente () (valor-verdad? (if (not (= (car args) (cadr args))) 1 0)))  ;; Diferente de

      (else (eopl:error 'apply-primitive "Operador primitivo desconocido: ~s" prim)))))  ;; Manejo de errores



;; Función para aplicar operaciones primitivas unarias
(define apply-primitive-un
  (lambda (prim args)
    (cases primitive-un prim
      ;; Operaciones unarias
      (primitiva-add1 () (+ (car args) 1))                              ;; Incrementa el argumento en 1
      (primitiva-sub1 () (- (car args) 1))                             ;; Decrementa el argumento en 1
      (primitiva-longitud () (string-length (car args)))               ;; Devuelve la longitud de una cadena
      (primitiva-negacion-booleana () (valor-verdad? (not (car args)))) ;; Negación booleana del argumento

      (else (eopl:error 'apply-primitive "Operador primitivo desconocido: ~s" prim))))) ;; Manejo de errores

      

;; Función que determina el valor de verdad 
(define valor-verdad?
  (lambda (x)
    (if (zero? x ) 0 1)))


;; Función que verifica si un valor es verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))))


;; Definición del tipo de dato 'procval' para representar procedimientos
(define-datatype procval procval?
  (cerradura
   (lista-id (list-of symbol?))
   (exp expression?)
   (amb environment?)))


;; Función para aplicar una cerradura (closure)
(define apply-closure
  (lambda (proc args)
    (cases procval proc
      (cerradura (ids cuerpo amb)
                 (let ((nuevo-env (extend-env ids args amb)))
                   (eval-expression cuerpo nuevo-env))))))


;; Función que obtiene la lista de identificadores de una cerradura (closure)
(define proc-val-ids
  (lambda (proc)
    (cases procval proc
      (cerradura (ids exp env) ids))))

 
;; Función que obtiene el cuerpo (expresión) de una cerradura (closure)
(define proc-val-body
  (lambda (proc)
    (cases procval proc
      (cerradura (ids exp env) exp))))
  

;; Función que obtiene el entorno de una cerradura (closure
(define proc-val-env
  (lambda (proc)
    (cases procval proc
      (cerradura (ids exp env) env))))


;;=========================================================================
;;=========================================================================
  
;; Interpretador
(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))


;; eval-program: <programa> -> numero
;; Función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env))))))


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


;; Definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?))
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expression?))
                                   (env environment?)))


;; Función que verifica si un valor es de tipo 'scheme-value'
(define scheme-value? (lambda (v) #t))

  
;; empty-env:      -> enviroment
;;Función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;; extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;; Función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

;; extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;; Función que crea un ambiente extendido para procedimientos recursivos
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
                                             (cerradura (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env) 
                                             (buscar-variable old-env sym)))))))




;;=========================================================================
;Funciones Auxiliares

;; Funciones auxiliares para encontrar la posición de un símbolo
;; en la lista de símbolos de unambiente

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
#|

Pruebas evaluadas:

===========================================================================

A) Escriba un programa en su lenguaje de programación que contenga un
   procedimiento @sumarDigitos que le permita sumar los dígitos de un número
   entero positivo. evaluar @sumarDigitos(147) finEval deberá retornar 12.

letrec [
        @dividirEntero(@numerador , @denominador) =
          Si (@numerador < @denominador) {
            0
          } sino {
            (1 + evaluar @dividirEntero((@numerador ~  @denominador),@denominador) finEval)
       }]

      [
       @sumarDigitos(@valor) =
         Si (@valor < 10) {
           @valor
         } sino {
           ((@valor ~ (evaluar  @dividirEntero(@valor, 10) finEval * 10)) + evaluar @sumarDigitos(evaluar  @dividirEntero(@valor, 10) finEval) finEval)
       }]
            
       in 
         evaluar  @sumarDigitos(147) finEval


===========================================================================

B) Escriba un programa en su lenguaje de programación que contenga un
   procedimiento que permita calcular el factorial de un número n. Como la
   gramática para funciones recursivas debe ser propuesta por el grupo,
   incluya dos ejemplos de uso para el factorial de 5 y el factorial de 10.

FACTORIAL DE 5:

letrec [ @factorial (@number) = Si (@number == 0) {1} sino {(@number * evaluar @factorial (sub1(@number)) finEval)}]
       in
        evaluar @factorial (5) finEval


FACTORIAL DE 10:

letrec [ @factorial (@number) = Si (@number == 0) {1} sino {(@number * evaluar @factorial (sub1(@number)) finEval)}]
       in
        evaluar @factorial (10) finEval

===========================================================================

C) Escriba un programa en su lenguaje de programación que contenga un
   procedimiento que permita calcular una potencia de forma recursiva
   @potencia(base, exponente). Si no se evidencia el uso de recursión, el
   ejercicio no será valido. Incluya un llamado a la función recursiva:
   "evaluar @potencia (4, 2) finEval " que retornaría 16.

letrec [ @potencia(@base, @exponente) = Si (@exponente == 0) {1} sino {(@base * evaluar @potencia(@base, (@exponente ~ 1)) finEval)}]
          in 
            evaluar @potencia(4,2) finEval

;;=========================================================================

D) Escriba un programa que sume los números en un rango de valores positivos
   [a,b], donde siempre se cumple en la invocación a < b:  Por ejemplo
   "evaluar @sumaRango (2, 5) finEval  "  retornaría 14.

letrec [ @sumaRango (@num1, @num2) = Si (@num1 > @num2) {0} sino {(@num1 + evaluar @sumaRango ((@num1 + 1) , @num2) finEval)}]
          in
            evaluar @sumaRango (2 , 5) finEval

;;=========================================================================

E) Crea una función @integrantes que muestre los nombres de los integrantes
   del grupo y adicionalmente crea un decorador que al invocarlo salude
   a los integrantes.




;;=========================================================================

F)  Modifique el ejercicio anterior para que el decorador reciba como
    parámetro otro mensaje que debe ponerse al final de todo el string.
    Ejemplo:
    // Invocación del decorador
    evaluar @decorate ("_ProfesoresFLP") finEval
    // Deberá retornar "Hola:Robinson_y_Sara_ProfesoresFLP"



;;=========================================================================



|#


(interpretador)