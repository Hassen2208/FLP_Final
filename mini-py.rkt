#lang eopl 

;;Integrantes
;; Marcela Mazo - 1843612
;;Diana Carolina Micolta - 2028287
;;Hassen Ortiz - 
;;Kevin -
;;Repositorio: https://github.com/Hassen2208/FLP_Final.git

;;-------------------------- mini-py-------------------------------------------------------------------------
;;--------------------------Grámatica---------------------------------------------------------------------------
;;<mini-py>          ::= <expresion>
;;                      <minipy-program (exp)>
;;<expresion>       ::= <numero>
;;                      <num-exp (datum)>
;;                  ::= x32 ({<entero>}*)
;;                      <big-exp (lsnum)>
;;                  ::= x16( {<numero>}* )
;;                      <hex-exp (lsnum)>
;;                  ::= x8 ({<entero>}*)
;;                      <oct-exp (lsnum)>
;;                  ::= '<caracter>'
;;                      <caracter-exp (caracter)>
;;                  ::= "<cadena>"
;;                      <cadena-exp (cadena)>
;;                  ::= <identificador>
;;                      <identificador-exp (id)>
;;                  ::= &<identificador>
;;                      <refid-exp (id)>                  
;;                  ::= var {<identificador> = <expresion>}*(,) in <expresion>
;;                      <var-exp (ids exps cuerpo)>;;
;;                  ::= const {<identificador> = <expresion>}*(,)
;;                      <const-exp (ids exps cuerpo)> in <expresion>
;;                  ::= rec  {<identificador> ({<identificador>}*(,)) = <expresion>}* in <expresion>
;;                      <rec-exp (lproc ids cuerpos cuerporec)>
;;                  ::= <lista>
;;                      <lista-exp (lista)>
;;                  ::= <exp-bool>
;;                      <bool-exp (exp-bool)>
;;                  ::= begin {<expresion>}+(;) end
;;                      <begin-exp (exp lexps)>
;;                  ::= if <expr-bool> then <expresion> else <expresion> end
;;                      <if-exp (expb exp1 exp2)>
;;                  ::= while <expr-bool> do
;;                      <expresion> done
;;                  ::= for <identificador> = <expresion> (to j downto ) <expresion> do
;;                        <expresion> done
;;                  ::= <prim-bin> (expresion , expresion)
;;                      <primbin-exp (lexp)>
;;                  ::= <prim-un> (expresion)
;;                      <primun-exp (lexp)>
;;                  ::= proc({<identificador>}*(,)) <expresion>
;;                      <proc-exp (ids body)>
;;                  ::= (<expresion> {expression}*)
;;                      <app-exp (expresion lexps)>

;;-----------------------primitivas binarias------------------------
;;<prim-bin>        ::= + | - | * | % | / | +x8 | -x8 | *x8 |+x16 | -x16 | *x16| +x32 | -x32 | *x32
;;                  ::= cons | append
;;                  ::= concat
;;-----------------------privimitivas unarias-----------------------
;;<prim-un>         ::= lenght 
;;                  ::= add1 | sub1 | add1x8 | sub1x8| add1x16 | sub1x16| add1x32 | sub1x32
;;                  ::= empty? | list? | car | cdr
;;------------------------------------------------------------------
;;<lista>           ::= empty
;;                      <empty-list>
;;                  ::= [{<expresion>}*(,)]
;;                      <lista1 (lexps)>
;;<exp-bool>        ::= <pred-prim> (<expresion> , <expresion>)
;;                      <comparacion (pprim exp1 exp2)>
;;                  ::= <oper-bin-bool> (<exp-bool> , <exp-bool>)
;;                      <conjuncion (obbool expb1 expb2)>
;;                  ::= <bool>
;;                      <vlr-bool (bool)>
;;                  ::= <oper-un-bool> (<expr-bool>)
;;                      <op-comp (oubool expb)>
;;<pred-prim>       ::= <|>|<=|>=|==|!=
;;<oper-bin-bool>   ::= and|or
;;<oper-un-bool>    ::= not
;;<bool>            ::= true | false


;;--------------------------------------------------------Lexico---------------------------------------------
(define scanner-interpreter
'(
  (white-sp (whitespace) skip)
  (comentario ("//" (arbno (not #\newline))) skip)
  (identificador (letter (arbno (or letter digit))) symbol)
  (cadena (#\" any (arbno (not #\")) #\") string)
  (numero (digit (arbno digit)) number)
  (numero ("-" digit (arbno digit)) number)
  (numero (digit (arbno digit) "." digit (arbno digit)) number)
  (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
 )
)

;;--------------------------------------------------------Especificacion sintactica---------------------------------------------

(define grammar-interpreter
  '(
    ;------------ Expresiones basicas -------------
    (programa ((arbno clase-declarada) expresion) a-programa)

    ;------------ Identificadores -------------
    (expresion (identificador) id-exp)

    ;------------ Definir Variables -------------
    (expresion ("var" (separated-list identificador "=" expresion ",") "in" expresion) var-exp)
    (expresion ("const" (separated-list identificador "=" expresion ",") "in" expresion) const-exp)
    (expresion ("rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion) "in" expresion) letrec-exp)

    ;------------ Datos -------------
    (expresion (numero) numero-lit)
    (expresion (cadena) cadena-exp)
    (expresion ("x8" "(" (arbno numero) ")") oct-exp)
    (expresion ("x16" "(" (arbno numero) ")") hex-exp)
    (expresion ("x32" "(" (arbno numero) ")") bignum-exp)

    ;------------ Constructor de datos predefinidos -------------
    (expresion ("[" (separated-list expresion ",") "]") list-exp)
    (expresion ("tupla" "[" (separated-list expresion ";") "]") tupla-exp)
    (expresion ("{" identificador "=" expresion (arbno "," identificador "=" expresion) "}") registro-exp)
    (expr-bool (pred-prim "(" expresion "," expresion ")") pred-prim-exp)
    (expr-bool (oper-bin-bool "(" expr-bool "," expr-bool ")") oper-bin-exp)
    (expr-bool (oper-un-bool "(" expresion ")") oper-un-exp)
    (expr-bool (bool) bool-exp)
    (bool ("true") bool-true)
    (bool ("false") bool-false)

    ;------------ Estrucutras de control -------------
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("if" expr-bool "then" expresion "else" expresion "end") if-exp)
    (expresion ("while" expr-bool "do" expresion "done") while-exp)
    (expresion ("for" identificador "=" expresion iterador expresion "do" expresion "done") for-exp)
    (iterador ("to") iter-to)
    (iterador ("downto") iter-down)

    ;------------ Primitivas aritmeticas para enteros -------------
    (expresion ("(" expresion primitiva-binaria expresion ")") primapp-bin-exp)
    (expresion (primitiva-unaria "(" expresion ")") primapp-un-exp)

    ;------------ Primitivas sobre cadenas -------------
    (expresion ("longitud" "(" expresion ")") primitiva-longitud)
    (expresion ("concat" "(" expresion "," expresion ")") primitiva-concat)

    ;------------ Primitivas sobre listas -------------
    (expresion ("vacio?" "(" expresion ")") vacio?-exp)
    (expresion ("vacio") vacio-exp)
    (expresion ("crear-lista" "(" expresion (arbno "," expresion) ")") crear-lista-exp)
    (expresion ("lista?" "(" expresion ")") list?-exp)
    (expresion ("cabeza" "(" expresion ")") cabeza-exp)
    (expresion ("cola" "(" expresion ")") cola-exp)
    (expresion ("append" "(" expresion "," expresion ")") append-exp)
    (expresion ("ref-list" "(" expresion "," expresion ")") ref-list-exp)
    (expresion ("set-list" "(" expresion "," expresion "," expresion ")") set-list-exp)

    ;------------ Primitivas sobre tuplas -------------
    (expresion ("crear-tupla" "(" expresion (arbno "," expresion) ")") crear-tupla-exp)
    (expresion ("tupla?" "(" expresion ")") tupla?-exp)
    (expresion ("ref-tupla" "(" expresion "," expresion ")") ref-tupla-exp)
    (expresion ("cabeza-tupla" "(" expresion ")") cabeza-tupla-exp)
    (expresion ("cola-tupla" "(" expresion ")") cola-tupla-exp)

    ;------------ Primitivas sobre registros -------------
    (expresion ("registro?" "(" expresion ")") registro?-exp)
    (expresion ("crear-registro" "(" identificador "=" expresion (arbno "," identificador "=" expresion) ")") crear-registro-exp)
    (expresion ("ref-registro" "(" expresion "," expresion ")") ref-registro-exp)
    (expresion ("set-registro" "(" expresion "," expresion "," expresion ")") set-registro-exp)

    ;------------ Invocación de procedimientos -------------
    (expresion ("function" "(" (separated-list identificador ",") ")" "{" expresion "}") procedimiento-exp)
    (expresion ("evaluar" "(" (separated-list expresion ",") ")") evaluar-exp)
    (expresion ("&" identificador) referencia-exp)

    ;------------ Variables actualizables -------------
    (expresion ("set" identificador "=" expresion) set-exp)

    ;------------ Primitivas -------------
    (pred-prim (">") mayor-exp)
    (pred-prim (">=") mayor-igual-exp)
    (pred-prim ("<") menor-exp)
    (pred-prim ("<=") menor-igual-exp)
    (pred-prim ("==" ) igual-exp)
    (pred-prim ("!=") diferente-exp)

    ;------------ Operadores booleanos -------------
    (oper-bin-bool ("and") primitiva-and)
    (oper-bin-bool ("or") primitiva-or)

    (oper-un-bool ("not") primitiva-not)

    ;------------ Primitivas binarias -------------
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("-") primitiva-resta)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("%") primitiva-mod)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)

    ; Octales
    (primitiva-binaria ("+(x8)") oct-suma)
    (primitiva-binaria ("~(x8)") oct-resta)
    (primitiva-binaria ("*(x8)") oct-multi)
    (primitiva-unaria ("add1(x8)") oct-add1)
    (primitiva-unaria ("sub1(x8)") oct-sub1)

    ; Hexadecimales
    (primitiva-binaria ("+(x16)") hex-suma)
    (primitiva-binaria ("~(x16)") hex-resta)
    (primitiva-binaria ("*(x16)") hex-multi)
    (primitiva-unaria ("add1(x16)") hex-add1)
    (primitiva-unaria ("sub1(x16)") hex-sub1)

    ; Base 32
    (primitiva-binaria ("+(x32)") big-suma)
    (primitiva-binaria ("~(x32)") big-resta)
    (primitiva-binaria ("*(x32)") big-multi)
    (primitiva-unaria ("add1(x32)") big-add1)
    (primitiva-unaria ("sub1(x32)") big-sub1)

    ; Imprimir
    (expresion ("print" "(" expresion ")") print-exp)

    ;------------ Producciones OOP -------------
    (clase-declarada
      ("class" identificador
       "extends" identificador ":"
       (arbno "field" identificador)
       (arbno method-decl))
      a-clase-declarada)

    (method-decl
      ("define" identificador
       "(" (separated-list identificador ",") ")"
       expresion)
      a-method-decl)

    (expresion ("mostrar") mostrar-exp)

    (expresion
      ("new" identificador "(" (separated-list expresion ",") ")")
      new-object-exp)

    (expresion
      ("send" expresion identificador
       "(" (separated-list expresion ",") ")")
      method-app-exp)

    (expresion
      ("super" identificador "(" (separated-list expresion ",") ")")
      super-call-exp)
    ))

;------------------------------------------------------------------------------------------------------------------------
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)
;;letrec fact(n) = Si n entonces (n * (fact sub1(n))) sino 1 finSi in (fact 20)
(define scan&parse
  (sllgen:make-string-parser scanner-interpreter grammar-interpreter))

;El Analizador Léxico (Scanner)
(define just-scan
  (sllgen:make-string-scanner scanner-interpreter grammar-interpreter))

(sllgen:make-define-datatypes scanner-interpreter grammar-interpreter)

(define list-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-interpreter grammar-interpreter)))


;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-interpreter
      grammar-interpreter)))

;------------------------------------------------------------------------------------------------------------------------
;El Interprete
;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)
(define eval-program
  (lambda (pgm)
    (cases programa pgm
      (un-programa (body)
                 (evaluar-expresion body init-env)))))
;Definición tipos de datos referencia y blanco
(define-datatype target target?
  (direct-target (expval expval?))
  (const-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?)))
(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

;;--------------------------------------------------------Appilar y Evaluar-expresion---------------------------------------------

;eval-expression: <expression> <enviroment> -> numero | string
; evalua la expresión en el ambiente de entrada
(define evaluar-expresion
  (lambda (exp env)
    (cases expresion exp
      ;Numero
      (numero-lit (numero) numero)
      ;Texto
      (cadena-exp (cadena) (substring cadena 1 (- (string-length cadena) 1)))
      ;Identificadores
      (id-exp (id) (apply-env env id))    
      (var-exp (vars rands body)
               (let ((args (eval-rands rands env)))
                 (evaluar-expresion body (extended-env-record vars (list->vector args) env))))
      (const-exp (ids rands body)
                (let ((args (map (lambda (x) (const-target (evaluar-expresion x env))) rands)))
                   (evaluar-expresion body (extended-env-record ids (list->vector args) env))))
      ;Referencia
      (referencia-exp (id) (apply-env-ref env id))
      ;Set
      (set-exp (id exp)(begin
                         (setref! (apply-env-ref env id)
                                  (evaluar-expresion exp env))
                        "Se ha actualizado la variable"))
      ;Primitivas unarias
      (primapp-un-exp (prim rand)
                   (apply-primitive-un prim (evaluar-expresion rand env)))
      ;Primitivas binarias
      (primapp-bin-exp (rand1 prim rand2)
                   (apply-primitive-bin prim (evaluar-expresion rand1 env) (evaluar-expresion rand2 env)))
      ;Primitivas booleanas
      (exp-bool (expr-bool)
                (eval-expr-bool expr-bool env))
      ;Begin
      (begin-exp (exp exps)
                 (let loop ((acc (evaluar-expresion exp env))
                            (exps exps))
                   (if (null? exps)
                       acc
                       (loop (evaluar-expresion (car exps) env)
                             (cdr exps)))))
      ;If
      (if-exp (expr-bool true-exp false-exp)
              (if (eval-expr-bool expr-bool env)
                  (evaluar-expresion true-exp env)
                  (evaluar-expresion false-exp env)))
      ;While
      (while-exp (expr-bool body)
                 (while expr-bool body env))
      ;Listas
      (list-exp (list) (evaluar-lista list env))
      (crear-lista-exp (ca co)
                (cons (evaluar-expresion ca env) (evaluar-lista co env))
                )
      (vacio-exp () '())
      (vacio?-exp (list) (eqv? (evaluar-expresion list env) '()))
      (list?-exp (list) (list? (evaluar-expresion list env)))
      (cabeza-exp (list) (car (evaluar-expresion list env)) )
      (cola-exp (list) (cdr (evaluar-expresion list env)))
      (append-exp (list1 list2)
                 (append (evaluar-expresion list1 env) (evaluar-expresion list2 env)))
      (ref-list-exp (l p) (list-ref (evaluar-expresion l env) (evaluar-expresion p env)))
      (set-list-exp (l p exp)
                    (let
                        (
                         (le (evaluar-expresion l env))
                         (pe (evaluar-expresion p env))
                         (expe (cons(evaluar-expresion exp env) '()))
                         )
                      (append (append (head-to-position '() le pe 0) expe) (list-tail le (+ pe 1)))
                      ))
      ;Registros
      (registro-exp (id exp ids exps)
                (list (cons id ids) (evaluar-lista (cons exp exps) env))
                )
      (registro?-exp (reg) ((list-of list?) (evaluar-expresion reg env)))
      (crear-registro-exp (id exp ids exps) (list (cons id ids) (evaluar-lista (cons exp exps) env)))
      (ref-registro-exp (lis reg)
                        (cases expresion reg
                          (id-exp (x)
                                  (let (
                                        (list (evaluar-expresion lis env))
                                        )
                                   (list-ref (car(cdr list)) (pos-registro (car list) x 0))                                    
                                   )
                                  )
                          (else (eopl:error 'invalid-register "No es un indice de registro valido"))
                         )
               )
      (set-registro-exp (lis reg exp)
                        (cases expresion reg
                          (id-exp (x)
                                  (let* (
                                        (le (evaluar-expresion lis env))
                                        (expe (cons(evaluar-expresion exp env) '()))
                                        (pe (pos-registro (car le) x 0))
                                        (listval (car(cdr le)))
                                        )
                                     (cons (car le) (cons (append (append (head-to-position '() listval pe 0) expe) (list-tail listval (+ pe 1)))'()))
                                   )
                                  )
                          (else (eopl:error 'invalid-register "No es un indice de registro valido"))
                         )
               )
      (crear-tupla-exp (head tail)
                       (list->vector (map (lambda (arg) (evaluar-expresion arg env)  ) (cons head tail)))
                       )
      (tupla?-exp (body) (vector? (evaluar-expresion body env)))
      (ref-tupla-exp (tupla index)
                     (vector-ref (evaluar-expresion tupla env) (evaluar-expresion index env)))

      (else (eopl:error 'invalid-register "No es un indice de registro valido"))
      )))
;apply-primitive-bin: <expresion> <primitiva> <expresion> -> numero
(define apply-primitive-bin
  (lambda (prim args1 args2)
    (cases primitiva-binaria prim
      (primitiva-suma () (+ args1 args2))     
      (primitiva-resta () (- args1 args2))
      (primitiva-multi () (* args1 args2))
      (primitiva-div () (/ args1 args2))
      (primitiva-mod () (modulo args1 args2))
      (primitiva-concat () (string-append args1 args2)))))
       ;aqui iria append
    
;apply-primitive-un: <primitiva> <expresion> -> numero
(define apply-primitive-un
  (lambda (prim args)
    (cases primitiva-unaria prim
      (primitiva-add1 () (+ args 1))
      (primitiva-sub1 () (- args 1))
      (primitiva-longitud () (string-length args)))))
;apply-pred-prim: <primitiva>
(define apply-pred-prim
  (lambda (prim args1 args2)
    (cases pred-prim prim
      (menor-exp () (< args1 args2))
      (mayor-exp () (> args1 args2))
      (menor-igual-exp () (<= args1 args2))
      (mayor-igual-exp () (>= args1 args2))
      (igual-exp () (eqv? args1 args2))
      (diferente-exp () (not (eqv? args1 args2))))))
;apply-oper-bin-bool: <primitiva>
(define apply-oper-bin-bool
  (lambda (prim args1 args2)
    (cases oper-bin-bool prim
      (primitiva-and () (and args1 args2))
      (primitiva-or () (or args1 args2)))))
;apply-oper-un-bool: <primitiva>
(define apply-oper-un-bool
  (lambda (prim args1)
    (cases oper-un-bool prim
      (primitiva-not () (not args1)))))
;; función para probar booleanos
(define valor-verdad?
  (lambda(x)
    (not (zero? x))))

;WHILE
(define while
      (lambda (expr-bool body env)
        (if (eval-expr-bool expr-bool env)
            (begin (evaluar-expresion body env)
                   (while expr-bool body env))
             1)))

;;eval-rands evalua los operandos y los convierte en un ambiente
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))
(define eval-rand
  (lambda (rand env)
    (cases expresion rand
      (referencia-exp (id)
               (indirect-target
                (let ((ref (apply-env-ref env id)))
                  (cases target (primitive-deref ref)
                    (const-target (expval) ref)
                    (direct-target (expval) ref)
                    (indirect-target (ref1) ref1)))))
      (else
       (direct-target (evaluar-expresion rand env))))))
;;funcion auxiliar para listas
(define evaluar-lista
  (lambda (list env)
    (if (null? list)
        '()
        (cons(evaluar-expresion (car list) env) (evaluar-lista (cdr list) env))
        )
    ))
(define head-to-position
  (lambda (list2 list position counter)
    (if (eqv? position counter)
        (reverse list2)
        (head-to-position (cons (car list) list2) (cdr list) position (+ counter 1))))
  )
;funcion auxiliar para registros
(define pos-registro
  (lambda (lis reg counter)
    (if (= counter (+ (length lis) 1))
        (eopl:error 'out-of-register "No existe el registro ~s" reg)
        (if (eqv? reg (car lis))
            counter
            (pos-registro (cdr lis) reg (+ counter 1))
         )
     )
   )
 )
;Booleanos
(define eval-expr-bool
  (lambda (exp env)
    (cases expr-bool exp
      (pred-prim-exp (prim args1 args2)
                     (apply-pred-prim prim
                                      (evaluar-expresion args1 env)
                                      (evaluar-expresion args2 env)))
      (oper-bin-exp  (prim args1 args2)
                     (apply-oper-bin-bool prim
                                          (eval-expr-bool args1 env)
                                          (eval-expr-bool args2 env)))
      (oper-un-exp (prim args1)
                   (apply-oper-un-bool prim
                                       (eval-expr-bool args1 env)))   
      (bool-exp (arg)
                (cases bool arg
                  (bool-true () #t)
                  (bool-false () #f))))))
;;--------------------------------------------------------Blancos y Referencias---------------------------------------------
(define expval?
  (lambda (x)
    (or (number? x) (procVal? x) (string? x) (list? x) (vector? x))))
;
(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (const-target (v) #t)
                    (indirect-target (v) #f)))))))
;
(define deref
  (lambda (ref)
    (cases target (primitive-deref ref)
      (const-target (expval) expval)
      (direct-target (expval) expval)
      (indirect-target (ref1)
                       (cases target (primitive-deref ref1)
                         (const-target (expval) expval)
                         (direct-target (expval) expval)
                         (indirect-target (p)
                                          (eopl:error 'deref
                                                      "Illegal reference: ~s" ref1)))))))
(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))
(define setref!
  (lambda (ref expval)
    (let
        ((ref (cases target (primitive-deref ref)
                (direct-target (expval1) ref)
                (const-target (expval1) (eopl:error "No se puede cambiar el valor de una variable CONST"))
                (indirect-target (ref1) ref1))))
      (primitive-setref! ref (direct-target expval)))))
(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))
;;--------------------------------------------------------Ambientes---------------------------------------------
;****Gramatica*******
;<env-exp> ::= (empty-env)
;          ::= (extend-env <list-of symbol>
;                          <list-of scheme-value> <env-exp>)
;Representación
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))
(define scheme-value? (lambda (v) #t))
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 
;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
      (deref (apply-env-ref env sym))))
(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))
; Ambiente inicial
(define init-env  
  (extended-env-record (list '@x '@y '@z '@a)
              (list->vector
                (list (direct-target 4)
                      (direct-target 2)
                      (direct-target 5)
                      (indirect-target (a-ref 0 (list->vector (list (direct-target 4)))))))
              (empty-env)))
;------------------------------------------------------------------------------------------------------------------------
;Funciones Auxiliares
; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente
; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente
(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))
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
;;--------------------------------------------------------Procedimientos---------------------------------------------
(define-datatype procVal procVal?
  (cerradura
   (lista-ID(list-of symbol?))
   (body expresion?)
   (amb environment?)))
;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procVal proc
      (cerradura (ids body env)
               (evaluar-expresion body (extended-env-record ids (list->vector args) env))))))

;;-------------------------------------Ambientes---------------------------------

(define-datatype environment environment?
  (env-empty)
  (env-extend
    (syms (list-of symbol?))
    (vec vector?)            
    (env environment?))
  )

(define empty-env
  (lambda ()
    (env-empty)))

(define extend-env
  (lambda (syms vals env)
    (env-extend syms (list->vector vals) env)))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (env-empty ()
        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (env-extend (syms vals env)
        (let ((pos (rib-find-position sym syms)))
          (if (number? pos)
              (a-ref pos vals)
              (apply-env-ref env sym)))))))

(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (env-extend proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (cerradura ids body env)))
            (iota len) idss bodies)
          env)))))

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

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

(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((memv (car set1) set2)
       (difference (cdr set1) set2))
      (else (cons (car set1) (difference (cdr set1) set2))))))

(define extend-env-refs
  (lambda (syms vec env)
    (env-extend syms vec env)))

(define list-find-last-position
  (lambda (sym los)
    (let loop
      ((los los) (curpos 0) (lastpos #f))
      (cond
        ((null? los) lastpos)
        ((eqv? sym (car los))
         (loop (cdr los) (+ curpos 1) curpos))
        (else (loop (cdr los) (+ curpos 1) lastpos))))))


;; evaluar
(define aux
   (lambda (x) x))

(define-datatype part part? 
  (a-part
    (class-name symbol?)
    (fields vector?)))

(define new-object
  (lambda (class-name)
    (if (eqv? class-name 'object)
      '()
      (let ((c-decl (lookup-class class-name)))
        (cons
          (make-first-part c-decl)
          (new-object (clase-declarada->super-name c-decl)))))))

(define make-first-part
  (lambda (c-decl)
    (a-part
      (clase-declarada->class-name c-decl)
      (make-vector (length (clase-declarada->field-ids c-decl))))))



;;----llamado al interpretador-----
(interpretador)





