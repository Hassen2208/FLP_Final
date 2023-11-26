#lang eopl 

;;Integrantes
;; Marcela Mazo - 1843612
;;Diana Carolina Micolta - 2028287
;;Hassen Ortiz - 
;;Kevin Tobar - 1841369
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
;;<prim-un>         ::= length
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
    (expresion (expr-bool) exp-bool)
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
    (expresion (primitiva-binaria "(" expresion "," expresion ")") primapp-bin-exp)
    (expresion (primitiva-unaria "(" expresion ")") primapp-un-exp)

    ;------------ Primitivas sobre cadenas -------------
    (expresion ("length" "(" expresion ")") length-exp)
    (expresion ("concat" "(" expresion "," expresion ")") concat-exp)

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
      (a-programa (c-decls exp)
        (elaborate-class-decls! c-decls)
        (evaluar-expresion exp (empty-env))))))

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

      (mostrar-exp () the-class-env)

      ;Numeros
      (numero-lit (numero) numero)
      (oct-exp (numero) numero)
      (hex-exp (numero) numero)
      (bignum-exp (numero) numero)
      
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
      (primapp-bin-exp (prim rand1 rand2)
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
      
      ;For
      (for-exp (var value way x body ) (forfunction-verify var (evaluar-expresion value env) way (evaluar-expresion x env) body env)  )

      ;Cadenas
      (concat-exp (string1 string2)
                  (string-append (evaluar-expresion string1 env) (evaluar-expresion string2 env)))
      (length-exp (string)
                  (string-length (evaluar-expresion string env)))

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

      ;Tuplas
      (tupla-exp (list) (list->vector (map (lambda (arg) (evaluar-expresion arg env)  ) list )))
      (crear-tupla-exp (head tail)
                       (list->vector (map (lambda (arg) (evaluar-expresion arg env)  ) (cons head tail)))
                       )
      (tupla?-exp (body) (vector? (evaluar-expresion body env)))
      (ref-tupla-exp (tupla index)
                     (vector-ref (evaluar-expresion tupla env) (evaluar-expresion index env)))
      (cabeza-tupla-exp (tupla)(car (vector->list (evaluar-expresion tupla env))))
      (cola-tupla-exp (tupla) (list->vector (cdr (vector->list (evaluar-expresion tupla env)))))

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
                                   ))
                          (else (eopl:error 'invalid-register "No es un indice de registro valido"))
                         ))

      ;procedimientos
      
      (procedimiento-exp (ids body)
                (cerradura ids body env))
      
      (letrec-exp (proc-names idss bodies letrec-body)
                  (evaluar-expresion letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))
      
(evaluar-exp (expresion)
  (let ((proc (evaluar-expresion (expresion) env))
        (args (eval-rands (id-exp expresion) env)))
    (if (procval? proc)
        (apply-procval proc args)
        (eopl:error 'evaluar-exp
                    "Attempt to apply non-procedure ~s" proc))))

      
      ;Imprimir
      (print-exp (txt) (display (evaluar-expresion txt env)) (newline))

       ;
      (new-object-exp (class-name rands)
        (let ((args (eval-rands rands env))
              (obj (new-object class-name)))
          (find-method-and-apply
            'initialize class-name obj args)
          obj))
      (method-app-exp (obj-exp method-name rands)
        (let ((args (eval-rands rands env))
              (obj (evaluar-expresion obj-exp env)))
          (find-method-and-apply
            method-name (object->class-name obj) obj args)))
      (super-call-exp (method-name rands)
        (let ((args (eval-rands rands env))
              (obj (apply-env env 'self)))
          (find-method-and-apply
            method-name (apply-env env '%super) obj args)))

      (else (eopl:error 'invalid-register "No es un indice de registro valido"))
      )))


;apply-primitive-bin: <expresion> <primitiva> <expresion> -> numero
(define apply-primitive-bin
  (lambda (prim args1 args2)
    (cases primitiva-binaria prim

      ;decimales
      (primitiva-suma () (+ args1 args2))     
      (primitiva-resta () (- args1 args2))
      (primitiva-multi () (* args1 args2))
      (primitiva-div () (/ args1 args2))
      (primitiva-mod () (modulo args1 args2))

      ;octales
      (oct-suma () (suma-base args1 args2 8) )
      (oct-resta () (resta-base args1 args2 8) )
      (oct-multi () (multi-base args1 args2 8) )
          
      ;hexadecimales
      (hex-suma () (suma-base args1 args2 16) )
      (hex-resta () (resta-base args1 args2 16) )
      (hex-multi () (multi-base args1 args2 16) )
      
      ;base 32
      (big-suma () (suma-base args1 args2 32) )
      (big-resta () (resta-base args1 args2 32) )
      (big-multi () (multi-base args1 args2 32) )
      )))
    
;apply-primitive-un: <primitiva> <expresion> -> numero
(define apply-primitive-un
  (lambda (prim args)
    (cases primitiva-unaria prim
      (primitiva-add1 () (+ args 1))
      (primitiva-sub1 () (- args 1))
      (oct-add1 () (sucesor-base args 8) )
      (oct-sub1 () (predecesor-base args 8) )
      (hex-add1 () (sucesor-base args 16) )
      (hex-sub1 () (predecesor-base args 16) )
      (big-add1 () (sucesor-base args 32) )
      (big-sub1 () (predecesor-base args 32) )
      )))
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



;***********************************************numeros no decimales*************************
;operaciones aritmeticas para numeros no decimales

(define sucesor-base
  (lambda (num base)
    (if(null? num)
      '(1)
      (if (< (car num) (- base 1 ))
           (cons (+ 1 (car num)) (cdr num)  )
          (cons 0 (sucesor-base(cdr num) base ) )
          ))))

(define predecesor-base
  (lambda (num base)
    (if(null? num)
       (eopl:error "limite alcanzado")
       (if (> (car num) 0)
           (if (and (eq? (- (car num) 1) 0) (null? (cdr num)))
               '()
               (cons (- (car num) 1) (cdr num)))
           (cons (- base 1) (predecesor-base (cdr num) base)))
           )))

(define suma-base
 (lambda (elem1 elem2 base)
   (if(null? elem2)
      elem1
      (suma-base (sucesor-base elem1 base) (predecesor-base elem2 base) base))))


(define resta-base
 (lambda (elem1 elem2 base)
   (if (null? elem2)
       elem1
       (predecesor-base (resta-base elem1 (predecesor-base elem2 base) base) base))))

(define multi-base
 (lambda (elem1 elem2 base)
   (if (null? elem2 )
       elem1
       (suma-base elem1 (multi-base elem1 (predecesor-base elem2 base) base) base))))

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

;for
(define forfunction-verify
  (lambda (var val way x body env)
    (cases iterador way 
      (iter-to () (forfunction-up var val x body env) )
      (iter-down () (forfunction-down var val x body env) ))
  ))

(define forfunction-up
  (lambda (var val x body env)    
    (if
     (< val x)
     (begin
       (evaluar-expresion body env)
       (forfunction-up var (+ 1 val) x body env )
       )
     1)
  ))

(define forfunction-down
  (lambda (var val x body env)
    (if
     (> val x)
     (begin
       (evaluar-expresion body env)
       (forfunction-down var (- val 1) x body env )

       )
     1)
  ))

;;eval-rands evalua los operandos y los convierte en un ambiente
(define eval-rands
  (lambda (exps env)
    (map
      (lambda (exp) (evaluar-expresion exp env)) exps)))

;///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

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
    (or (number? x) (procval? x) (string? x) (list? x) (vector? x))))
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
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref! 
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
        (vector-set! vec pos val)))
    ))
;******************************************** Procedimientos *******************************************
(define-datatype procval procval?
  (cerradura
   (lista-ID(list-of symbol?))
   (body expresion?)
   (amb environment?)))

;apply-procval: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procval
  (lambda (proc args)
    (cases procval proc
      (cerradura (ids body env)
               (evaluar-expresion body (extended-env-record ids (list->vector args) env))))))
              

;^;;;;;;;;;;;;;;; environments ;;;;;;;;;;;;;;;;

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
    (syms (list-of symbol?))
    (vec vector?)            
    (env environment?))
  )

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

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

(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
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
    (extended-env-record syms vec env)))

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

;;--------------------------------------------------------methods---------------------------------------------

;;; methods are represented by their declarations.  They are closed
;;; over their fields at application time, by apply-method.

(define find-method-and-apply
  (lambda (m-name host-name self args)
    (if (eqv? host-name 'object)
      (eopl:error 'find-method-and-apply
        "No method for name ~s" m-name)
      (let ((m-decl (lookup-method-decl m-name
                      (class-name->method-decls host-name))))
        (if (method-decl? m-decl)
          (apply-method m-decl host-name self args)
          (find-method-and-apply m-name 
            (class-name->super-name host-name)
            self args))))))

(define view-object-as
  (lambda (parts class-name)
    (if (eqv? (part->class-name (car parts)) class-name)
      parts
      (view-object-as (cdr parts) class-name))))

(define apply-method
  (lambda (m-decl host-name self args)
    (let ((ids (method-decl->ids m-decl))
          (body (method-decl->body m-decl))
          (super-name (class-name->super-name host-name)))
      (evaluar-expresion body
        (extend-env
          (cons '%super (cons 'self ids))
          (cons super-name (cons self args))
          (build-field-env 
            (view-object-as self host-name)))))))

(define build-field-env
  (lambda (parts)
    (if (null? parts)
      (empty-env)
      (extend-env-refs
        (part->field-ids (car parts))
        (part->fields    (car parts))
        (build-field-env (cdr parts))))))

;;--------------------------------------------------------method environments---------------------------------------------
;; find a method in a list of method-decls, else return #f

(define lookup-method-decl 
  (lambda (m-name m-decls)
    (cond
      ((null? m-decls) #f)
      ((eqv? m-name (method-decl->method-name (car m-decls)))
       (car m-decls))
      (else (lookup-method-decl m-name (cdr m-decls))))))
      

;;--------------------------------------------------------class environments---------------------------------------------
;; find a method in a list of method-decls, else return #f

(define the-class-env '())

(define elaborate-class-decls!
  (lambda (c-decls)
    (set! the-class-env c-decls)))

(define lookup-class
  (lambda (name)
    (let loop ((env the-class-env))
      (cond
        ((null? env)
         (eopl:error 'lookup-class
           "Unknown class ~s" name))
        ((eqv? (clase-declarada->class-name (car env)) name) (car env))
        (else (loop (cdr env)))))))
        

;;--------------------------------------------------------declarations---------------------------------------------
;; find a method in a list of method-decls, else return #f

(define clase-declarada->class-name
  (lambda (c-decl)
    (cases clase-declarada c-decl
      (a-clase-declarada (class-name super-name field-ids m-decls)
        class-name))))

(define clase-declarada->super-name
  (lambda (c-decl)
    (cases clase-declarada c-decl
      (a-clase-declarada (class-name super-name field-ids m-decls)
        super-name))))

(define clase-declarada->field-ids
  (lambda (c-decl)
    (cases clase-declarada c-decl
      (a-clase-declarada (class-name super-name field-ids m-decls)
        field-ids))))

(define clase-declarada->method-decls
  (lambda (c-decl)
    (cases clase-declarada c-decl
      (a-clase-declarada (class-name super-name field-ids m-decls)
        m-decls))))

(define method-decl->method-name
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) method-name))))

(define method-decl->ids
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) ids))))

(define method-decl->body
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) body))))

(define method-decls->method-names
  (lambda (mds)
    (map method-decl->method-name mds)))      
      

;;--------------------------------------------------------selectors of all sorts ---------------------------------------------

(define part->class-name
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields)
        class-name))))

(define part->fields
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields)
        fields))))

(define part->field-ids
  (lambda (part)
    (clase-declarada->field-ids (part->clase-declarada part))))

(define part->clase-declarada
  (lambda (part)
    (lookup-class (part->class-name part))))

(define part->method-decls
  (lambda (part)
    (clase-declarada->method-decls (part->clase-declarada part))))

(define part->super-name
  (lambda (part)
    (clase-declarada->super-name (part->clase-declarada part))))

(define class-name->method-decls
  (lambda (class-name)
    (clase-declarada->method-decls (lookup-class class-name))))

(define class-name->super-name
  (lambda (class-name)
    (clase-declarada->super-name (lookup-class class-name))))

(define object->class-name
  (lambda (parts)
    (part->class-name (car parts))))
;;----llamado al interpretador-----
(interpretador)




;; ----------- PRUEBAS -----------------

; Pruebas para valores enteros
(scan&parse "42") 

; Pruebas para valores flotantes
(scan&parse "3.14") 

; Pruebas para números en base 8
(scan&parse "x8(1 2 7)") 

; Pruebas para números en base 16
(scan&parse "x16(10 9 15)") 

; Pruebas para números en base 32
(scan&parse "x32(15 7 23)") 

; Pruebas para cadenas de caracteres
(scan&parse "\"Hola mundo\"") 

; Pruebas para booleanos
(scan&parse "true") 
(scan&parse "false") 

;Pruebas para identificadores
(scan&parse "abc") 
(scan&parse "xyz") 

; Referencia
(scan&parse "&foo")

; Variable
(scan&parse "var y = 8 in sub1(y)") 

; Actualización de variable
(scan&parse "var z = 3 in begin set z = 5; z end") 

; Procedimientos
(scan&parse "function(x, y, z) {((x * y) + z)}") 

; ----------- Invocación de procedimientos -----
; Por valor
(scan&parse "var p = function(x) {add1(x)} in var y = 3 in evaluar p(y)")
(scan&parse "var p = function(arr) {set-list(arr, 1, 42)} in var nums = [1, 2, 3] in evaluar p(nums)")
(scan&parse "var p = function(z) {set z = 10} in var w = 5 in begin evaluar p(w); w end")

; Por referencia
(scan&parse "var p = function(a) {set a = 8} in var b = 3 in begin evaluar p(&b); b end")

; Listas
(scan&parse "crear-lista(2, 4, 6)") ; Crear lista
(scan&parse "var lst = crear-lista(10, 20, 30) in set-list(lst, 2, 42)") ; Set list
(scan&parse "lista? (crear-lista(5, 6, 7))") ; ¿Es lista?
(scan&parse "cabeza (crear-lista(8, 9, 10))") ; Cabeza de la lista
(scan&parse "cola (crear-lista(12, 14, 16))") ; Cola de la lista
(scan&parse "append ([1, 2, 3], [4, 5, 6])") ; Append
(scan&parse "ref-list([7, 8, 9], 1)") ; Referencia a la lista

; Tupla
(scan&parse "crear-tupla(1, 2, 3, 4, 5)")
(scan&parse "tupla?(crear-tupla(10, 20, 30, 40, 50))")
(scan&parse "ref-tupla(crear-tupla(6, 7, 8, 9, 10), 3)")
(scan&parse "cabeza-tupla(crear-tupla(11, 12, 13, 14))")
(scan&parse "cabeza-tupla(tupla[16, 17, 18, 19])")
(scan&parse "cola-tupla(crear-tupla(21, 22, 23, 24))")
(scan&parse "cola-tupla(tupla[26, 27, 28, 29])")

; Registros
(scan&parse "crear-registro(x = 42, y = 56)") ; Crear registro
(scan&parse "registro?(crear-registro(a = 1, b = 2))") ; ¿Es registro?
(scan&parse "ref-registro({a = 99, b = 88}, b)") ; Referencia a registro
(scan&parse "set-registro({x = 33, y = 44}, x, 11)") ; Set registro

; Estructuras de control
(scan&parse "if <(4, 8) : (12+16) else : (20~24) end")
(scan&parse "while false : print(false) done ")
(scan&parse "for(counter = 8 to 12) { print(counter) }")


; Operaciones aritméticas
(scan&parse "(5 + 3)")
(scan&parse "(7 ~ 4)")
(scan&parse "(2 * 8)")
(scan&parse "(10 / 3)")
(scan&parse "(7 % 3)")
(scan&parse "(x8(6) +(x8) x8(9))")
(scan&parse "(x16(20 8) +(x16) x16(5 2))")
(scan&parse "(x32(17) +(x32) x32(6))")
(scan&parse "(x8(5) *(x8) x8(2))")
(scan&parse "(x16(10) *(x16) x16(5))")
(scan&parse "(x32(18 4) *(x32) x32(12))")
(scan&parse "(x8(2) ~(x8) x8(5))")
(scan&parse "(x16(12) ~(x16) x16(8))")
(scan&parse "(x32(25) ~(x32) x32(7))")

; Primitivas unarias
(scan&parse "sub1(9)")
(scan&parse "add1(7)")
(scan&parse "add1(x8)(x8(5))")
(scan&parse "add1(x16)(x16(22))")
(scan&parse "add1(x16)(x16(3 7))")

; Primitivas sobre cadenas
(scan&parse "sub1(10)")

; Invocación de procedimientos
(scan&parse "function (2, 6){(2+6)}")

; Funciones booleanas
(scan&parse "> (8,5)")
(scan&parse "< (3,9)")
(scan&parse "<= (4,4)")
(scan&parse ">= (6,3)")
(scan&parse "== (5,5)")
(scan&parse "!= (1,1)")
(scan&parse "or(false,true)")
(scan&parse "and(false,false)")
(scan&parse "not(false)")

