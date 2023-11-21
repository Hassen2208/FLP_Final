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
(define lexico
  '(
    (white-sp (whitespace) skip)
    (comentario ("//" (arbno (not #\newline))) skip)
    (identificador (letter (arbno (or letter digit))) symbol)
    (letras (letter) string)
    (letras (letter (arbno (or letter digit))) string)    
    (numero (digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
    )
  )

;;--------------------------------------------------------Especificacion sintactica---------------------------------------------

(define grammar-simple-interpreter
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

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)
;;letrec fact(n) = Si n entonces (n * (fact sub1(n))) sino 1 finSi in (fact 20)
(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)
(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define list-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))


;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))
