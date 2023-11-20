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