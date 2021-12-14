(ns scm.core-test
  (:require [clojure.test :refer :all]
            [scm.core :refer :all]))

(deftest leer-entrada-test

  (testing "case 1"
    (let [
          input "123"
          expected 123
          read (with-in-str input (leer-entrada))]
          
      (is (= read expected))))
    
  

  (testing "case 2"
    (let [
          input "123\n456"
          expected 123
          read (with-in-str input (leer-entrada))]
          
      (is (= read expected))))
    
  

  (testing "case 3"
    (let [input "(hola mundo)"
          expected '(hola mundo)
          read (with-in-str input (leer-entrada))]
          
      (is (= read expected))))
    
  

  (testing "case 4"
    (let [input "(hola\n mundo)"
          expected '(hola mundo)
          read (with-in-str input (leer-entrada))]
          
      (is (= read expected))
    )
  ) 

  (testing "case 5"
    (let [
          input "(hola\nmundo)"
          expected '(hola mundo)
          read (with-in-str input (leer-entrada))]
          
      (is (= read expected))))
    
  (testing "case 6"
    (let [input "(123\n456\n789\n101112\n131415)"
          expected '(123 456 789 101112 131415)
          read (with-in-str input (leer-entrada))]
      (is (= read expected))
    )
  )
)

(deftest verificar-parentesis-test

  (testing "case 1"
    (let [
          input "(hola 'mundo"
          expected 1
          res (verificar-parentesis input)]
          
      (is (= res expected))))
    
  

  (testing "case 2"
    (let [
          input "(hola '(mundo)))"
          expected -1
          res (verificar-parentesis input)]
          
      (is (= res expected))))
    
  

  (testing "case 3"
    (let [
          input "(hola '(mundo) () 6) 7)"
          expected -1
          res (verificar-parentesis input)]
          
      (is (= res expected))))
    
  

  (testing "case 4"
    (let [
          input "(hola '(mundo) () 6) 7) 9)"
          expected -1
          res (verificar-parentesis input)]
          
      (is (= res expected))))
    
  

  (testing "case 5"
    (let [
          input "(hola '(mundo) )"
          expected 0
          res (verificar-parentesis input)]
          
      (is (= res expected)))))
  

(deftest error?-test

  (testing "case 1"
    (let [
          arg (list (symbol ";ERROR:") 'mal 'hecho)
          expected true
          res (error? arg)]
          
      (is (= res expected))))
  
  (testing "case 2"
    (let [
          arg (list 'mal 'hecho)
          expected false
          res (error? arg)]
          
      (is (= res expected))))
  
  (testing "case 3"
    (let [
          arg (list (symbol ";WARNING:") 'mal 'hecho)
          expected true
          res (error? arg)]
          
      (is (= res expected))))
  
    (testing "case 4"
      (let [arg 3
            expected false
            res (error? arg)]

        (is (= res expected))))
)

(deftest buscar-test

  (testing "case 1"
    (let [
          key 'c
          amb '(a 1 b 2 c 3 d 4 e 5)
          expected 3
          res (buscar key amb)]
          
      (is (= res expected))))
  
  (testing "case 2"
    (let [key 'f
          amb '(a 1 b 2 c 3 d 4 e 5)
          expected (generar-mensaje-error :unbound-variable key)
          res (buscar key amb)]

      (is (= res expected))))
)

(deftest actualizar-amb-test

  (testing "case 1"
    (let [
          amb '(a 1 b 2 c 3)
          key 'd
          val 4
          expected '(a 1 b 2 c 3 d 4)
          res (actualizar-amb amb key val)]
          
      (is (= res expected))))
  
  (testing "case 2"
    (let [amb '(a 1 b 2 c 3)
          key 'b
          val 4
          expected '(a 1 b 4 c 3)
          res (actualizar-amb amb key val)]

      (is (= res expected))))
  
  (testing "case 3"
    (let [amb '(a 1 b 2 c 3)
          key 'b
          val (list (symbol ";ERROR:") 'mal 'hecho)
          expected '(a 1 b 2 c 3)
          res (actualizar-amb amb key val)]

      (is (= res expected))))
  
  (testing "case 4"
    (let [amb '()
          key 'b
          val 7
          expected '(b 7)
          res (actualizar-amb amb key val)]

      (is (= res expected))))
)

(deftest proteger-bool-en-str-test

  (testing "case 1"
    (let [
          string "(or #F #f #t #T)"
          expected "(or %F %f %t %T)"
          res (proteger-bool-en-str string)]
          
      (is (= res expected))))
  
  (testing "case 2"
    (let [string "(and (or #F #f #t #T) #T)"
          expected "(and (or %F %f %t %T) %T)"
          res (proteger-bool-en-str string)]

      (is (= res expected))))
  
  (testing "case 3"
    (let [string ""
          expected ""
          res (proteger-bool-en-str string)]

      (is (= res expected))))
  
  (testing "case 4"
    (let [string "(+ 1 2)"
          expected "(+ 1 2)"
          res (proteger-bool-en-str string)]

      (is (= res expected))))
  
  (testing "case 5"
    (let [string '(+ 1 2)
          expected "(+ 1 2)"
          res (proteger-bool-en-str string)]

      (is (= res expected))))

)

(deftest restaurar-bool-test

  (testing "case 1"
    (let [string "(and (or #F #f #t #T) #T)"
          expected '(and (or (symbol "#F") (symbol "#f") (symbol "#t") (symbol "#T")) (symbol "#T"))
          res (restaurar-bool (read-string (proteger-bool-en-str string)))]

      (is (= res expected))))

  (testing "case 2"
    (let [string "(and (or %F %f %t %T) %T)"
          expected '(and (or (symbol "#F") (symbol "#f") (symbol "#t") (symbol "#T")) (symbol "#T"))
          res (restaurar-bool (read-string string))]

      (is (= res expected))))
  
  (testing "case 3"
    (let [string "(+ 1 2)"
          expected '(+ 1 2)
          res (restaurar-bool (read-string string))]

      (is (= res expected))))

)

(deftest igual?-test

  (testing "case 1"
    (let [x 'if
          y 'IF
          expected true
          res (igual? x y)]

      (is (= res expected))))
  
  (testing "case 2"
    (let [x 'if
          y 'if
          expected true
          res (igual? x y)]

      (is (= res expected))))
  
  (testing "case 3"
    (let [x 'IF
          y 'IF
          expected true
          res (igual? x y)]

      (is (= res expected))))
  
  (testing "case 4"
    (let [x 'IF
          y "IF"
          expected false
          res (igual? x y)]

      (is (= res expected))))
  
  (testing "case 5"
    (let [x 6
          y "6"
          expected false
          res (igual? x y)]

      (is (= res expected))))  
  
  (testing "case 7"
    (let [x '(a (b) c)
          y '(A (B) C)
          expected true
          res (igual? x y)]

      (is (= res expected))))
  
  (testing "case 8"
    (let [x (list "asd" 5)
          y (list "ASD" 5)
          expected false
          res (igual? x y)]

      (is (= res expected))))
  
  (testing "case 9"
    (let [x (list "asd" 5)
          y (list "asd" 5)
          expected true
          res (igual? x y)]

      (is (= res expected))))
)

(deftest fnc-append-test

  (testing "case 1"
    (let [lists '((1 2) (3) (4 5) (6 7))
          expected '(1 2 3 4 5 6 7)
          res (fnc-append lists)]

      (is (= res expected))))
  
  (testing "case 2"
    (let [lists '((1 2) 3 (4 5) (6 7))
          expected (generar-mensaje-error :wrong-type-arg fnc-append 3)
          res (fnc-append lists)]

      (is (= res expected))))
  
  (testing "case 3"
    (let [lists '((1 2) A (4 5) (6 7))
          expected (generar-mensaje-error :wrong-type-arg fnc-append 'A)
          res (fnc-append lists)]

      (is (= res expected))))
)

(deftest fnc-equal?-test

  (testing "case 1"
    (let [input '()
          expected (symbol "#t")
          res (fnc-equal? input)]

      (is (= res expected))))
  
  (testing "case 2"
    (let [input '(A)
          expected (symbol "#t")
          res (fnc-equal? input)]

      (is (= res expected))))
  
  (testing "case 3"
    (let [input '(A a)
          expected (symbol "#t")
          res (fnc-equal? input)]

      (is (= res expected))))
  
  (testing "case 4"
    (let [input '(A a A)
          expected (symbol "#t")
          res (fnc-equal? input)]

      (is (= res expected))))
  
  (testing "case 5"
    (let [input '(A a A a)
          expected (symbol "#t")
          res (fnc-equal? input)]

      (is (= res expected))))
  
  (testing "case 6"
    (let [input '(A a A B)
          expected (symbol "#f")
          res (fnc-equal? input)]

      (is (= res expected))))
  
  (testing "case 7"
    (let [input '(1 1 1 1)
          expected (symbol "#t")
          res (fnc-equal? input)]

      (is (= res expected))))
  
  (testing "case 8"
    (let [input '(1 1 2 1)
          expected (symbol "#f")
          res (fnc-equal? input)]

      (is (= res expected))))
  
)

(deftest fnc-read-test

  (testing "case 1"
    (let [args '()
          input "(hola\nmundo)"
          expected '(hola mundo)
          res (with-in-str input (fnc-read args))]

      (is (= res expected))))

  (testing "case 2"
    (let [args '(1)
          expected (generar-mensaje-error :io-ports-not-implemented read)
          res (fnc-read args)]

      (is (= res expected))))
  
  (testing "case 3"
    (let [args '(1 2)
          expected (generar-mensaje-error :wrong-number-args-prim-proc fnc-read)
          res (fnc-read args)]

      (is (= res expected))))
  
  (testing "case 4"
    (let [args '(1 2 3)
          expected (generar-mensaje-error :wrong-number-args-prim-proc fnc-read)
          res (fnc-read args)]

      (is (= res expected))))

)

(deftest fnc-sumar-test

  (testing "case 1"
    (let [args '()
          expected 0
          res (fnc-sumar args)]

      (is (= res expected))))
  
  (testing "case 2"
    (let [args '(3)
          expected 3
          res (fnc-sumar args)]

      (is (= res expected))))
  
  (testing "case 3"
    (let [args '(3 4)
          expected 7
          res (fnc-sumar args)]

      (is (= res expected))))
  
  (testing "case 4"
    (let [args '(3 4 5)
          expected 12
          res (fnc-sumar args)]

      (is (= res expected))))
  
  (testing "case 5"
    (let [args '(3 4 5 6)
          expected 18
          res (fnc-sumar args)]

      (is (= res expected))))
  
  (testing "case 6"
    (let [args '(A 4 5 6)
          expected (generar-mensaje-error :wrong-type-arg1 + 'A)
          res (fnc-sumar args)]

      (is (= res expected))))
  
  (testing "case 7"
    (let [args '(3 A 5 6)
          expected (generar-mensaje-error :wrong-type-arg2 + 'A)
          res (fnc-sumar args)]

      (is (= res expected))))
  
  (testing "case 8"
    (let [args '(3 4 A 6)
          expected (generar-mensaje-error :wrong-type-arg2 + 'A)
          res (fnc-sumar args)]

      (is (= res expected))))

)

(deftest fnc-restar-test

  (testing "case 1"
    (let [args '()
          expected (generar-mensaje-error :wrong-number-args -)
          res (fnc-restar args)]

      (is (= res expected))))
  
  (testing "case 2"
    (let [args '(3)
          expected -3
          res (fnc-restar args)]

      (is (= res expected))))
  
  (testing "case 3"
    (let [args '(3 4)
          expected -1
          res (fnc-restar args)]

      (is (= res expected))))
  
  (testing "case 4"
    (let [args '(3 4 5)
          expected -6
          res (fnc-restar args)]

      (is (= res expected))))
  
  (testing "case 5"
    (let [args '(3 4 5 6)
          expected -12
          res (fnc-restar args)]

      (is (= res expected))))
  
  (testing "case 6"
    (let [args '(A 4 5 6)
          expected (generar-mensaje-error :wrong-type-arg1 - 'A)
          res (fnc-restar args)]

      (is (= res expected))))
  
  (testing "case 7"
    (let [args '(3 A 5 6)
          expected (generar-mensaje-error :wrong-type-arg2 - 'A)
          res (fnc-restar args)]

      (is (= res expected))))
  
  (testing "case 8"
    (let [args '(3 4 A 6)
          expected (generar-mensaje-error :wrong-type-arg2 - 'A)
          res (fnc-restar args)]

      (is (= res expected))))
  
)

(deftest fnc-menor-test

  (testing "case 1"
    (let [args '()
          expected (symbol "#t")
          res (fnc-menor args)]

      (is (= res expected))))
  
  (testing "case 2"
    (let [args '(1)
          expected (symbol "#t")
          res (fnc-menor args)]

      (is (= res expected))))
  
  (testing "case 3"
    (let [args '(1 2)
          expected (symbol "#t")
          res (fnc-menor args)]

      (is (= res expected))))
  
  (testing "case 4"
    (let [args '(1 2 3)
          expected (symbol "#t")
          res (fnc-menor args)]

      (is (= res expected))))
  
  (testing "case 5"
    (let [args '(1 2 3 4)
          expected (symbol "#t")
          res (fnc-menor args)]

      (is (= res expected))))
  
  (testing "case 6"
    (let [args '(1 2 2 4)
          expected (symbol "#f")
          res (fnc-menor args)]

      (is (= res expected))))
  
  (testing "case 7"
    (let [args '(1 2 1 4)
          expected (symbol "#f")
          res (fnc-menor args)]

      (is (= res expected))))
  
  (testing "case 8"
    (let [args '(A 1 2 4)
          expected (generar-mensaje-error :wrong-type-arg1 < 'A)
          res (fnc-menor args)]

      (is (= res expected))))
  
  (testing "case 9"
    (let [args '(1 A 1 4)
          expected (generar-mensaje-error :wrong-type-arg2 < 'A)
          res (fnc-menor args)]

      (is (= res expected))))
  
  (testing "case 10"
    (let [args '(1 2 A 4)
          expected (generar-mensaje-error :wrong-type-arg2 < 'A)
          res (fnc-menor args)]

      (is (= res expected))))

)

(deftest fnc-mayor-test

  (testing "case 1"
    (let [args '()
          expected (symbol "#t")
          res (fnc-mayor args)]

      (is (= res expected))))

  (testing "case 2"
    (let [args '(1)
          expected (symbol "#t")
          res (fnc-mayor args)]

      (is (= res expected))))

  (testing "case 3"
    (let [args '(1)
          expected (symbol "#t")
          res (fnc-mayor args)]

      (is (= res expected))))

  (testing "case 4"
    (let [args '(2 1)
          expected (symbol "#t")
          res (fnc-mayor args)]

      (is (= res expected))))

  (testing "case 5"
    (let [args '(4 3 2 1)
          expected (symbol "#t")
          res (fnc-mayor args)]

      (is (= res expected))))

  (testing "case 6"
    (let [args '(4 2 2 1)
          expected (symbol "#f")
          res (fnc-mayor args)]

      (is (= res expected))))

  (testing "case 7"
    (let [args '(4 2 1 4)
          expected (symbol "#f")
          res (fnc-mayor args)]

      (is (= res expected))))

  (testing "case 8"
    (let [args '(A 3 2 1)
          expected (generar-mensaje-error :wrong-type-arg1 > 'A)
          res (fnc-mayor args)]

      (is (= res expected))))

  (testing "case 9"
    (let [args '(3 A 2 1)
          expected (generar-mensaje-error :wrong-type-arg2 > 'A)
          res (fnc-mayor args)]

      (is (= res expected))))

  (testing "case 10"
    (let [args '(3 2 A 1)
          expected (generar-mensaje-error :wrong-type-arg2 > 'A)
          res (fnc-mayor args)]

      (is (= res expected))))
)

(deftest fnc-mayor-o-igual-test

  (testing "case 1"
    (let [args '()
          expected (symbol "#t")
          res (fnc-mayor-o-igual args)]

      (is (= res expected))))

  (testing "case 2"
    (let [args '(1)
          expected (symbol "#t")
          res (fnc-mayor-o-igual args)]

      (is (= res expected))))

  (testing "case 3"
    (let [args '(1)
          expected (symbol "#t")
          res (fnc-mayor-o-igual args)]

      (is (= res expected))))

  (testing "case 4"
    (let [args '(2 1)
          expected (symbol "#t")
          res (fnc-mayor-o-igual args)]

      (is (= res expected))))

  (testing "case 5"
    (let [args '(4 3 2 1)
          expected (symbol "#t")
          res (fnc-mayor-o-igual args)]

      (is (= res expected))))

  (testing "case 6"
    (let [args '(4 2 2 1)
          expected (symbol "#t")
          res (fnc-mayor-o-igual args)]

      (is (= res expected))))

  (testing "case 7"
    (let [args '(4 2 1 4)
          expected (symbol "#f")
          res (fnc-mayor-o-igual args)]

      (is (= res expected))))

  (testing "case 8"
    (let [args '(A 3 2 1)
          expected (generar-mensaje-error :wrong-type-arg1 >= 'A)
          res (fnc-mayor-o-igual args)]

      (is (= res expected))))

  (testing "case 9"
    (let [args '(3 A 2 1)
          expected (generar-mensaje-error :wrong-type-arg2 >= 'A)
          res (fnc-mayor-o-igual args)]

      (is (= res expected))))

  (testing "case 10"
    (let [args '(3 2 A 1)
          expected (generar-mensaje-error :wrong-type-arg2 >= 'A)
          res (fnc-mayor-o-igual args)]

      (is (= res expected)))))

(deftest evaluar-escalar-test

  (testing "case 1"
    (let [key 32
          amb '(x 6 y 11 z "hola")
          expected (list key amb)
          res (evaluar-escalar key amb)]

      (is (= res expected))))
  
  (testing "case 2"
    (let [key "chau"
          amb '(x 6 y 11 z "hola")
          expected (list key amb)
          res (evaluar-escalar key amb)]

      (is (= res expected))))

  (testing "case 3"
    (let [key 'y
          amb '(x 6 y 11 z "hola")
          expected (list 11 amb)
          res (evaluar-escalar key amb)]

      (is (= res expected))))
  
  (testing "case 4"
    (let [key 'z
          amb '(x 6 y 11 z "hola")
          expected (list "hola" amb)
          res (evaluar-escalar key amb)]

      (is (= res expected))))
  
  (testing "case 5"
    (let [key 'n
          amb '(x 6 y 11 z "hola")
          expected (list (generar-mensaje-error :unbound-variable key) amb)
          res (evaluar-escalar key amb)]

      (is (= res expected))))

)

(deftest evaluar-define-test

  (testing "case 1"
    (let [expr '(define x 2)
          amb '(x 1)
          expected (list (symbol "#<unspecified>") '(x 2))
          res (evaluar-define expr amb)]

      (is (= res expected))))
  
  (testing "case 2"
    (let [expr '(define (f x) (+ x 1))
          amb '(x 1)
          expected (list (symbol "#<unspecified>") '(x 1 f (lambda (x) (+ x 1))))
          res (evaluar-define expr amb)]

      (is (= res expected))))
  
  (testing "case 3"
    (let [expr '(define)
          amb '(x 1)
          expected (list (generar-mensaje-error :missing-or-extra 'define '(define)) '(x 1))
          res (evaluar-define expr amb)]

      (is (= res expected))))
  
  (testing "case 4"
    (let [expr '(define x)
          amb '(x 1)
          expected (list (generar-mensaje-error :missing-or-extra 'define '(define x)) '(x 1))
          res (evaluar-define expr amb)]

      (is (= res expected))))
  
  (testing "case 5"
    (let [expr '(define x 2 3)
          amb '(x 1)
          expected (list (generar-mensaje-error :missing-or-extra 'define '(define x 2 3)) '(x 1))
          res (evaluar-define expr amb)]

      (is (= res expected))))

  (testing "case 6"
    (let [expr '(define ())
          amb '(x 1)
          expected (list (generar-mensaje-error :missing-or-extra 'define '(define ())) '(x 1))
          res (evaluar-define expr amb)]

      (is (= res expected))))
  
  (testing "case 7"
    (let [expr '(define () 2)
          amb '(x 1)
          expected (list (generar-mensaje-error :bad-variable 'define '(define () 2)) '(x 1))
          res (evaluar-define expr amb)]

      (is (= res expected))))
  
  (testing "case 8"
    (let [expr '(define 2 x)
          amb '(x 1)
          expected (list (generar-mensaje-error :bad-variable 'define '(define 2 x)) '(x 1))
          res (evaluar-define expr amb)]

      (is (= res expected))))

)

(deftest evaluar-if-test

  (testing "case 1"
    (let [expr '(if 1 2)
          amb '(n 7)
          expected '(2 (n 7))
          res (evaluar-if expr amb)]

      (is (= res expected))))
  
  (testing "case 2"
    (let [expr '(if 1 n)
          amb '(n 7)
          expected '(7 (n 7))
          res (evaluar-if expr amb)]

      (is (= res expected))))
  
  (testing "case 3"
    (let [expr '(if 1 n 8)
          amb '(n 7)
          expected '(7 (n 7))
          res (evaluar-if expr amb)]

      (is (= res expected))))
  
  (testing "case 4"
    (let [expr (list 'if (symbol "#f") 'n)
          amb (list 'n 7 (symbol "#f") (symbol "#f"))
          expected (list (symbol "#<unspecified>") (list 'n 7 (symbol "#f") (symbol "#f")))
          res (evaluar-if expr amb)]

      (is (= res expected))))
  
  (testing "case 5"
    (let [expr (list 'if (symbol "#f") 'n 8)
          amb (list 'n 7 (symbol "#f") (symbol "#f"))
          expected (list 8 (list 'n 7 (symbol "#f") (symbol "#f")))
          res (evaluar-if expr amb)]

      (is (= res expected))))
  
  (testing "case 6"
    (let [expr (list 'if (symbol "#f") 'n '(set! n 9))
          amb (list 'n 7 (symbol "#f") (symbol "#f"))
          expected (list (symbol "#<unspecified>") (list 'n 9 (symbol "#f") (symbol "#f")))
          res (evaluar-if expr amb)]

      (is (= res expected))))
  
  (testing "case 7"
    (let [expr '(if)
          amb '(n 7)
          expected (list (generar-mensaje-error :missing-or-extra 'if '(if)) '(n 7))
          res (evaluar-if expr amb)]

      (is (= res expected))))
  
  (testing "case 8"
    (let [expr '(if 1)
          amb '(n 7)
          expected (list (generar-mensaje-error :missing-or-extra 'if '(if 1)) '(n 7))
          res (evaluar-if expr amb)]

      (is (= res expected))))
  
)

(deftest evaluar-or-test

  (testing "case 1"
    (let [expr (list 'or)
          amb (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))
          expected (list (symbol "#f") (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
          res (evaluar-or expr amb)]

      (is (= res expected))))
  
    (testing "case 2"
      (let [expr (list 'or (symbol "#t"))
            amb (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))
            expected (list (symbol "#t") (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
            res (evaluar-or expr amb)]

        (is (= res expected))))
  
  (testing "case 3"
    (let [expr (list 'or 7)
          amb (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))
          expected (list 7 (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
          res (evaluar-or expr amb)]

      (is (= res expected))))
    
  (testing "case 4"
    (let [expr (list 'or (symbol "#f") 5)
          amb (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))
          expected (list 5 (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
          res (evaluar-or expr amb)]

      (is (= res expected))))
  
  (testing "case 5"
    (let [expr (list 'or (symbol "#f"))
          amb (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))
          expected (list (symbol "#f") (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t")))
          res (evaluar-or expr amb)]

      (is (= res expected))))
)

(deftest evaluar-set!-test

  (testing "case 1"
    (let [expr '(set! x 1)
          amb '(x 0)
          expected (list (symbol "#<unspecified>") '(x 1))
          res (evaluar-set! expr amb)]

      (is (= res expected))))
  
  (testing "case 2"
    (let [expr '(set! x 1)
          amb '()
          expected (list (generar-mensaje-error :unbound-variable 'x) '())
          res (evaluar-set! expr amb)]

      (is (= res expected))))
  
  (testing "case 3"
    (let [expr '(set! x)
          amb '(x 0)
          expected (list (generar-mensaje-error :missing-or-extra 'set! '(set! x)) '(x 0))
          res (evaluar-set! expr amb)]

      (is (= res expected))))
  
  (testing "case 4"
    (let [expr '(set! x 1 2)
          amb '(x 0)
          expected (list (generar-mensaje-error :missing-or-extra 'set! '(set! x 1 2)) '(x 0))
          res (evaluar-set! expr amb)]

      (is (= res expected))))
  
  (testing "case 5"
    (let [expr '(set! 1 2)
          amb '(x 0)
          expected (list (generar-mensaje-error :bad-variable 'set! 1) '(x 0))
          res (evaluar-set! expr amb)]

      (is (= res expected))))
  
)