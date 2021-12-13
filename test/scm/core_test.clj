(ns scm.core-test
  (:require [clojure.test :refer :all]
            [scm.core :refer :all]))

(deftest leer-entrada-test

  (testing "lectura sin parentesis ni salto de linea"
    (let [
          input "123"
          expected "123"
          read (with-in-str input (leer-entrada))]
          
      (is (= read expected))))
    
  

  (testing "lectura sin parentesis con salto de linea"
    (let [
          input "123\n456"
          expected "123"
          read (with-in-str input (leer-entrada))]
          
      (is (= read expected))))
    
  

  (testing "lectura con parentesis balanceado sin salto de linea"
    (let [
          input "(hola mundo)"
          expected "(hola mundo)"
          read (with-in-str input (leer-entrada))]
          
      (is (= read expected))))
    
  

  (testing "lectura con parentesis balanceado con salto de linea"
    (let [
          input "(hola\n mundo)"
          expected "(hola mundo)"
          read (with-in-str input (leer-entrada))]
          
      (is (= read expected))
    )
  ) 

  (testing "lectura con parentesis balanceado con salto de linea sin separacion"
    (let [
          input "(hola\nmundo)"
          expected "(hola mundo)"
          read (with-in-str input (leer-entrada))]
          
      (is (= read expected))))
    
  (testing "lectura multilinea"
    (let [
          input "(123\n456\n789\n101112\n131415)"
          expected "(123 456 789 101112 131415)"
          read (with-in-str input (leer-entrada))
          ]
      (is (= read expected))
    )
  )     
  

  (testing "lectura con parentesis de mas sin salto de linea"
    (let [
          input "(hola mundo))"
          expected "(hola mundo))"
          read (with-in-str input (leer-entrada))
         ]
          
      (is (= read expected)))))    
  


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
          expected "(;ERROR: unbound variable: f)"
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
)