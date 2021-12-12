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
    
  
