(ns jmlogic.core
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [clojure.core.logic.fd :as fd]))

(defne nonmembero
  "Comprueba si un elemento es miembro de una colecci�n"
  [x l]
  ([_ ()])
  ([_ [head . tail]]
     (!= x head)
     (nonmembero x tail)))

(defne alldistincto
  "Comprueba si todos los elementos de una lista degenerada de esas son distintos"
  [l]
  ([()]) ; Cuando no hay nada, son todos distintos
  ([[head . tail]]
   (nonmembero head tail)
   (alldistincto tail)))


(run* [q]
      (alldistincto [1 2 673 3])
      (== q 1))

; Esto funciona y unifica sobre la misma lista
(defne idento
  [xs ys]
  ([() ()])
  ([[h1 . t1] [h2 . t2]]
     (== h1 h2)
     (idento t1 t2)))


(def queens (repeatedly 8 lvar))
(def domain (fd/domain 1 2 3 4 5 6 7 8))

(defne rowo
  "unifica la fila"
  [queen r]
  ([[?row _] ?row]))

(defne columno
  "unifica la columna"
  [queen c]
  ([[_ ?col] ?col]))


(defne rowso
  "unifica un vector de posiciones, expresadas como vectores [row column] con el vector formado por sus filas"
  [queens rows]
  ; Si no hay ninguna reina, no hay ninguna fila
  ([() ()])
  ; Si hay cosas, tomamos la fila de la primera y la concatenamos con
  ; las filas del resto
  ([[[?r _] . tail] [?r . ?rs]]
     (rowso tail ?rs)))

(run* [q]
      (rowso [[1 2] [5 6] [8 15] [6 1]] q))


(run* [q]
      (fresh [r c]
             (rowo [5 11] r)
             (columno [56 19] c)
             (== q [r c])))

(run 1 [q]
     (everyg (fn [queen]
               (fresh [r c]
                      (== [r c] queen)
                      (fd/in r domain)
                      (fd/in c domain))) queens)
     (fd/distinct queens)
     (== q queens))

