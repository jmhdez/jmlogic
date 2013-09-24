(ns jmlogic.core
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [clojure.core.logic.fd :as fd]))

(defne nonmembero
  "Comprueba si un elemento es miembro de una colección"
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


(def queens (repeatedly 8 lvar))
(def domain (fd/domain 1 2 3 4 5 6 7 8))

(run 4 [q]
     (== q [rows columns])
     (everyg #(fd/in % domain) rows)
     (everyg #(fd/in % domain) columns))

(run 1 [q]
     (== q [queens])
     (everyg (fn [queen]
               (fresh [r c]
                      (== [r c] queen)
                      (fd/in r domain)
                      (fd/in c domain))) queens))
