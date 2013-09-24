(ns jmlogic.core
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [clojure.core.logic.fd :as fd]))


(def queens (repeatedly 8 lvar))
(def domain (fd/domain 1 2 3 4 5 6 7 8))

(defne rowso
  "unifica un vector de posiciones, expresadas como vectores [row column] con el vector formado por sus filas"
  [queens rows]
  ([() ()]) ; caso base. No hay reinas, no hay filas
  ([[[?r _] . tail] [?r . ?rs]] ; la fila de la 1ª con las filas del resto
     (rowso tail ?rs)))

(defne colso
  "unifica un vector de posiciones, expresadas como vectores [row column] con el vector formado por sus columnas"
  [queens columns]
  ([() ()]) ; caso base. No hay reinas, no hay columnas
  ([[[_ ?c] . tail] [?c . ?cs]] ; la fila de la 1ª con las columnas del resto
     (colso tail ?cs)))

(defne diags1o
  "diagonales de arriba-izda a abajo dcha"
  [queens diags]
  ([() ()])
  ([[[?r ?c] . tail] [?d1 . ?ds]]
     (diags1o tail ?ds)
     (diag1o [?r ?c] ?d1)))

(defne diag1o [queen diag]
  ([[r c] d]
     (project [r c d]
              (== (- r c) d))))

; Esto no funciona 
(run 1 [q]
     (fresh [x y]
            (diags1o [[1 5] [x y]] q)
            (== x 1)
            (== y 54)))

;... pero esto sí
(run 1 [q]
     (fresh [x y]
            (== x 1)
            (== y 54)
            (diags1o [[1 5] [x y]] q)))



(run 1 [q]
     (fresh [rows cols diags1 diags2]
            (everyg (fn [queen]
                      (fresh [r c]
                             (== [r c] queen)
                             (fd/in r domain)
                             (fd/in c domain))) queens)
            ;(rowso queens rows)
            ;(distincto rows)
            ;(colso queens cols)
            ;(distincto cols)
            (diags1o queens diags1)
            ;(distincto diags1)
            )
     (== q queens))
