(ns jm.play)

;; Árboles simétricos
;; Un árbol es de la forma:
;; nil -> árbol vacío
;; raiz hijo-izquierdo hijo-derecho -> con raiz distinto de nil

(defn sym
  ([[node left right]]
     (sym left right))
  ([[n1 l1 r1] [n2 l2 r2]]
    (or (= n1 n2 nil)
        (and (= n1 n2)
             (sym l1 r2)
             (sym l2 r1)))))


(defn ce [f d]
  (set (map (comp set second) (group-by f d))))

(defn is-an [x y]
  (= (sort x) (sort y)))

(defn af [words]
  (filter #(> (count %) 1) (map vals (group-by #(= (sort %1) (sort %2)) words))))

;; Decurry

(defn decurry [f]
  (fn [& more]
    (reduce #(%1 %2) f more)))






;; Cada vez que intento hacer algo hecho en falta poder controlar
;; el elemento como un par con la posición y el valor para no
;; depender de la posición

;; Represento un número como un vector en el que cada elemento es a su
;; vez un vector con el índice y el valor, por ejemplo:
;; 1723 => [[0 1] [1 7] [2 2] [3 3]]

(defn make-number [number length]
  "Convierte un número en un vector de la forma indicada, con length posiciones"
  (letfn [(to-int [c] (- (int c) (int 0)))]
    (map-indexed #(%1 (to-int %2)) (format (str "%" length "d") number))))

(defn count-eq-val [number value]
  "Cuenta el número de veces que aparece un determinado valor en number"
  (count (filter #(= value (second %)) number)))

(defn is-auto? 
  "Comprueba is un elemento es autodescriptivo dentro de su colección o si la colección entera lo es"
  ([xs]
     (every? (partial is-auto? xs) xs))
  ([xs [idx value]]
     (= value (count-eq-val xs idx))))

(defn auto-part [xs]
  (let [number (map-indexed vector xs)]
    (map second (take-while #(is-auto? number %) number))))

;; auto-part [1 2 1 0] me devuelve [1 2 1 0]. He encontrado uno a
;; mano!
