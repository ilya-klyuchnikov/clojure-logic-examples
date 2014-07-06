(ns arith
  (:refer-clojure :exclude [== record?])
  (:use [clojure.core.logic :exclude [record?]])
  (:use [clojure.core.logic.protocols]))

(defrecord Z []
  IUnifyTerms
  (unify-terms [_ v s]
    (if (instance? Z v) s nil))
  IUninitialized
  (-uninitialized [_] (Z.)))

(defrecord Succ [prev]
  IUnifyTerms
  (unify-terms [u v s]
    (if (instance? Succ v) (unify s (.prev u) (.prev v)) nil))
  IUninitialized
  (-uninitialized [_] (Succ. nil)))

(defrecord Plus [l r]
  IUnifyTerms
  (unify-terms [u v s]
    (if (instance? Plus v) (unify s [(.l u) (.r u)] [(.l v) (.r v)]) nil))
  IUninitialized
  (-uninitialized [_] (Plus. nil nil)))

; (run* [q] (== (Succ. 1) q))
; (run* [q] (== (Z.) q))
; (run* [x y] (== (Plus. 1 x) (Plus. y 2)))


(defn nat?
  "Natural number"
  [n]
  (conde
    [(== (Z.) n)]
    [(fresh [n1] (== n1 n))]
    [(fresh [n1]
            (nat? n1)
            (== (Succ. n1) n))]))

; (set (run 30 [x y z] (nplus x y z)))
(defn nplus
  [x y z]
  (conde
    ; 0 + x = x
    [(== (Z.) x) (nat? y) (nat? z) (== y z)]
    ; S x1 + y = S (x1 + y)
    [(fresh [x1 z1]
            (== (Succ. x1) x)
            (== (Succ. z1) z)
            (nplus x1 y z1))]))

; enumerates expressions
; (run 10 [q] (nexpr q))
(defn nexpr
  "Expression over natural numbers"
  [n-expr]
  (conde
    [(nat? n-expr)]
    [(fresh [n1 n2]
            (== (Plus. n1 n2) n-expr)
            (nexpr n1)
            (nexpr n2))]))
