(ns lattices
  (:refer-clojure :exclude [== record?])
  (:use [clojure.core.logic :exclude [record?]]))

(defn l-el
  "A goal testing that el is an element of the lattice."
  [el]
  (membero el [:bot :null :not-null :top]))

(defn l-join
  "A goal for z being a (precise) join of x an y."
  [x y z]
  (conde
    [(== :bot x) (l-el y) (== y z)]
    [(l-el x) (== :bot y) (== x z)]
    [(== :top x) (l-el y) (== :top z)]
    [(l-el x) (== :top y) (== :top z)]
    [(l-el x) (l-el y) (!= x y) (!= x :bot) (!= y :bot) (== :top z)]
    [(l-el x) (l-el y) (== x y) (== x z)]))

(defn l-meet
  "A goal for z being a (precise) meet of x an y."
  [x y z]
  (conde
    [(== :bot x) (l-el y) (== :bot z)]
    [(l-el x) (== :bot y) (== :bot z)]
    [(== :top x) (l-el y) (== y z)]
    [(l-el x) (== :top y) (== x z)]
    [(l-el x) (l-el y) (!= x y) (!= x :top) (!= y :top) (== :bot z)]
    [(l-el x) (l-el y) (== x y) (== x z)]))
