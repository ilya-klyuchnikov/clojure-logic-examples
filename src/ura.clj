(ns ura
  (:refer-clojure :exclude [== record?])
  (:use [clojure.core.logic :exclude [record?]])
  (:use [clojure.core.logic.protocols]))

(defmacro time-limited [ms & body]
  `(let [f# (future ~@body)]
     (deref f# ~ms nil)))

; examples that are hard for URA
(defrecord Leaf [data]
  IUnifyTerms
  (unify-terms [u v s]
    (if (instance? Leaf v) (unify s (.data u) (.data v)) nil))
  IUninitialized
  (-uninitialized [_] (Leaf. nil)))

(defrecord Node [l data r]
  IUnifyTerms
  (unify-terms [u v s]
    (if (instance? Node v)
      (unify s [(.l u) (.data u) (.r u)] [(.l v) (.data v) (.r v)])
      nil))
  IUninitialized
  (-uninitialized [_] (Node. nil nil nil)))

(defrecord BNode [l r]
  IUnifyTerms
  (unify-terms [u v s]
    (if (instance? BNode v)
      (unify s [(.l u) (.r u)] [(.l v) (.r v)])
      nil))
  IUninitialized
  (-uninitialized [_] (BNode. nil nil)))

; (run* [q] (concato [1 2] q [1 2 3]))
(defn concato
  "List concatenation relation"
  [xs ys zs]
  (conde
    [(== [] xs) (== ys zs)]
    [(fresh [h xs1 zs1]
            (conso h xs1 xs)
            (conso h zs1 zs)
            (concato xs1 ys zs1))]))

; gFlatten(Leaf(a)) = Cons(a, Nil());
; gFlatten(Node(lt, s, rt)) = gAppend(gFlatten(lt), Cons(s, gFlatten(rt)));
; the same property as URA: doesn't terminate
; (run 2 [t] (flatteno t ['a]))
(defn flatteno
  "tree t is flattened into list l"
  [t l]
  (conde
    [(fresh [d]
            (== (Leaf. d) t)
            (== [d] l))]
    [(fresh [lt d rt ltf rtf tmp]
            (== (Node. lt d rt) t)
            (flatteno lt ltf)
            (flatteno rt rtf)
            (conso d rtf tmp)
            (concato ltf tmp l))]))

; doesn't terminate
; (run 4 [t] (b-flatteno t ['a 'a 'b 'b]))
(defn b-flatteno
  "tree t is flattened into list l"
  [t l]
  (conde
    [(fresh [d]
            (== (Leaf. d) t)
            (== [d] l))]
    [(fresh [lt rt ltf rtf]
            (== (BNode. lt rt) t)
            (flatteno lt ltf)
            (flatteno rt rtf)
            (concato ltf rtf l))]))

(defn fixpoint [e] (fresh [x] (fixpoint x) (== x e)))
