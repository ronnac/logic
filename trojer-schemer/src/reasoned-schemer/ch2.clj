(ns reasoned-schemer.ch2
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

(let [x (fn [a] a)
      y :c]
  (x y))
;; => :c
;; because (x y) applies (fn [a] a)
;; to :c.

(run* (r)
  (fresh [y x]
    (== [x y] r)))
;; => ([_0 _1])
;; because the variables in [x y] have
;; been introduced by fresh.

(run* (r)
  (fresh (v w)
    (== (let [x v
              y w]
             [x y]) r)))
;; => ([_0 _1])
;; because v and w are variables introduced
;; by fresh.

(first '(:grape :raisin :pear))
;; => :grape
;; Scheme uses car and cdr
;; iso. first and rest

(first '(:a :c :o :r :n))
;; => :a

(run* (r)
      (firsto [:a :c :o :r :n] r))
;; => (:a)

;; => (:a)
;; Scheme uses caro and cdro
;; iso. firsto and resto

;;===============================
;;  page 18
;;===============================

(run* (q)
      (firsto [:a :c :o :r :n] :a)
      (== true q))
;; => (true)
;; because a is the first
;; of [:a :c :o :r :n]

(run* (r)
      (fresh (x y)
             (firsto [r y] x)   ;; !!!
             (== :pear x)))
;; => (:pear)
;; since x is associated with the first
;; of [r y], which is the fresh variable r.
;; Then x is associated with pear, which in
;; turn associates r with pear.

;; Here is the definition of caro.
(defn caro [p a]
  (fresh (d)
         (== [a d] p)))
;; => #'reasoned-schemer.ch2/caro
;; What is unusual about this definition?
;; Whereas first (Scheme: car) takes one
;; argument, firsto (caro) takes two.


(defn caro [p a]
  (== (first p) a))
;; => #'reasoned-schemer.ch2/caro

(run* (r) 
  (caro [:a :b] r))
;; => (:a)

;; [r fresh(d)] == [:a :b]

(run* (r)
      (fresh (x y)
             (firsto [:grape :raisin :pear] x)
             (firsto [:a :b :c] y)
             ;; u#
             (== [x y] r)))

(run* (r)
      (fresh (v)
             (resto [:a :c :o :r :n] v)
             (firsto v r)))

(defn cdro [p d]
  (fresh (a)
         (== (cons a d) p)))

(defn cdro [p d]
  (== (rest p) d))

(run* (x)
      ;;(resto [:c :o :r :n] [x :r :n])
      (cdro [:c :o :r :n] [x :r :n])
      )

(run* (l)
      (fresh (x)
             (resto l [:c :o :r :n])
             (firsto l x)
             (== :a x)))

(run* (q)
      (cdro [:a :c :o :r :n] [:c :O :r :n])
      (== true q))

;; ---

(run* (l)
      (conso [:a :b :c] [:d :e] l))

(run* (x)
      (conso x [:a :b :c] [:d :a :b :c]))

(run* (r)
      (fresh (x y z)
             (== [:e :a :d x] r)
             (conso y [:a z :c] r)))

(run* (x)
      (conso x [:a x :c] [:d :a x :c])
      ;;(conso2 x [:a x :c] [:d :a x :c])
      )

(run* (l)
      (fresh (x)
             (== [:d :a x :c] l)
             (conso x [:a x :c] l)))

(defn conso2 [a d p]
  (== (cons a d) p))

(run* (l)
      (fresh (d x y w s)
             (conso w [:a :n :s] s)
             (resto l s)
             (firsto l x)
             (== :b x)
             (resto l d)
             (firsto d y)
             (== :e y)))

;; ---

(run* (q)
      (emptyo [:grape :raisin :pear])
      (== true q))

(run* (q)
      (emptyo [])
      (== true q))

(run* (q)
      (emptyo q))

(defn eqo [x y]
  (== x y))

(run* (q)
      (eqo :pear :plum)
      (== true q))

(run* (q)
      (eqo :plum :plum)
      (== true q))

(cons [:split] [:pea])

(run* (r)
      (fresh (x y)
             (== (cons x (cons y [:salad])) r)))

(let [p (pair :pear [])]
  [p (nth p 0) (nth p 1) (.lhs p)])

(defn mycons [a b]
  (if (coll? b)
    (cons a b)
    (pair a b)))

(mycons :split :pea)
(mycons :split [])

(defn pairo [p]             ;; this is a dodgy definition, see https://github.com/clojure/core.logic/wiki/Differences-from-The-Reasoned-Schemer
  (fresh (a d)
         (conso a [d] p)))

(run* (q)
      (pairo (cons q [q]))
      (== true q))

(run* (q)
      (pairo [])
      (== true q))

(run* (q)
      (pairo :pair)
      (== true q))

(run* (x)
      (pairo x))

(run* (r)
      (pairo (cons r [:pear])))

(run* (r)
      (pairo [r :pear]))

