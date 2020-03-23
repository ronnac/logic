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

;; What is the value of
[ (first [:grape :raisin :pear])
 (first [:a :b :c])]
;; => [:grape :a]

(run* (r)
  (fresh (x y)
    (firsto [:grape :raisin :pear] x)
    (firsto [:a :b :c] y)
    ;; u#
    (== [x y] r)))
;; => ([:grape :a])
;; That’s the same: ([:grape :a]).

;;===============================
;;  page 19
;;===============================

(rest [:grape :raisin :pear])
;; => (:raisin :pear)
;; That’s easy: ((raisin pear)).

(first (rest [:a :c :o :r :n]))
;; => :c

(run* (r)
      (fresh (v)
             (resto [:a :c :o :r :n] v)
             (firsto v r)))
;; => (:c)
;; The process of transforming
;; (first (rest l)) into (firsto l v) and
;; (resto v r) is called unnesting.
;; Some readers may recognize the
;; similarity between unnesting and
;; continuation-passing style.

;; Here is the definition of cdro.
;; Oh. It is almost the same as caro.

(defn cdro [p d]
  (fresh (a)
         (== (cons a d) p)))
;; => #'reasoned-schemer.ch2/cdro

;;This definition also works
(defn cdro [p d]
  (== (rest p) d))
;; => #'reasoned-schemer.ch2/cdro

(cons [:raisin :pear] '(:a))
;; => ([:raisin :pear] :a)

(cons
  (rest [:grape :raisin :pear])
  (list (first [:a :b :c])))
;; => ((:raisin :pear) :a)

;;===============================
;;  page 20
;;===============================

(run* (r)
  (fresh (x y)
     (resto [:grape :raisin :pear] x)
     (firsto [:a :b :c] y)
     (== [x y] r))))
;; => ([(:raisin :pear) :a])

(run* (x)
      ;;(resto [:c :o :r :n] [x :r :n])
      (cdro [:c :o :r :n] [x :r :n])
      )
;; => (:o)
;; because [:o :r :n] is the cdr of
;; [:c :o :r :n])
;; so x gets associated with :o.;; (
(run* (l)
      (fresh (x)
             (resto l [:c :o :r :n])
             (firsto l x)
             (== :a x)))
;; => ((:a :c :o :r :n))
;; because if the rest of l is
;; [:c :o :r :n], then l must be the
;; vector [a :c :o :r :n] where a is
;; the fresh variable introduced in the
;; definition of resto. Taking the resto
;; of l associates the first of l with
;; x. When we associate x with :a, we
;; also associate a, the first of l,
;; with :a, so l is associated with the
;; vector [:a :c :o :r :n]

(run* (l)
  (conso [:a :b :c] [:d :e] l))
;; => (([:a :b :c] :d :e))
;; since conso associates l with
;; (cons [:a :b :c] [:d :e])

(run* (x)
  (conso x [:a :b :c] [:d :a :b :c]))
;; => (:d)
;; Since (cons :d [:a :b :c])
;; is [:d :a :b :c]
;; conso associates x with :d.

(run* (r)
  (fresh (x y z)
    (== [:e :a :d x] r)
    (conso y [:a z :c] r)))
;; => ([:e :a :d :c])
;; because first we associate r with a
;; vector whose last element is the
;; fresh variable x. We then perform
;; the conso, associating x with :c, z
;; with :d, and y with :e.

;;===============================
;;  page 21
;;===============================

(run* (x)
  (conso x [:a x :c] [:d :a x :c])
  ;;(conso2 x [:a x :c] [:d :a x :c])
  )
;; => (:d)
;; preparation for the next exercise

(run* (l)
  (fresh (x)
    (== [:d :a x :c] l)
    (conso x [:a x :c] l)))
;; => ([:d :a :d :c])
;; because l is [:d :a x :c]. Then when
;; we conso x onto [:a x :c], we
;; associate x with :d.

(run* (l)
  (fresh (x)
    (conso x [:a x :c] l)
    (== [:d :a x :c] l)))
;; => ((:d :a :d :c))
;; because we cons x onto [:a x :c] and
;; associate l with the vector
;; [x :a x :c]. Then when we associate l
;; with  [:d :a x :c], we associate
;; x with d
.
;; Define conso using ==.
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
;; => ((:b :e :a :n :s))

;; l must clearly be a five element
;; list, since s is (rest l). Since l is
;; fresh, (resto l s) places a fresh
;; variable in the first position of l,
;; while associating w and [:a :n :s]
;; with the second position and the rest
;; of the first of l, respectively. The
;; first variable in l gets associated
;; with x, which in turn gets associated
;; with b. The rest of l is a list whose
;; first element is the variable w.That
;; variable gets associated with y,
;; which in turn gets associated with e.

;; TRS null? is called empty?
;; in core.logic
(empty? [:grape :raisin :pear])
;; => false

(empty? [])
;; => true

;;===============================
;;  page 22
;;===============================

(run* (q)
      (emptyo [:grape :raisin :pear])
      (== true q))
;; => ()

(run* (q)
      (emptyo [])
      (== true q))
;; => (true)

(run* (q)
      (emptyo q))
;; => (()

;; define emptyo using unifier
(defn emptyo2 [x]
   (== [] x))
;; => #'reasoned-schemer.ch2/emptyo2

(run* (q)
  (emptyo2 [])
  (== true q))
;; => (true)
;; testing emptyo2

(= :pear :plum)
;; => false

(= :plum :plum)
;; => true

                                        ;; This defn is actually on page 23
;; but we need the function now
(defn eqo [x y]
  (== x y))
;; => #'reasoned-schemer.ch2/eqo

(run* (q)
      (eqo :pear :plum)
      (== true q))
;; => ()

;;===============================
;;  page 23
;;===============================

(run* (q)
      (eqo :plum :plum)
      (== true q))
;; => (true)

(cons [:split] [:pea])
;; => ([:split] :pea)

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

(defn pairo [p]
  ;; this is a dodgy definition, see
  ;; https://github.com/clojure/core.logic/wiki/Differences-from-The-Reasoned-Schemer
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

