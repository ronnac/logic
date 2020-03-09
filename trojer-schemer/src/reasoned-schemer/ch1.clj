(ns reasoned-schemer.ch1
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:use [serializable.fn]))


;;In order to do relational programming,
;;we need only two constants:
;;  #s and #u,
;;and only three operators:
;;  ==, fresh, and conde.

;;#s and #u are reminiscent of the
;;Boolean constants: #t and #f in Scheme

;; #function[clojure.core.logic/fail]
;; or Unsuccesful
u#

;; #function[clojure.core.logic/succeed]
s#

(run* (q) u#)
;; => ()
;;since #u fails, and because the
;;expression (run∗ (q) g ...)
;;returns an empty sequence if any goal
;;in g ... fails.

(run* (q) s#)
;; => (_0)
;;not in the little Schemer:
;; _ (underscore) means
;;   "can take any value"
;;core.logic enumerates these
;; _0 _1 _2 etc. - more later

(run* (q) (== true q))
;; => (true)
;;because true is associated with q
;;if (== true q) succeeds.

(run* (q)
      u#
      (== true q))
;; => ()
;;because the expression
;;(run∗ (q) g ... (== true q))
;;has the value (
;;if the goals g . . . fail.

(run* (r)
      s#
      (== :corn r))
;; => (:corn)
;;because r is associated with corn
;;when (== corn r)succeeds.

(run* (r)
  u#
  (== :corn r))
;; => ()
;;because u# fails.

(run* (x)
      (let [x false]
        (== true x)))
;; => ()
;;since false is not equal to true.

(run* (q)
      (fresh (x)
             (== true x)
             (== true q)))
;; => (true)
;;because ‘(fresh (x ...) g .. .)’
;;binds fresh variables to x ...
;;and succeeds if the goals g ...
;;succeed.
;;(== true x) succeeds when x is fresh.

;;When is a variable fresh?
;;=> When it has no association.


(run* (q)
      (fresh (x)
             (== true x)
             (== x q)))
;; => (true)

;;===================
;; The Law of Fresh ;
;;===================
;;If x is fresh, then (≡ v x) succeeds
;;and associates x with v.

;;===================
;; Law of ==        ;
;;===================
;; (== v w) is the same as (== w v).

(run* (x)
      s#)
;; => (_0)
;;a symbol representing a fresh variable.†

(run* (x)
   (let [x false]
      (fresh (x)
          (== true x))))
;; => (_0)
;;since the x in (== true x) is the one
;;introduced by the fresh expression;
;;it is neither the x introduced in the
;;run expression nor the x introduced in
;;the lambda expression.

(run* (r)
  (fresh (x y)
    (== (cons x (cons y '())) r)))
;; => ((_0 _1))
;;For each different fresh variable
;;there is a symbol with an underscore
;;followed by a number. This entity is
;;not a variable but rather is a way of
;;showing that the variable was fresh.
;;We say that such a variable has been
;;reified.

(run* (r)
  (fresh (x y)
    (== (list x y) r)))
;; => ((_0 _1))
;; A Clojure way of doing the same as in previous example.

(run* (r)
  (fresh (x y)
    (== (conj [] x y) r)))
;; => ([_0 _1])
;;What holds for lists (previous
;;exercise) also holds for vectors

(run* (r)
  (fresh (x)
    (let [y x]
      (fresh (x)
        (== (cons y (cons x [y])) r)))))
;; => ((_0 _1 _0))
;;Within the inner fresh, x and y are
;;different variables, and since they
;;are still fresh, they get different
;;reified names.

(run* (r)
  (fresh (x)
    (let [y x]
      (fresh (x)
        (== (list y x y) r)))))
;; => ((_0 _1 _0))
;; A Clojure way of doing the same as in previous example.

(run* (q)
      (== false q)
      (== true q))
;; => ()
;;The first goal (== false q) succeeds,
;;associating false with q; true cannot
;;then be associated with q,
;;since q is no longer fresh.

(run* (q)
      (== false q)
      (== false q))
;; => (false)
;;In order for the run to succeed, both
;;(== false q) and (== false q) must
;;succeed. The first goal succeeds while
;;associating false with the fresh
;;variable q. The second goal succeeds
;;because although q is no longer fresh, false is already associated with it.

(run* (q)
  (fresh (x)
     (== x q)
     (== true x)))
;; => (true)
;;because q and x are the same.

(run* (r)
  (fresh (x)
    (== x r)))
;; => (_0)
;;because r starts out fresh and then r
;;gets whatever association x gets, but
;;both x and r remain fresh. When one
;;variable is associated with another,
;;we say they co-refer or share.
;;            ========    =====

(run* (q)
  (fresh (x)
    (== true x)
    (== x q)))
;; => (true)
;;because q starts out fresh and then
;;q gets x’s association.

(run* (q)
  (fresh (x)
    (== x q)
    (== true x)))
;; => (true)
;;because the first goal ensures that
;;whatever association x gets,
;;q also gets.

(run* (q)
  (fresh (x)
    (== true x)
    (== x q)))
;; => (true)
(run* (q)
  (fresh (x)
    (== true x)
    (== x q)))
;; => (true)

(cond
  false true
  :else false)
;; => false
;;because the question of the first
;;cond line is false, so the value
;;of the cond expression is determined
;;by the answer in the second
;;cond line.

;;Which false is the value?
;;=> The one in the "else: false"
;cond line.

(cond
  false s#
  :else u#)
;; => #object[clojure.core.logic$fail
;;it fails because the answer of the
;;second cond line is #u.


(run* (x)
  (conde
   (u# s#)
   (:else u#)))
;; => ()
;;fails, because the question of the
;;first conde line is the goal #u.


(run* (x)
  (conde
   (u# s#)
   (:else s#)))
;; => ()

(run* (x)
      (conde
        ((== :olive x) s#)
        ((== :oil x) s#)
        (:else u#)))
;; => (:olive :oil)
;; => (:olive :oil)

; "not supported", conde stmts that are
;;guaranteed to fail are not needed

(run 1 (x)
      (conde
        ((== :olive x) s#)
        ((== :oil x) s#)))
;; => (:olive)

(run* (x)
      (conde
        ((== :virgin x) u#)
        ((== :olive x) s#)
        (s# s#)
        ((== :oil x) s#)))

(run 2 (x)
      (conde
        ((== :extra x) s#)
        ((== :virgin x) u#)
        ((== :olive x) s#)
        (s# s#)
        ((== :oil x) s#)))

(run* (r)
      (fresh (x y)
             (== :split x)
             (== :pea y)
             (== (cons x [y]) r)))

(run* (r)
      (fresh (x y)
             (conde
               ((== :split x) (== :pea y))
               ((== :navy x) (== :bean y)))
             (== (cons x [y]) r)))

(run* (r)
      (fresh (x y)
             (conde
               ((== :split x) (== :pea y))
               ((== :navy x) (== :bean y)))
             (== (conj '(:soup) x y) r)))

;; ----

(defn teacupo [x]
  (conde
    ((== :tea x) s#)
    ((== :cup x) s#)))

(run* (x)
      (teacupo x))

(run* (r)
      (fresh (x y)
             (conde
               ((teacupo x) (== true y) s#)
               ((== false x) (== true y)))
             (== (cons x [y]) r)))

(run* (r)
      (fresh (x y z)
             (conde
               ((== y x) (fresh (x) (== z x)))
               ((fresh (x) (== y x)) (== z x)))
             (== (cons y [z]) r)))

(run* (r)
      (fresh (x y z)
             (conde
               ((== y x) (fresh (x) (== z x)))
               ((fresh (x) (== y x)) (== z x)))
             (== false x)
             (== (cons y [z]) r)))

(run* (q)
      (let [a (== true q)
            b (== false q)]        
        b))
