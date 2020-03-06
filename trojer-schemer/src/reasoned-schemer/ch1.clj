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

(run* (x)
      (let [x false]
        (== true x)))

(run* (q)
      (fresh (x)
             (== true x)
             (== true q)))
(run* (q)
      (fresh (x)
             (== true x)
             (== x q)))

(run* (x)
      (let [x false]
        (fresh (x)
               (== true x))))

(run* (r)
      (fresh (x y)
             (== (cons x (cons y '())) r)))

(run* (r)
      (fresh (x y)
             (== (conj [] x y) r)))

(run* (r)
      (fresh (x)
             (let [y x]
               (fresh (x)
                      (== (cons y (cons x [y])) r)))))

(run* (q)
      (fresh (x)
             (== x q)
             (== true x)))

;; ---

(run* (x)
      (conde
        ((== :olive x) s#)
        ((== :oil x) s#)
        (:else u#)))            ;; "not supported", conde stmts that are guaranteed to fail is not needed

(run 1 (x)
      (conde
        ((== :olive x) s#)
        ((== :oil x) s#)))

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
