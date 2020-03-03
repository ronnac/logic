(ns primer
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic))
;; => nil

;; Constraints
;; Motivating Example
(run* [q]
  (membero q [1 2 3])
  (membero q [2 3 4]))
;; => (2 3)

;; Unification of a single lvar with a literal
(run* [q]
  (== q 1))
;; => (1)

(run* [q]
  (== q {:a 1 :b 2}));; => ({:a 1, :b 2})

(run* [q]
  (== {:a q :b 2} {:a 1 :b 2}))
;; => (1)

(run* [q]
  (== q '(1 2 3)))
;; => ((1 2 3)) q can only take this list as value


;; union of mutually exclusive sets
(run* [q]
  (== q 1)
  (== q 2))
;; => ()

;; Unification of two lvars 
(run* [q]
  (membero q [1 2 3]))
;; => (1 2 3)

(run* [q]
  (membero q [1 2 3])
  (membero q [3 4 5]))
;; => (3)

(run* [q]
  (fresh [a]
    (membero a [1 2 3])
    (membero q [3 4 5])
    (== a q)))
;; => (3)

;; The final operator, cond
;; a bit like logical or
(run* [q]
  (conde
   [succeed succeed succeed succeed]))
;; => (_0)

(run* [q]
  (conde
   [succeed succeed fail succeed]))
;; => ()

(run* [q]
  (conde
   [succeed]
   [succeed]))
;; => (_0 _0)

(run* [q]
  (conde
   [succeed]
   [fail]))
;; => (_0)

(run* [q]
  (conde
   [(== q 2) (== q 1)]))
;; => ()

(run* [q]
  (conde
   [(== q 1)]
   [(== q 2)]))
;; => (1 2)

(run* [q]
  (conde
   [(== q 2) (== q 1)]))
;; => ()

(run* [q]
  (conde
   [(== q 1)]
   [(== q 2)]))
;; => (1 2)

;;More Goals
(run* [q]
  (conso 1 [2 3] q))
;; => ((1 2 3))

(run* [q]
  (conso 1 q [1 2 3])
);; => ((2 3))

(run* [q]
  (conso 1 [2 q] [1 2 3]))
;; => (3)

(run* [q]
  (resto [1 2 3 4] q))
;; => ((2 3 4))

;; Resto
(run* [q]
  (resto [1 2 3 4] q))
;; => ((2 3 4))

;; Membero
(run* [q]
  (membero q [1 2 3]))
;; => (1 2 3)

(run* [q]
  (membero 7 [1 3 8 q]))
;; => (7)

