(ns reasoned-schemer.ch3
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

(defn pairo [p]
  (fresh [a d]
         (== (lcons a d) p)))
;; => #'reasoned-schemer.ch3/pairo

(defn listo [l]
  (conde
    ((emptyo l) s#)
    ((pairo l)
     (fresh (d)
            (resto l d)
            (listo d)))))
;; => #'reasoned-schemer.ch3/listo

(run* (x)
      (listo [:a :b x :d]))
;; => (_0)

(run 1 (x)
     (listo (llist :a :b :c x)))
;; => (())

(run 5 (x)
     (listo (llist :a :b :c x)))
;; => (() (_0) (_0 _1) (_0 _1 _2) (_0 _1 _2 _3))

(run* (x)
      (pairo [:a :b]))
;; => (_0)
;; ========================================
;; p. 27
;; ========================================

;; Consider the definition of list?.
;; reasoned-schemer.ch3> (source list?)
;; (defn list?
;;   [x] (instance?
;;          clojure.lang.IPersistentList x)
;; )

(list? '('(:a)'(:a :b) :c))
;; => true
(list? '())
;; => true
(list? :s)
;; => false
(llist :date :s)
;; => (:date . :s)
(list? (llist :date :s))
;; => false

(defn listo [l]
  (conde
   [(emptyo l) s#]
   [(pairo l)
    (fresh [d]
      (resto l d)
      (listo d))]))
;; => #'reasoned-schemer.ch3/listo

;; ========================================
;; p. 28
;; ========================================
;; Where does
;; (fresh [d] (resto l d) (listo d))
;; come from?
;; It is an unnesting of (list? (cdr l)).
;; First we take the cdr of l and associate
;; it with a fresh variable d, and then we
;; use d in the recursive call.




;; ---

(defn lolo [l]
  (conde
    ((emptyo l) s#)
    ((fresh (a)
            (firsto l a)
            (listo a))
     (fresh (d)
            (resto l d)
            (lolo d)))))

(run* (q)
      (fresh (x y)
             (lolo [[:a :b] [x :c] [:d y]])
             (== true q)))

(run 1 (q)
     (fresh (x)
            (lolo (llist [:a :b] x))
            (== true q)))

(run 5 (x)
     (lolo (llist [:a :b] [:c :d] x)))      ;; hm...

(defn twinso [s]
  (fresh (x y)
         (conso x y s)
         (conso x [] y)))

(defn twinso2 [s]
  (fresh (x)
         (== [x x] s)))

(run* (q)
      (twinso2 [:tofu :tofu])
      (== true q))

(run* (z)
      (twinso [z :tofu]))

(defn loto [l]
  (conde
    ((emptyo l) s#)
    ((fresh (a)
            (firsto l a)
            (twinso a))
     (fresh (d)
            (resto l d)
            (loto d)))))

(run* (z)
     (loto [[:g :g] z]))

(run* (r)
     (fresh (w x y z)
            (loto [[:g :g] [:e w] [x y] z])
            (== [w [x y] z] r)))

(defn listofo [predo l]
  (conde
    ((emptyo l) s#)
    ((fresh (a)
            (firsto l a)
            (predo a))
     (fresh (d)
            (resto l d)
            (listofo predo d)))))

(run* (out)
      (fresh (w x y z)
             (== [[:g :g] [:e w] [x y] z] out)
             (listofo twinso out)))

;; ---

(defn eq-caro [l x]
  (firsto l x))

(defn membero2 [x l]
  (conde
    ;;((emptyo l) u#)          ;; redundant
    ((eq-caro l x) s#)
    (s#                      ;; s# as else here, must be possible to do better?
     (fresh (d)
            (resto l d)
            (membero2 x d)))))

(run* (q)
      (membero2 :olive [:virgin :olive :oil])
      (== true q))

(run* (y)
      (membero2 y [:hummus :with :pita]))

(defn identity2 [l]
  (run* [y]
        (membero y l)))

(identity2 [23 23])

(run 1 (x)
     (membero :e [:pasta :e x :fagioli]))

(run 1 (x)
     (membero :e [:pasta x :e :fagioli]))

(run* (r)
      (fresh (x y)
             (membero :e [:pasta x :fagioli y])
             (== [x y] r)))

(run 5 (l)
     (membero2 :tofu l))

(defn pmembero [x l]
  (conde
    ((eq-caro l x) (resto l []))
    ((eq-caro l x) (fresh (a d)
                          (resto l [a d])))   ;; this line is wrong
    (s# (fresh (d)
               (resto l d)
               (pmembero x d)))))

(run* (q)
      (pmembero :tofu [:a :b :tofu :d :todu])
      (== true q))

;; ---

(defn first-value [l]
  (run 1 (y)
       (membero y l)))

(first-value [:pasta :e :fagioli])
