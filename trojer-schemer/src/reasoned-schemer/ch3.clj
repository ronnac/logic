(ns reasoned-schemer.ch3
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

(defn pairo [p]
  (fresh [a d]
         (== (lcons a d) p)))
;; => #'reasoned-schemer.ch3/pairo


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


;; The First Commandment
;; To transform a function whose value is a
;; Boolean into a function whose value is a
;; goal, replace cond with conde and unnest
;; each question and answer. Unnest the
;; answer #t (or #f) by replacing it with
;; #s (or #u).

;; Why is 0 the value associated with x in
(run* (x)
  (listo [:a :b x :d]))
;; => (_0)
;; When determining the goal returned by
;; list o, it is not necessary to determine
;; the value of x. Therefore x remains
;; fresh, which means that the goal
;; returnedfrom the call to list o succeeds
;; for all values associated with x.
;; When list o reaches the end of its
;; argument, it succeeds. But x does not
;; get associated with any value.


;; ========================================
;; p. 29
;; ========================================

(run 1 (x)
  (listo (llist :a :b :c x)))
;; => (())
;; Because ((a b c . x)) is a proper list
;; when x is the empty list.

(run 5 (x)
  (listo (llist :a :b :c x)))
;; => (() (_0) (_0 _1) (_0 _1 _2) (_0 _1 _2 _3))

;; In list? each cond line results in a
;; value, whereas in listo each conde
;; line results in a goal. To have each
;; conde result in a goal, we unnest each
;; cond question and each cond answer. Used
;; with recursion, a conde expression can
;; produce an unbounded number of values.
;; We have used an upper bound, 5 in the
;; previous frame, to keep from creating a
;; list with an unbounded number of value

;; ========================================
;; p. 30
;; ========================================


;; Consider the definition of lol?, where
;; lol? stands for list-of-lists?.
(defn lol? [l]
  (cond
    (empty? l) true
    (seq? (first l)) (lol? (rest l))
    :else false))
;; => #'reasoned-schemer.ch3/lol?
;; As long as each top-level value in the
;; list l is aproperlist, lol? returns
;; true. Otherwise, lol? returns false.

;; The definition of lol? has Boolean
;; values as questions and answers. lolo
;; has goals as questions and answers.
;; Hence, it uses conde instead of cond.
;; (seq? (first l)) and (lol? (rest l))
;; have been unnested.

(defn lolo [l]
  (conde
    ((emptyo l) s#)
    ((fresh (a)
       (firsto l a)
       (listo a))
     (fresh (d)
       (resto l d)
       (lolo d)))))
;; => #'reasoned-schemer.ch3/lolo

(run 1 (l) (lolo l))
;; => (())
;; Since l is fresh, (emptyo l) succeeds
;; and in the process associates
;; l with ().

;; ========================================
;; p. 31
;; ========================================
(run* (q) 
      (fresh (x y)
             (lolo [[:a :b] [x :c] [:d y]])
             (== true q)))
;; => (true)
;; since [[:a :b] [x :c] [:d y]] is a list
;; of lists.

(run 1 (q)
     (fresh (x)
            (lolo (llist [:a :b] x))
            (== true q)))
;; => (true)
;; because emotyo of a fresh variable
;; always succeeds and associates the fresh
;; variable, in this case x, with ().

(run 1 (x)
  (lolo (llist [:a :b] [:c :d] x))) 
;; => (())
;; since replacing x with the empty list
;; in ([:a :b] [:c :d] x) transforms it to
;; ([:a :b] [:c :d] '()),which is the same
;; as ([:a :b] [:c :d]).
(run 5 (x)
     (lolo (llist [:a :b] [:c :d] x)))      ;; hm...
;; => (() (()) ((_0)) (() ()) ((_0 _1)))

(run 5 (x)
    (lolo (llist '('a 'b) '('c 'd) x)))
;; => (() (()) ((_0)) (() ()) ((_0 _1)))

;; Is (tofu tofu) a twin? 
;; Yes, because it is a list of two
;; identical values.

;; ========================================
;; p. 32
;; ========================================

;; Is ((gg)(tofu tofu)) a list of twins?
;; Yes, since both (gg) and (tofu tofu)
;; are twins.

; The definition in the book TRS, too long
(defn twinso [s]
  (fresh (x y)
         (conso x y s)
         (conso x [] y)))
;; => #'reasoned-schemer.ch3/twinso

                                        ; T; The simpler definition
(defn twinso2 [s]
  (fresh (x)
         (== [x x] s)))
;; => #'reasoned-schemer.ch3/twinso2

(run* (q)
      (twinso2 [:tofu :tofu])
      (== true q))
;; => (true)

(run* (z)
      (twinso2 [z :tofu]))
;; => (:tofu)

;; ========================================
;; p. 33
;; =======================================

;lot stands for list-of-twins.
(defn loto [l]
  (conde
    ((emptyo l) s#)
    ((fresh (a)
            (firsto l a)
            (twinso2 a))
     (fresh (d)
            (resto l d)
            (loto d)))))
;; => #'reasoned-schemer.ch3/loto

(run* (z)
     (loto [[:g :g] z]))
;; => ([_0 _0])

;; ========================================
;; p. 34
;; =======================================
(run* (z)
  (loto (pairo [[:g :g] z])))
;; => ()

(run 5 (z)
  (loto (pairo [[:g :g] z])))
;; => ()

(run* (r)
     (fresh (w x y z)
            (loto [[:g :g] [:e w] [x y] z])
            (== [w [x y] z] r)))
;; => ([:e [_0 _0] [_1 _1]])

;; ========================================
;; p. 35
;; =======================================
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
