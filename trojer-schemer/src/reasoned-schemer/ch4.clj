(ns reasoned-schemer.ch4
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

(defn mem [x l]
  (cond
    (empty? l) false
    (clojure.core/= (first l) x) l
    :else (mem x (rest l))))
;; => #'reasoned-schemer.ch4/mem

(mem :tofu [:a :b :peas :d :peas :e])
;; => false

(run* [out]
  (== (mem :tofu
           [:a :b :tofu :d :peas :e])
      out))
;; => ((:tofu :d :peas :e))

;;=====================================
;; p. 48
;;=====================================

(defn eq-caro [l x]
  (firsto l x))
;; => #'reasoned-schemer.ch4/eq-caro

(defn memo [x l out]      ;; not a boolean function needs out param
  (conde
   ((eq-caro l x) (== l out))
   ((s# (fresh (d)
               (resto l d)
               (memo x d out))))))
;; => #'reasoned-schemer.ch4/memo

;; The list?, lol?, and member?
;; definitions from the previous chapter;; have only Booleans as their values,
;; but mem,onthe otherhand, does not.
;; Because of this we need an additional;; variable, which here we call out,
;; that holds memo’s value.

(run* (out)
  (memo :tofu
        [:a :b :tofu :d :tofu "e"]
        out))
;; => ((:tofu :d :tofu "e") (:tofu "e"))

(run 1 (out)   ;; !!
  (fresh (x)
    (memo :tofu
          [:a :b x :d :tofu "e"] out)))
;; => ((:tofu :d :tofu "e"))

;;=====================================
;; p. 49
;;=====================================
(run* (r)
  (memo r
        [:a :b :tofu :d :tofu "e"]
        [:tofu :d :tofu "e"]))
;; => (:tofu)

(run* (q)
      (memo :tofu [:tofu :e] [:tofu :e])
      (== true q))
;; => (true)

(run* (q)
      (memo :tofu [:tofu :e] [:tofu])
      (== true q))
;; => ()

(run* (x)
  (memo :tofu [:tofu :e] [x :e]))
;; => (:tofu)


(run* (x)
  (memo :tofu [:tofu :e] [:peas x]))
;; => ()
                                        ;; because there is no value that, when
;; associated with x, makes [:peas x]
;; be [:tofu :e].


(run* (out) 
  (fresh (x)
    (memo :tofu
          [:a :b x :d :tofu :e] out)))
;; => ((:tofu :d :tofu :e) (:tofu :e))
;;=====================================
;; p. 50
;;=====================================

(run 12 (z) 
  (fresh (u)
    (memo :tofu
       (llist :a :b :tofu :d :tofu :e z)
       u)))
;; => (_0 _0 (:tofu . _0) (_0 :tofu . _1) (_0 _1 :tofu . _2) (_0 _1 _2 :tofu . _3) (_0 _1 _2 _3 :tofu . _4) (_0 _1 _2 _3 _4 :tofu . _5) (_0 _1 _2 _3 _4 _5 :tofu . _6) (_0 _1 _2 _3 _4 _5 _6 :tofu . _7) (_0 _1 _2 _3 _4 _5 _6 _7 :tofu . _8) (_0 _1 _2 _3 _4 _5 _6 _7 _8 :tofu . _9))
;; How do we get the first two 0’s?
;; The first 0 corresponds to finding
;; the first tofu.The second 0
;; corresponds to finding the second
;; tofu.
;; Where do the other ten lists come
;; from? 
;; In order for (memo :tofu (llist :a :b
;; :tofu :d :tofu :e  z) u) to succeed,
;; there must be a tofu in z. So memo
;; creates all the possible lists with
;; tofu as one element of the list.
;;  That’s very interesting!


;;=====================================
;; p. 51
;;=====================================
;; rember means remove member
;; it removes the first occurrence
;; of a given list member

(defn rember [x l]
  (cond (nil? l) []
         (= x (first l))(rest l)
        :else
         (cons (first l)
               (rember x (rest l)))))
;; => #'reasoned-schemer.ch4/rember

(rember :peas [:a :b :peas :d :peas :e])
;; => (:a :b :d :peas :e)

(defn rembera [x l out]
  (conde
   ((emptyo l) (== out []))
   ((eq-caro l x) (resto l out))
   (s# (fresh (res)
          (fresh (d)
                  (resto l d)
                  (rembera x d res))
          (fresh (a)
                (resto l a)
                (conso a res out))))))

;; Why are there three freshes in
;; (fresh (res)
;;   (fresh (d)
;;     (resto l d)
;;     (rembero x d res))
;;   (fresh (a)
;;     (resto l a)
;;     (conso a res out)))  ?

;; Because d is only mentioned in
;; (resto l d) and (rembero x d res)
;; a is only mentioned in (firsto l a)
;; and (conso a res out); but res is
;; mentioned throughout.

;;=====================================
;; p. 52
;;=====================================
(defn rembera [x l out]
(conde
  ((emptyo l) (== out []))
  ((eq-caro l x) (resto l out))
   (s# (fresh (a d res)
              (resto l d)
              (rembera x d res)
              (firsto l a)
              (conso a res out)))))
;; => #'reasoned-schemer.ch4/rembera

(defn rembera [x l out]
  (conde
   ((emptyo l) (== out []))
   ((eq-caro l x) (resto l out))
   (s# (fresh (a d res)
              (conso a d l)
              (rembera x d res)
              (conso a res out)))))
;; => #'reasoned-schemer.ch4/rembera

(run* (out)
  (fresh (y res)
    (rembera :peas
             [:a :b y :d :peas :e] res)
    (== out [y res])))
;; => ([:peas (:a :b :d :peas :e)] [_0 (:a :b _0 :d :e)] [_0 (:a :b _0 :d :peas :e)])

;;=====================================
;; p. 53
;;=====================================
(run* (out)
  (fresh (y z res)
    (rembera y [:a :b y :d z :e] res)
    (== out [y z res])))
;; => ([:a _0 (:b :a :d _0 :e)]
;; [:b _0 (:a :b :d _0 :e)]
;; [_0 _1 (:a :b :d _1 :e)]
;; [:d _0 (:a :b :d _0 :e)]
;; [_0 _0 (:a :b _0 :d :e)]
;; [:e _0 (:a :b :e :d _0)]
;; [_0 _1 (:a :b _0 :d _1 :e)])

;; Why is (:b :a :d _0 :e) the first value?
;; The b comes first because the a has been
;; removed.
;; In order to remove the a, y gets associated
;; with a. The y in the list is then replaced
;; with its value.

;; Why is (:a :b :d _0 :e) the second value?
;; In order to remove the a, y gets associated
;; with a. The y in the list is then replaced
;; with its value.

;; Why is (:a :b :d _1 :e) the third value?
;; The y has been removed.


;; Why is (:a :b _0 :d :e) the fourth value?
;; I don't understand this. 


(run* (r)
    (fresh (y z)
        (rembera y [y :d z :e] [y :d :e])
        (== [y z] r)))
;; => ([:d :d] [:d :d] [_0 _0] [:e :e])
;; core.logic/rembero => ([:d :d])
;; Why is [:d :d] the first value?
;; When y is :d and z is :d, then
;; (rembera :d [:d :d :d :e][:d :d :e])
;; succeeds.

;; Why is [:d :d] the second value?
;; => same answer

;; Why is [_0 _0] the third value?
;; rembera removes z from the list [y :d z :e]
;; ,yielding the list [y :d :e]; [y :d :e] is
;; always the same as out, [y :d :e]. Also, in
;; order to remove the z, y gets associated
;; with z,so they co-refer.
;; As long as y and z are the same, y can be
;; anything.

;; How is [:e :e] the fourth value?
;; rembero removes :e from the list
;; [y :d z :e], yielding the list [y :d z]
;; [y :d z] is the same as out, [y :d :e],
;; only when z is e. Also, in order to remove
;; the :e, y gets associated with :e.


(run 13 (w)
   (fresh (y z out)
      (rembera y (llist :a :b y :d z w) out)))
;; =>
;;(_0
;; _0
;; _0
;; _0
;; _0
;; ()
;; (_0 . _1)
;; (_0)
;; (_0 _1 . _2)
;; (_0 _1)
;; (_0 _1 _2 . _3)
;; (_0 _1 _2)
;; (_0 _1 _2 _3 . _4))

;; Why is _0 the first value?
;; When y is :a, out becomes ((b y d z ! w)),
;; which makes
;; (rembera y (a b y d z . w)(b y d z . w))
;; succeed

;; How is _0 the 2nd, 3rd, and 4th value?
;: This is the same as in the previous frame,
;; except that rembera removes b from the
;; original l, y from the original l, and d
;; from the original l,respectively.

;; How is _0 the fifth value?
;; Next, rembero removes z from l. When the
;; (eq-caro l x) question of the second conde
;; line succeeds, (car l) is z. The answer of
;; the second conde line, (cdro l out), also
;; succeeds, associating the cdr of l (the
;; fresh variable w) with the fresh variable
;; out. The variable out, however, is just
;; res, the fresh variable passed into the
;; recursive call to rembero.

;; How is () the sixth value?
;; Because none of the first five values in l
;; are removed. The (emptyo l) question of the
;; first conde line then succeeds, associating;; w with the empty list.

;; How is (0 . 1) the seventh value?
;; Because none of the first five values in l
;; are removed, and because we pretend that
;; the (emptyo l) question of the first conde
;; line fails. The (eq-caro l x) question of
;; the second conde line succeeds, however,
;; and associates w with a pair whose car is y
;; . The answer (cdro l out) of the second
;; conde line also succeeds, associating w
;; with a pair whose cdr is out.The variable
;; out, however, is just res, the fresh
;; variable passed into the recursive call to
;; rembero. During the recursion, the caro
;; inside the second conde line’s eq-caro
;; associates the fresh variable y with the
;; fresh variable a.

;; ---

(defn surpriseo [s]
  (rembero s [:a :b :c] [:a :b :c]))
;; => #'reasoned-schemer.ch4/surpriseo

(run* (r)
     (surpriseo r))
;; => ()

(run* (r)               ;; wtf!!!
      (surpriseo r)
      (== :b r))
;; => ()
