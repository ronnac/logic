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
                  (rembero x d res))
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
;; rembera => ([:d :d] [:d :d] [_0 _0] [:a :e])
;; core.logic/rembero => ([:d :d])
;; so rembera yields wrong solutions !!

(run 13 (w)
     (fresh (y z out)
            (rembero y (llist :a :b y :d z w) out)))
;; => (_0 _0
;; (_0 :- (!= (_1 :a)) (!= (_1 :b)))
;; (_0 :- (!= (_1 _1)) (!= (_1 :d)) (!= (_1 :a)) (!= (_1 :b)))
;; ((_0 . _1) :- (!= (_0 _0)) (!= (_2 _0)) (!= (_0 :d)) (!= (_0 :a)) (!= (_0 :b)))
;; ((_0 _1 . _2) :- (!= (_1 _1)) (!= (_1 :d)) (!= (_1 :a)) (!= (_1 :b)) (!= (_0 _1)) (!= (_3 _1)))
;; ((_0 _1 _2 . _3) :- (!= (_1 _2)) (!= (_4 _2)) (!= (_2 :a)) (!= (_2 _2)) (!= (_2 :d)) (!= (_2 :b)) (!= (_0 _2)))
;; ((_0 _1 _2 _3 . _4) :- (!= (_3 :d)) (!= (_2 _3)) (!= (_3 _3)) (!= (_3 :a)) (!= (_1 _3)) (!= (_3 :b)) (!= (_5 _3)) (!= (_0 _3)))
;; ((_0 _1 _2 _3 _4 . _5) :- (!= (_2 _4)) (!= (_4 :b)) (!= (_4 :d)) (!= (_0 _4)) (!= (_4 :a)) (!= (_1 _4)) (!= (_4 _4)) (!= (_6 _4)) (!= (_3 _4)))
;; ((_0 _1 _2 _3 _4 _5 . _6) :- (!= (_2 _5)) (!= (_3 _5)) (!= (_0 _5)) (!= (_5 :a)) (!= (_7 _5)) (!= (_5 :d)) (!= (_1 _5)) (!= (_5 _5)) (!= (_4 _5)) (!= (_5 :b)))
;; ((_0 _1 _2 _3 _4 _5 _6 . _7) :- (!= (_6 :a)) (!= (_0 _6)) (!= (_1 _6)) (!= (_4 _6)) (!= (_8 _6)) (!= (_5 _6)) (!= (_6 _6)) (!= (_2 _6)) (!= (_3 _6)) (!= (_6 :b)) (!= (_6 :d)))
;; ((_0 _1 _2 _3 _4 _5 _6 _7 . _8) :- (!= (_6 _7)) (!= (_7 :a)) (!= (_2 _7)) (!= (_9 _7)) (!= (_1 _7)) (!= (_3 _7)) (!= (_5 _7)) (!= (_4 _7)) (!= (_7 _7)) (!= (_0 _7)) (!= (_7 :b)) (!= (_7 :d)))
;; ((_0 _1 _2 _3 _4 _5 _6 _7 _8 . _9) :- (!= (_7 _8)) (!= (_8 :b)) (!= (_2 _8)) (!= (_4 _8)) (!= (_8 :a)) (!= (_6 _8)) (!= (_1 _8)) (!= (_8 :d)) (!= (_8 _8)) (!= (_0 _8)) (!= (_5 _8)) (!= (_3 _8)) (!= (_10 _8))))

;; ---

(defn surpriseo [s]
  (rembero s [:a :b :c] [:a :b :c]))

(run* (r)
     (surpriseo r))

(run* (r)               ;; wtf!!!
      (surpriseo r)
      (== :b r))
