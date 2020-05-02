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


;; ---

(defn rembero [x l out]
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

(defn rembero [x l out]
  (conde
   ((emptyo l) (== out []))
   ((eq-caro l x) (resto l out))
   (s# (fresh (a d res)
              (resto l d)
              (rembero x d res)
              (firsto l a)
              (conso a res out)))))

(defn rembero [x l out]
  (conde
   ((emptyo l) (== out []))
   ((eq-caro l x) (resto l out))
   (s# (fresh (a d res)
              (conso a d l)
              (rembero x d res)
              (conso a res out)))))

(run* (out)
     (fresh (y res)
            (rembero :peas [:a :b y :d :peas :e] res)
            (== out [y res])))

(run* (out)
      (fresh (y z res)
             (rembero y [:a :b y :d z :e] res)
             (== out [y z res])))

(run* (r)
      (fresh (y z)
             (rembero y [y :d z :e] [y :d :e])
             (== [y z] r)))

(run 13 (w)
     (fresh (y z out)
            (rembero y (llist :a :b y :d z w) out)))

;; ---

(defn surpriseo [s]
  (rembero s [:a :b :c] [:a :b :c]))

(run* (r)
     (surpriseo r))

(run* (r)               ;; wtf!!!
      (surpriseo r)
      (== :b r))
