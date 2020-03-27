(ns logic.core
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:require [clojure.core.logic.pldb :as facts])
  (:require [clojure.core.logic.fd :as fd]))

;;unification
(run* [q]
      (== 1 q))
;; => (1)

;;conde
(run* [q]
      (conde
        [(== q 1)]
        [(== q "zwei")]))
;; => (1 "zwei")

;;disunification
(run* [q]
      (conde
        [(== q 1)]
        [(== q 2)])
      (!= q 2))
;; => (1)

;;membero
(run* [s p o] ;; subject predicate object
      (membero s [:mother :child])
      (membero o [:mother :child])
      (membero p [:loves :has])
      (!= s o))
;; => ([:mother :loves :child] [:mother :has :child] [:child :loves :mother] [:child :has :mother])

;;distincto
(run* [s p o]
      (membero s [:mother :child])
      (membero o [:mother :child])
      (membero p [:loves :has])
      (distincto [s o]))
;; => ([:mother :loves :child] [:mother :has :child] [:child :loves :mother] [:child :has :mother])

;;everyg
(run* [s p o]
      (everyg #(membero % [:mother :child])
              [s o])
      (membero p [:loves :has])
      (distincto [s o]))
;; => ([:mother :loves :child] [:mother :has :child] [:child :loves :mother] [:child :has :mother])

;;fresh
(run* [languages]
      (fresh [a b c d]
             (== a "romansh")
             (== b "italian")
             (== c "french")
             (== d "german")
             (== languages [a b c d])))
;; => (["romansh" "italian" "french" "german"])

;;Achtung: fresh creates new new lexical scope
(run* [q]
      (== q 1)
      (fresh [q]
             (== q 2)))
;; => (1)

;;fd/interval fd/in
(run* [q]
      (fd/in q (fd/interval 0 9)))
;; => (0 1 2 3 4 5 6 7 8 9)

(run* [q]
      (fresh [a b]
        (fd/in a b (fd/interval 0 9))
        (fd/+ a b 10)
        (== q [a b])))     
;; => ([1 9] [2 8] [3 7] [4 6] [5 5] [6 4] [7 3] [8 2] [9 1])

;;reset #1
;; In which year was Julia twice as old as Clodette?
;; Julia was born in 1978, 2 years before Clodette.
(run* [q]
      (fresh [julia clodette year]
             (fd/in julia clodette (fd/interval 0 120))
             (fd/in year (fd/interval 1978 2100))
             (fd/eq
               (= (- year julia) 1978)
               (= (- julia clodette) 2)
               (= julia (* 2 clodette)))
             (== q year)))
;; => (1982)

;;facts and rels
(facts/db-rel eats creature1 creature2)
;; => #'logic.core/eats

(def factbase
  (facts/db
    [eats :shark :seal]
    [eats :seal :tuna]
    [eats :tuna :herring]
    [eats :human :seal]
    [eats :human :tuna]
    [eats :human :calamar]
    [eats :shark :human]
    [eats :seal :calamar]
    [eats :calamar :prawn]))
;; => #'logic.core/factbase

;; All food chains of length 4 with shark on top
(facts/with-db
  factbase
  (run* [q]
    (fresh [x y z]
      (eats :shark x)
      (eats x y)
      (eats y z)
      (== q [:shark x y z]))))
;; => ([:shark :seal :calamar :prawn] [:shark :seal :tuna :herring] [:shark :human :tuna :herring] [:shark :human :seal :calamar] [:shark :human :seal :tuna] [:shark :human :calamar :prawn])

;;sudoku black-box
(comment
(sodoku
  [0 0 0 0 1 3 2 6 0
   5 0 1 6 0 0 9 0 0
   0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0
   0 0 5 0 6 0 3 0 4
   0 6 8 3 0 9 0 0 0
   0 2 3 0 8 0 0 0 9
   8 0 0 9 0 0 1 2 0
   1 7 0 2 3 0 0 8 5])
)

;; board is initialized with grounded
;; lvars where we got hints and free
;; lvars otherwise
(defn init-board [vars hints]
  ;;check for emptiness
  (if 
    (seq vars)
    (let [hint (first hints)]
      (all
        (if 
          (zero? hint)
          succeed
          ;;else
          (== (first vars) hint))
        (init-board (next vars) (next hints))))
    ;;else - emptiness
    succeed))

;;returns a square of 3x3 lvars starting at x:y
(defn square [rows x y]
  (for [x (range x (+ x 3))
        y (range y (+ y 3))]
    (get-in rows [x y])))

;;workhorse
(defn sodoku [hints]
  (let [board (repeatedly 81 lvar)
        rows (->> board (partition 9) (map vec) (into []))
        cols (apply map vector rows)
        squares (for [x (range 0 9 3)
                      y (range 0 9 3)]
                  (square rows x y))]
    (run 1 [q]
      (== q board)
      (everyg #(fd/in % (fd/domain 1 2 3 4 5 6 7 8 9)) board)
      (init-board board hints)
      (everyg fd/distinct rows)
      (everyg fd/distinct cols)
      (everyg fd/distinct squares))))

(def gotit
  (sodoku
    [0 0 0 0 1 3 2 6 0
     5 0 1 6 0 0 9 0 0
     0 0 0 0 0 0 0 0 0
     0 0 0 0 0 0 0 0 0
     0 0 5 0 6 0 3 0 4
     0 6 8 3 0 9 0 0 0
     0 2 3 0 8 0 0 0 9
     8 0 0 9 0 0 1 2 0
     1 7 0 2 3 0 0 8 5]))

;;pretty print
(doseq [r (partition 9 (first gotit))]
    (println r))
  






