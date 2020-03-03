(ns logic-examples.ancestors
(:refer-clojure :exclude [inc reify ==])
  (:use [clojure.core.logic])
  (:require  [clojure.core.logic.pldb :as pldb]))
; => nil
(pldb/db-rel father Father Son)
;; => #'logic-examples.ancestors/father
(pldb/db-rel mother Mother Son)
(pldb/db-rel brother Brother Sib)
;; => #'logic-examples.ancestors/brother
(pldb/db-rel male M)
;; => #'logic-examples.ancestors/male
(pldb/db-rel female F)
;; => #'logic-examples.ancestors/female

(defn parent [p child]
  (conde
   ((father p child))
   ((mother p child))))
;; => #'logic-examples.ancestors/parent

(defn brother [bro sib]
  (fresh [p]
         (parent p bro)
         (parent p sib)
         (male bro)
         (!= bro sib)))
;; => #'logic-examples.ancestors/brother

(defn uncle [u person]
  (fresh [p]
         (brother u p)
         (parent p person)))
;; => #'logic-examples.ancestors/uncle

(def facts
  (pldb/db
   (concat (
     (map #(cons father %)
       [['terach 'abraham]
        ['terach 'nachor]
        ['terach 'haran]
        ['abraham 'isaac]
        ['haran 'lot]
        ['haran 'milcah]
        ['haran 'yiscah]
        ['sarah 'isaac]])
     (map #(cons [male] %) ['terach
                       'abraham
                       'nachor
                       'haran
                       'isaac
                       'lot
                       'sarah
                       'milcah
                       'yiscah])))))
