(ns logic-tutorial.tut1
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:use [clojure.core.logic.pldb]))

(db-rel parent x y)
;; => #'logic-tutorial.tut1/parent
(db-rel male x)
;; => #'logic-tutorial.tut1/male
(db-rel female x)
;; => #'logic-tutorial.tut1/female

(defn child [x y]
  (parent y x))
;; => #'logic-tutorial.tut1/child

(defn son [x y]
  (all
   (child x y)
   (male x)))
;; => #'logic-tutorial.tut1/son

(defn daughter [x y]
  (all
   (child x y)
   (female x)))
;; => #'logic-tutorial.tut1/daughter

(defn grandparent [x y]
  (fresh [z]
    (parent x z)
    (parent z y)))
;; => #'logic-tutorial.tut1/grandparent

(defn granddaughter [x y]
  (fresh [z]
    (daughter x z)
    (child z y)))
;; => #'logic-tutorial.tut1/granddaughter
