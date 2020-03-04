(ns logic-baldridge.episode1)
;; https://tbaldridge.pivotshare.com/categories/logic-programming/1630/media
;; or
;; https://www.youtube.com/playlist?list=PLhi8pL3xn1OSlyhqnqFmH8il3z_LiYGza

;; An example logic engine based on mukanren
;; http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf

(defn lvar
	([] (lvar ""))
	([nm] (gensym (str nm "_"))))   ; nm ... name of the lvar

;; Check if a given Var is an lvar (logic variable)
;; For the sake of this demo it shall be enough to check whether it is a symbol
(defn lvar? [v]
	(symbol? v))

;; The walk operator searches for a variable's value in the
;; substitution. When a non-variable term is walked,
;; the term itself is returned. 
(defn walk [s u]
	(if-let [pr (get s u)]
    (if (lvar? pr)
			(recur s pr)
			pr)
		u))

(defn unify [s u v]
	(let [u (walk s u)
        v (walk s v)]
		(cond
			(and (lvar? u)
			(lvar? v)
			(= u v)) s
		(lvar? u) (assoc s u v)
		(lvar? v) (assoc s v u)
		:else (and (= u v) s))))

(let [s (lvar "s") ]
	(walk {} s))
;; => s_12443

(let [s (lvar "s")
      v (lvar "y")]
	(walk {s v v 42} s))
;; => 42

(unify {} (lvar "s") 42)
;; => {s_12450 42}

(unify {} (lvar "s") (lvar "v"))
;; => {s_12453 v_12454}

(unify {} 1 2)
;; => false

(unify {} 1 1)
;; => {}

(defn == [a b]
	(fn [s]
		(if-let [v (unify s a b)]
			[v]
			[])))
;; => #'logic-baldridge.episode1/==

((== 1 2) {})
;; => []

((== 1 1) {})
;; => [{}]

((== (lvar "foo") 1) {})
;; => [{foo_12471 1}]

((== (lvar "foo") 1) {})

(defn -conj
	( [a] a)
	( [a b]
		(fn [s]
			(for [aret (a s)
							:when aret
							bret (b aret)
							:when bret]
				bret)))
	( [a b & more]
		(-conj a (apply -conj b more))))

(let [a (lvar "a")
      b (lvar "b")]
  ((-conj
    (== b a)
    (== a 42)
    (== 1 2)) {}))
;; => ()

(defn -disj [& goals]
    (fn [s]
			(mapcat (fn [goal]
                (goal s))
          goals)))

(let [a (lvar "a")
      b (lvar "b")]
    ((-disj (-conj
              (== b a)
              (== a 42)
              (== 1 2))
            (== b 11)) {}))



