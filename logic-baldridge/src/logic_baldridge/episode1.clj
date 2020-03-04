(ns logic-baldridge.episode1)
;; https://tbaldridge.pivotshare.com/categories/logic-programming/1630/media
;; or
;; https://www.youtube.com/playlist?list=PLhi8pL3xn1OSlyhqnqFmH8il3z_LiYGza

(defn lvar
	([] (lvar ""))
	([nm] (gensym (str nm "_"))))

(defn lvar? [v]
	(symbol? v))

(defn walk [s u]
	(if-let [pr (get s u)]
    (if (lvar? pr)
			(recur s pr)
			pr)
		s))

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
	(walk {} s));; => {}

(let [s (lvar "s")
      v (lvar "y")]
	(walk {s v v 42} s))
;; => 42
	
(unify {} (lvar "s") 42)

(let [s (lvar "s")
v (lvar "v")]
(walk {s v v 42} s))
(unify { (lvar "s") (lvar "v"))

(unify {} 1 1)|

(defn == [a b]
	(fn [s]
		(if-let [v (unify s a b)]
			[v]
			[])))

((== 1 2) {})
((== (lvar "foo") 1) {})
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

((-conj
	(== b a)
	(== a 42)
	(== 1 2)) {}))

(defn -disj [& goals]
    (fn [s]
			(mapcat (fn [goal]
											 (goal s))
				goals)))

(let [a (lvar "a")
        b (lvar "b")]
    ((-disj (conj
    					 		(== b a}
				         (== a 42)
								(== 1 2))
								(== b 11)) {}))
 
