(ns info.josf.tulz)

(defn- total-seq-match [items pred patterns]
  (some (fn [pattern]
          (or (empty? items)
              (= pattern (mapv pred items))))
        patterns))

(defn- partial-seq-match [items pred patterns]
  (total-seq-match items pred (map (partial take (count items)) patterns)))

(defn take-while-matching 
  ([pred patterns coll]
   (take-while-matching pred patterns coll []))
  ([pred patterns coll accum]
   (println accum)
   (lazy-seq
     (let [have-match? (total-seq-match accum pred patterns)]
       (cond
         (and (not (seq coll)) have-match?) ; no more coll, done
         accum

         (and (not (seq coll)) (not have-match?))
         []

         (and (not have-match?)
              (not (partial-seq-match (conj accum (first coll)) pred patterns)))
         []

         ;; current accum matches, but not next
         have-match? 
         (do (println "done not next") (println (conj accum (first coll))) accum)

         ::otherwise-keep-going
         (take-while-matching
             pred patterns (rest coll) (conj accum (first coll))))))))
