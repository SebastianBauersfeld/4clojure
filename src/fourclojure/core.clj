(ns fourclojure.core)

; #24 Sum it all up
(defn sum-it-all-up [lst]
  (reduce + lst))

; #46 Flipping out
(defn flipping [func]
  (fn [& args] (apply func (reverse args))))


; #80 Perfect Numbers
(defn perfect [n]
  (= (reduce + (filter #(zero? (mod n %)) (range 1 n))) n))


; #60 Sequence Reductions
(defn step-reduc
  ([func coll]
   (if (empty? coll)
     nil
     (step-reduc func (first coll) (rest coll))))
  ([func acc coll]
   (if (empty? coll)
     (list acc)
     (lazy-seq (cons
                 acc
                 (step-reduc func
                             (func acc (first coll))
                             (rest coll)))))))

