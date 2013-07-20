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



; #58 Function Composition
(defn compose [& fns]
  (letfn [ (call [fns args]
             (if (== (count fns) 1)
               (apply (first fns) args)
               ((first fns) (call (rest fns) args))))]
    (fn [& args] (call fns args))))


; #77 Anagram Finder
; good lord, that's ugly, I need more practice...
(defn afinder [words]
  (letfn [(agram [w1 w2]
            (= (akey w1) (akey w2)))
          (akey [w]
            (reduce #(assoc %1 %2 (if-let [val (%1 %2)] (inc val) 1)) {} w))]
    (set
      (map val 
           (filter #(> (count (val %)) 1) 
                   (reduce
                     #(assoc %1 (akey %2) (if-let [s (%1 (akey %2))] (conj s %2) #{%2}))
                     {}
                     words))))))


; #78 Reimplement Trampoline
(defn tramp [f & args]
  (letfn [(invoke [f]
            (if (fn? f)
              (recur (f))
              f))]
    (invoke (apply f args))))


; # 102 intoCamelCase
(defn intoCamelCase [s]
  (letfn [(cap [x] (cons (java.lang.Character/toUpperCase (first x)) (remove- (rest x))))
          (remove- [x] (if (= (first x) \-)
                         (cap (rest x)) 
                         (if (= (first x) nil)
                           x
                           (cons (first x) (remove- (rest x))))))]
    (apply str (remove- s))))


; #custom splits a string with hyphens into a vector of substrings
(defn mysplit [s]
  (map
    (partial apply str)
    (reverse (reduce
               #(if (= %2 \-)
                  (cons [] %)
                  (cons (conj (first %) %2) (rest %)))
               (list [])
               s))))

; #custom returns all permutations of a sequence s
(defn powerset [s]
  (if (empty? s)
    #{s}
    (let [e (first s)
          power-e (powerset (disj s e))]
      (clojure.set/union
        power-e
        (set (map #(conj % e) power-e))))))


; great (non-recursive) solution for powerset, by http://halfabrane.blogspot.com/2011/12/power-sets.html
(comment
  (defn power-set [s]
    (reduce (fn [ss x] (concat ss (map #(conj % x) ss))) [#{}] s)))


; #custom Ackermann's function
(defn ackermann [m n]
  (cond
    (= m 0) (inc n)
    (= n 0) (recur (dec m) 1)
    :else (recur (dec m) (ackermann m (dec n)))))



; #custom permutate all items in a set
(defn permuts [s]
  (letfn [(weave [el ls]
            (map #(cons el %) ls))]
    (if (seq s)
      (apply concat (map #(weave % (permuts (disj s %))) s))
      (list ()))))

; #custom permutate all items in a list
(defn permutl [l]
  (letfn [(idx2l [idxstr]
            (map #(nth l %)  idxstr))]
    (map idx2l (permuts (set (range (count l))))))) 


; #custom implementation of map
(defn mymap [f xs]
  (if (seq xs)
    (cons (f (first xs)) (mymap f (rest xs)))
    ()))

; #custom tail-recursive implementation of map
(defn mymaptr [f xs]
  (letfn [(helper [xs acc]
            (if (seq xs)
              (recur (rest xs) (conj acc (f (first xs))))
              acc))]
    (seq (helper xs []))))


; #custom implementation of reduce
(defn myreduce [f v xs]
  (if (seq xs)
    (recur f (f v (first xs)) (rest xs))
    v))
