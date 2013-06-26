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
