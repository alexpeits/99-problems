(ns ninety-nine.lists)

(defn my-last
  [lst]
  (if (empty? (rest lst))
    (first lst)
    (recur (rest lst))))

(defn my-but-last
  [lst]
  (if (empty? (rest (rest lst)))
    lst
    (recur (rest lst))))

(defn element-at
  [lst pos]
  (if (= pos 0)
    (first lst)
    (recur (rest lst) (dec pos))))

(defn my-length
  [lst]
  (loop [coll lst
         count 0]
    (if (empty? coll)
      count
      (recur (rest coll) (inc count)))))

(defn my-reverse
  [lst]
  (loop [coll lst
         res []]
    (if (empty? coll)
      res
      (recur (butlast coll) (conj res (last coll))))))


(defn palindrome?
  [lst]
  (= lst (my-reverse lst)))
