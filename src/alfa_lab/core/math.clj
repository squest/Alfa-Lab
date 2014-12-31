(ns alfa-lab.core.math)

(defn expt
  [a m]
  (if (zero? m) 1 (*' a (expt a (dec m)))))

(def limits (expt 10 999))

(defn prime?
  [p]
  (let [lim (int (+ 1 (Math/sqrt p)))]
    (loop [i (int 3)]
      (if (> i lim)
        true
        (if (= 0 (rem p i))
          false
          (recur (+ 2 i)))))))

(defn sum-primes
  [lim]
  (loop [i (int 7) res (int 10)]
    (if (> i lim)
      res
      (recur (+ i 2)
             (if (prime? i) (+ i res) res)))))

(defn fibo
  [lim]
  (loop [i 1 j 1 idx 1]
    (if (> i lim)
      idx
      (recur (+' i j) i (+ 1 idx)))))
