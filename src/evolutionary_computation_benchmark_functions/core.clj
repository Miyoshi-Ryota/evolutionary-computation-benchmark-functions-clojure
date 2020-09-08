(ns evolutionary-computation-benchmark-functions.core
  (:require [clojure.spec.alpha :as s]))

(defn rastrigin-function
  "`A` and `theta` is double.
  `x` is col of double and each elements should be in range from -5.0 to 5.0."
  [A theta x]
  {:pre  [(s/valid? (s/coll-of number?) x)
          (s/valid? double? A)
          (s/valid? double? theta)]
   :post [(s/valid? (s/and double?) %)]}
  (let [n (count x)]
    (+ (* A n)
       (->> x
            (map #(- (* %1 %1) (* A (Math/cos (* theta Math/PI %1)))))
            (apply +)))))
