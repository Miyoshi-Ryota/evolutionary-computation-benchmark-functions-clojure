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

(defn rosenbrock-function
  "https://en.wikipedia.org/wiki/Rosenbrock_function
  {\\displaystyle f(\\mathbf {x} )=\\sum _{i=1}^{N-1}[100(x_{i+1}-x_{i}^{2})^{2}+(1-x_{i})^{2}]\\quad {\\mbox{where}}\\quad \\mathbf {x} =[x_{1},\\ldots ,x_{N}]\\in \\mathbb {R} ^{N}.}[4]\n"
  [a b x]
  (let [x_i (drop-last x)
        x_i1 (rest x)]
  (->> x_i
       (map #(+
               (* a (Math/pow (- %  (Math/pow %2 2)) 2))
               (Math/pow (- b %2) 2)) x_i1)
       (apply +))))
