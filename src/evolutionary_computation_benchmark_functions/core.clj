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

(defn ackley-function
  "see definition: https://www.sfu.ca/~ssurjano/ackley.html
   In above site, variable is ```Recommended variable values are: a = 20, b = 0.2 and c = 2π```"
  [a b c x]
  (let [dimension (count x)]
    (+ (* (* -1 a)
          (Math/exp (* (* -1 b)
                       (Math/sqrt (* (/ 1 dimension)
                                     (apply + (map #(* % %) x)))))))
       (* -1 (Math/exp (* (/ 1 dimension)
                          (apply + (map #(Math/cos (* c %)) x)))))
       a
       (Math/exp 1))))

(defn weierstrass-function
  "see definition:
  https://bee22.com/resources/Liang%20CEC2014.pdf
  In above site, recommended variable values are `a=0.5, b=3, kmax=20`"
  [a b k-max x]
  (let [dimension (count x)
        k (take (+ 1 k-max) (iterate inc 0))
        first-item-function (fn [x_i] (apply + (map
                                        #(* (Math/pow a %)
                                            (Math/cos (* 2 Math/PI (Math/pow b %) (+ x_i 0.5)))) k)))]
    (- (apply + (map first-item-function x))
       (* dimension (apply +
                           (map #(* (Math/pow a %)
                                    (Math/cos (* 2 Math/PI (Math/pow b %) 0.5))) k))))))

(defn happy-cat-function
  "see definition:
  http://benchmarkfcns.xyz/benchmarkfcns/happycatfcn.html"
  [alpha x]
  (let [dimension (count x)]
    (+ (Math/pow (Math/pow (- (apply + (map #(Math/pow % 2) x)) dimension) 2) alpha)
       (/ (* 0.5 (apply + (map #(Math/pow % 2) x)) (apply + x)) dimension)
       0.5)))

(defn shift
  "shift x by o."
  [o x]
  {:pre [(s/valid? #(= (count o) (count %)) x)]}
  (map #(- %1 %2) x o))

(defn- rotate-matrix-2d
  "return value is [col, row] matrix."
  [m]
  [[(Math/cos (* Math/PI m)) (Math/sin (* Math/PI m))]
   [(* -1 (Math/sin (* Math/PI m))) (Math/cos (* Math/PI m))]])

(defn- multiply
  [x_row_vector y_col_vector]
  {:pre [(s/valid? (s/coll-of number?) x_row_vector)
         (s/valid? (s/coll-of number?) y_col_vector)
         (s/valid? #(= (count x_row_vector) (count %)) y_col_vector)]
   :post [(s/valid? number? %)]}
  (->> (map #(* %1 %2) x_row_vector y_col_vector)
       (apply +)))

(defn rotate
  "Limitation: Now, this function takes only 2 dimension x."
  [m x]
  {:pre [(s/valid? (s/coll-of number?) x)
         (s/valid? #(= 2 (count %)) x)]}
  (let [rotate-matrix (rotate-matrix-2d m)]
    (map (partial multiply x) rotate-matrix)))
