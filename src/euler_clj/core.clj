(ns euler-clj.core
  (:use [clojure.data.priority-map :only [priority-map]])
  (:gen-class))

(defn multiple?
  "true if number is a multiple of base"
  [number base]
  (zero? (mod number base)))

(defn multiple-any?
  "true if number is a multiple of any bases"
  [number & bases]
  (some (partial multiple? number) bases))

(defn problem-001
  "Sum the multiples of 3 or 5 smaller than 1000"
  []
  (reduce + (filter #(multiple-any? % 3 5) (range 1000))))

(defn fibonacci-seq
  "Lazy sequence of Fibonacci numbers"
  ([] (fibonacci-seq 0 1))
  ([a b] (cons a (lazy-seq (fibonacci-seq b (+' b a))))))

(defn problem-002
  "Sum the even terms in the Fibonacci sequence lower than 4 million"
  []
  (reduce + (filter even? (take-while (partial > 4e6) (fibonacci-seq)))))

(defn prime-sieve
  "Lazy filter of non-primes using the Sieve of Eratosthenes"
  ([candidates]
     (prime-sieve candidates (priority-map)))
  ([candidates multiples]
     (let [candidate (first candidates)]
       (letfn [(next-multiple [factor]
                 (if (contains? multiples factor)
                   (+ factor (multiples factor))
                   (* factor factor)))
               (updated-multiples [factor]
                 (assoc multiples factor (next-multiple factor)))
               (found-prime []
                 (cons candidate
                       (lazy-seq (prime-sieve (rest candidates)
                                              ( updated-multiples candidate)))))
               (found-composite [factor]
                 (lazy-seq (prime-sieve (rest candidates)
                                        (updated-multiples factor))))
               (catch-up-to-candidate [factor]
                 (lazy-seq (prime-sieve candidates
                                        (updated-multiples factor))))]
         (if (empty? multiples)
           (found-prime)
           (let [[factor smallest-multiple] (first multiples)]
             (if (< candidate smallest-multiple)
               (found-prime)
               (if (= candidate smallest-multiple)
                 (found-composite factor)
                 (catch-up-to-candidate factor)))))))))

(defn primes
  "Lazy sequence of prime numbers generated with Sieve of Eratosthenes"
  []
  (letfn [(prime-candidates [] ;; 2, 3, ... all odd numbers
            (cons 2 (iterate (partial + 2) 3)))])
  (prime-sieve (prime-candidates)))

(defn prime-factors
  "Lazy sequence of prime factors of a number"
  [n]
  (filter (partial multiple? n)
          (take-while (partial >= (inc (int (Math/sqrt n)))) (primes))))

(defn problem-003
  "Largest prime factor of 600851475143"
  []
  (apply max (cons 1 (prime-factors 600851475143))))

(defn solve-euler
  "Format the answer to Problem N as a string"
  [n]
  (try
    (load-string (format "(problem-%03d)" n))
    (catch Exception e
      (.getMessage e))))

(defn -main
  "Answers to some of the problems of Project Euler"
  [& args]
  (let [problem-set (apply range args)]
    (zipmap problem-set (map solve-euler problem-set))))
