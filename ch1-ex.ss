experiments

inc (+ 3 5))
  (inc (inc (+ 2 5)))
  (inc (inc (inc (+ 1 5))))
  (inc (inc (inc (inc (+ 0 5)))))
  (inc (inc (inc (inc 5))))
  (inc (inc (inc 6)))
  (inc (inc 7))
  (inc 8)
  9
2 (+ 4 5)
  (+ 3 6)
  (+ 2 7)
  (+ 1 8)
  (+ 0 9)
  9

2 iterative 1 recursive

E1.10
(A 1 10)
(* 2 (A (1 9))
(* 2 (* 2 A (1 8))))
(* 2 (* 2 (* 2 A ( 1 7))))
(* 2 (* 2 (* 2 (* 2 A (1 6)))))
(* 2 (* 2 (* 2 (* 2 (* 2 A (1 5))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 A (1 4)))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 A (1 3)))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (*2 A (1 2))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (*2  (* 2 (*2 (*2 A (1 1))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (*2  (*2  (*2  (*2 2)))))
2^10

(A 2 4)
(A 1 A (2 3))
2^(A 1 A (2 2))
2^(2^A(1 A (2 1)))
2^(2^(2^2))


(A 3 3)
(A 2 A (2 2))
(A 2 2^2)
2^2^2^2

(define (f n) (A 0 n)) f => 2*n
(define (g n) (A 1 n)) g => 2^n
(define (h n) (A 2 n)) h => 2^2…^2 (n 2)

E1.11
iterative
(define (f n)
 (f-iter 2 1 0 n))
(define (f-iter a b c counter)
    (cond ((= counter 2) a)
          ((= counter 1) b)
          ((= counter 0) c)
          (else (f-iter (+ a (+ (* 2 b) (* 3 c)))
                        a
                        b
                        (- counter 1)))))

recursive
(define (f n)
    (if ( < n 3)
        n
        ( + (f ( - n 1))
            ( + (* 2 (f (- n 2)))
                (* 3 (f (- n 3)))))))

E1.12
recursive 
(define (pascal level idx)
    ( if (or ( = idx 1)  
             ( = idx level))
         1
         ( + (pascal (- level 1)
                     (- idx 1))
             (pascal (- level 1)
                     idx))))

E1.13
带入求左右两边相等就可以了。

E1.14
(count-change 11)
(cc 11 5)
( + (cc 11 4) (cc -39 5))
( + (+ (cc 11 3) (cc -14 4)) 0)
( + (+ (+ (cc 11 2) (cc 1 3)) 0) 0)
( + (+ (+ (+ (cc 11 1) (cc 6 2)) (+ (cc 1 2) (cc -9 3))) 0) 0)
( + (+ (+ (+ (+ (cc 11 0) (cc 10 1)) (+ (cc 6 1) (cc 1 2)) (+ (+ (cc 1 1) (cc -4 2)) 0)) 0) 0)

….
order of growth of space 2^n
order of growth of steps n

E1.15
(sine 12.15)
(p (sine 4.05))
(p (p (sine 1.35)))
(p (p (p (sine 0.45))))
(p (p (p (p (sine 0.15)))))
(p (p (p (p (p (sine 0.03))))))
(p (p (p (p (p (p (sine 0.01)))))))

p is applied 6 times.
order of growth of space log3(n)
order of growth of space log3(n)

E1.16
(define (exp b n)
   (exp-iter 1 b n))

(define (even? n)
    (= (remainder n 2) 0))

(define (exp-iter a b counter)
    (cond ((= counter 0) a)
          ((even? counter) (exp-iter a 
                                     (square b)
                                     (/ counter 2)))
          (else (* b (exp-iter a 
                               (square b)
                               (/ (- counter 1) 2))))))

E1.17
(define (double x)
    (* 2 x))
(define (halve x)
    (/ x 2.0))
(define (fast-multi a b)
    (cond(( = b 1) a)
         ((even? b) (fast-multi (double a)
                                (halve b)))
         (else (+ a (fast-multi (double a)
                                (halve (- b 1)))))))

E1.18 same above

E1.19
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))      ; compute p'
                   (+ (* 2 (* p q)) (square q))     ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

E1.20
normal-order (fully expand and then reduce)
(gcd 206 40)
(gcd (gcd 40 (remainder 206 40)/*1*/)
(gcd (gcd (gcd 6/*1*/ (remainder 40 6/*1*/))))
(gcd (gcd (gcd (gcd (remainder 206 40) (remainder 40)….

7 times
applicative-order 
4 times

E1.21
(smallest-divisor 19)
19
 (smallest-divisor 199)
199
(smallest-divisor 1999)
1999
 (smallest-divisor 19999)
7

E1.22

(define (search-for-primes a b)
    (search-for-primes-iter (+ a 1) b))
(define (search-for-primes-iter counter b)
    (if (< counter b)
        ((timed-prime-test counter)
         (search-for-primes-iter (+ counter 1) b))))
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (prime? n)
  (= n (smallest-divisor n)))

=======================================


 (search-for-primes 1000 1020)
1009 *** 3
1013 *** 3
1019 *** 3

(search-for-primes 10000 10200)

10007 *** 9
10008
10009 *** 9
10037 *** 9

(search-for-primes 100000 100200)
100003 *** 26
100019 *** 29

100043 *** 26
(search-for-primes 100000 100200)
1000003 *** 87
1000033 *** 84

1000037 *** 84

YES
E1.23

(define (next-iter counter)
    (if (= 0 (remainder counter 2))
        (next-iter (+ counter 1))
        counter))
(define (next test-divisor)
    (next-iter (+ test-divisor 1)))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(timed-prime-test 1009)

1009 *** 217
1013 *** 8
1019 *** 8
10007 *** 13
10009 *** 13
10037 *** 14
100003 *** 32
100019 *** 31
100043 *** 35
1000003 *** 88
1000033 *** 91
1000037 *** 87

Ratio is not 2, the larger the n is, the ratio is closer to 1.
I guess something is wrong with my next function. 

E1.24 fast-prime?
1009 *** 18
1013 *** 19
1019 *** 20
10007 *** 24
10009 *** 22
10037 *** 24
100003 *** 12750
100019 *** 38
100043 *** 28
1000003 *** 31
1000033 *** 35
1000037 *** 38

constant factor

E1.25
YES

E1.26
(* n n) use normal order, expand all then reduce, calculate exp more times
(square n) use applicative order

E1.27
(define (fermat-test n)
  (try-it-iter 2 n))

(define (try-it-iter a n)
   (if (< a n)
       (if (= (expmod a n n) a)
           (try-it-iter (+ a 1) n)
           false)
        true))

E1.28
(define (miller-rabin-test n)
  (define (try-it a)
     (= (expmod a (- n 1) n) 1))
  (try-it (+ 2 (random (- n 2)))))

(define (find-nontrivial? x m)
  (if (and (and (not (= x 1)) 
                (not (= x (- m 1))))
           (= (remainder (square x) m) 1))
      0
      (remainder (square x) m)))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (find-nontrivial? (expmod base (/ exp 2) m) m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))
E1.29
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (inc x) (+ x 1))
  (define (y k)
    (f (+ a (* k h))))
  (define (term k)
    (* (cond ((odd? k) 4)
             ((or (= k 0) (= k n)) 1)
             ((even? k) 2))
       (y k)))
  (/ (* h (sum term 0 inc n)) 3))
(define (cube x)
    (* x x x))

(simpson cube 0 1 1000.0)
0.2500000000000003
(simpson cube 0 1 100)
1/4

E1.30
;; old sum definition
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;; new sum definition
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a)
                          result))))
  (iter a 0))

;; simple test cases
(sum cube 0 next 10)
3025
(sum cube 0 next 5)
225

E1.31
;; recursive product for factorial method
(define (factorial n)
    (define (term x) x)
    (define (next x) (+ x 1))
    (define (product term a next b)
       (if (> a b)
           1
           (* (term a)
           (product term (next a) next b))))
    (product term 1 next n)) 

;; test cases
(factorial 2)
2
(factorial 3)
6
(factorial 4)
24 

;; confirm the pi value.
(define (approx-pi n)
    (define (square x) (* x x))
    (define (term x) (/ (* (* 2 x) (* 2 (+ x 1)))
                        (square (+ (* 2 x) 1))))
    (define (next x) (+ x 1))
 
    (* 4 (product term 1 next n)))
(approx-pi 100)
#e3.1493784731686008593667128...

;; iterative product method
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a)
                          result))))
  (iter a 1))

E1.32
;; recursive
(define (accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a)
                  (accumulate combiner null-value term (next a) next b))))
;; iterative 
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a)
                                 result))))
  (iter a null-value))

;; sum and product definition.

(define (sum term a next b)
    (accumulate + 0 term a next b))
(define (product term a next b)
    (accumulate * 1 term a next b))

E1.33
(define (filtered-accumulate combiner null-value filter term a next b)
    (if (> a b)
        null-value
        (combiner (if (filter a)
                      (term a)
                      null-value)
                  (accumulate combiner null-value term (next a) next b))))

;; the sum of the squares of the prime numbers in the interval a to b
(define (sum-square-prime a b)
    (define (term x) (square x))
    (define (next x) (+ x 1))
    (filtered-accumulate + 0 prime? term a next b))
;;the product of all the positive integers less than n that are relatively prime to n 
(define (product-prime-to n)
    (define (term x) x)
    (define (next x) (+ x 1))
    (define (filter x) (= (gcd x n) 1))
    (filtered-accumlate * 1 term 1 next n))

E1.34
(f f)
(f 2)
(2 2)

E1.35
 (define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0)
1.6180327868852458

E1.36
 (define tolerance 0.00001)
(define (fixed-point-process f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
           guess
          (try next))))
  ((try first-guess)
   (newline)
   (display "***")))

(fixed-point-process (lambda(x) (/ (log 1000) (log x)))
                             10)

10
2.9999999999999996
6.2877098228681545
3.7570797902002955
5.218748919675316
4.1807977460633134
4.828902657081293
4.386936895811029
4.671722808746095
4.481109436117821
4.605567315585735
4.522955348093164
4.577201597629606
4.541325786357399
4.564940905198754
4.549347961475409
4.5596228442307565
4.552843114094703
4.55731263660315
4.554364381825887
4.556308401465587
4.555026226620339
4.55587174038325
4.555314115211184
4.555681847896976
4.555439330395129
4.555599264136406
4.555493789937456
4.555563347820309
4.555517475527901
4.555547727376273
4.555527776815261
4.555540933824255
***

E1.37
;; recursive 
(define (cont-frac n d k) 
    (if (= k 0)
        0
        (/ (n k) 
           (+ (d k) (cont-frac n d (- k 1))))))
> (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             1)
1.0
> (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             2)
0.5
> (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             3)
0.6666666666666666

;;iterative
(define (cont-frac n d k) 
    (define (iter-cont-fact result k)
        (if (= k 0)
            result
            (iter-cont-fact (/ (n k)
                               (+ (d k) result)) (- k 1))))
    (iter-cont-fact 0 k))

;; test case
(cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             3)
0.6666666666666666

E1.38
(cont-frac (lambda (i) 1.0)
             (lambda (i)
               (cond ((or (= i 1) (= i 2)) i)
                     ((or (= (remainder i 3) 0) (= (remainder i 3) 1)) 1.0)
                     (else (* (/ (+ i 1) 3.0) 2))))
             3)
0.75
(cont-frac (lambda (i) 1.0)
             (lambda (i)
               (cond ((or (= i 1) (= i 2)) i)
                     ((or (= (remainder i 3) 0) (= (remainder i 3) 1)) 1.0)
                     (else (* (/ (+ i 1) 3.0) 2))))
             10)
0.7182817182817183

E1.39

> (define (tan-cf x k)
    (cont-frac (lambda (i) (cond ((= i 1) x)
                                 (else (square x))))
               (lambda (i) (- (* i 2) 1))
               k))
> (define (cont-frac n d k) 
    (define (iter-cont-fact result k)
        (if (= k 0)
            result
            (iter-cont-fact (/ (n k)
                               (- (d k) result)) (- k 1))))
    (iter-cont-fact 0 k))

E1.40

(define (deriv g)
  (define dx 0.00001)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
    (define (cube x)
      (* x x x))
    (define (square x)
      (* x x))
    (lambda (x) (+ (cube x)
                   (* a (square x))
                   (* b x)
                   c)))

(newtons-method (cubic 1 1 1) 1)
-0.9999999999997796

E1.41

return 13

E1.42
(define (compose f g)
    (lambda (x)
      (f (g x))))
> ((compose square inc) 6) 
49

E1.43
(define (repeated f n)
     (if (= n 1)
          f
          (compose f (repeated f (- n 1)))))

((repeated square 2) 5)
625

E1.44

(define (smooth f)
    (define dx 0.00001)
    (define (average x y z)
        (/ (+ x y z) 3.0))
    (lambda (x)
            (average (f x)
                     (f (+ x dx))
                     (f (- x dx)))))

((smooth square) 5)
25.000000000066663

E1.45

(define (exp b n)
  (cond ((= n 0) 1)
        ((even? n) (square (exp b (/ n 2))))
        (else (* b (exp b (- n 1))))))

(define (nth-root x n)
  (fixed-point ((repeated average-damp (- n 2)) (lambda (y) (/ x (exp y (- n 1)))))
               1.0))

;; test cases

(nth-root 8 3)
1.9999981824788517
>  (nth-root 10000 4)
10.0

E1.46
 (define (iterative-improve good-enough improve)
  (lambda (guess)
    (if (good-enough guess)
        guess
        ((iterative-improve good-enough improve) 
         (improve guess)))))

(define (sqrt-iter x)
    ((iterative-improve (lambda (guess) 
                         (< (abs (- (square guess) x)) 0.0001))
                       (lambda (guess)
                         (average guess (/ x guess))))
     1.0))
> (sqrt-iter 9)
3.000000001396984

(define (fixed-point f)
    ((iterative-improve (lambda (guess)
                         (< (abs (- (f guess) guess)) 0.00001))
                        (lambda (guess)
                          (f guess)))
     1.0))
> (fixed-point square)
1.0

;; the end of Chapter 1 excersie.
