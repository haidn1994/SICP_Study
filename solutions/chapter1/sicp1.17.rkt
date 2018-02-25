#lang planet neil/sicp

(define (double n)
  (* n 2))

; n을 2번 더하면 2*n이 된다.
(define (double-scratch n)
  (+ n n))

(define (halve n)
  (/ n 2))

; 나눗셈이 정의되어 있지 않다고 했을 때, 원시적인 방법으로 정의한 halve
; count가 2로 나누었을 때 몫으로 사용된다.
(define (halve-scratch n)
  (define (iter remain count)
    (if (< remain 2)
        count
        (iter (- remain 2) (+ count 1))))
    (iter n 0))        

(define (even? n)
  (= (remainder n 2) 0))

(define (square n)
  (* n n))

; 거듭 제곱을 로그로 비례하게 만든 프로시저
; 이 코드를 조금만 바꾸면 문제에서 원하는 바를 얻어 낼 수 있다.
; 바꿔야 할 곳은 다음과 같다. 1. 이름 2. 항등원(base case) 3. square -> double(짝수일 때 반복 조건)  4. + -> *(반복 조건)
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (fast-mult b n)
  (cond ((= n 0) 0)
        ((even? n) (double (fast-mult b (halve n))))
        (else (+ b (fast-mult b (- n 1))))))

; 곱셈과 나눗셈이 정의되어 있지 않다고 하고, 앞서 만든 프로시저들로 만든 솔루션이다.
(define (fast-mult-scratch b n)
  (cond ((= n 0) 0)
        ((even? n) (double-scratch (fast-mult b (halve-scratch n))))
        (else (+ b (fast-mult b (- n 1))))))

(* 3 4)
(* 5 6)
; => 12
; => 30

(fast-mult 3 4)
(fast-mult 5 6)
; => 12
; => 30

(fast-mult-scratch 3 4)
(fast-mult-scratch 5 6)
; => 12
; => 30
