#lang planet neil/sicp

; 보조해주는 함수들
(define (inc n) (+ n 1))

(define (square n) (* n n))

; 되도는 프로세스
(define (sum term a next b)
  (if (> a b)
      0 ; 탈출 조건을 덧셈의 항등원으로 설정하자.
      (+ (term a)
         (sum term (next a) next b)))) ; 스택을 쌓고 풀어나가면서 덧셈을 계산해 나간다.

; 반복하는 프로세스
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        ; result는 값을 계속 쌓아놓는 일종의 누산기(acculmulator)로써 동작한다.
        result
        (iter (next a) (+ (term a) result))))
  ; 따라서 처음 실행기를 실행할 때 덧셈의 항등원인 0을 인자로 줘야 한다.
  (iter a 0))

(sum square 1 inc 15)
(sum-iter square 1 inc 15)
