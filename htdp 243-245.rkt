;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |htdp 243-245|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Exercise 243.
;Assume the definitions area in DrRacket contains
(define (f x) x)

;Identify the values among the following expressions:
(cons f '())
(list f)

(f f)
; f

(cons f (cons 10 (cons (f 10) '())))
(list f 10 10)

;Explain why they are (not) values.

;; Exercise 244.
;Argue why the following sentences are now legal:
(define (f1 x) (x 10))

(define (f2 x) (x f))

(define (f3 x y) (x 'a y 'b))

;Explain your reasoning.
;Because functions can be manipulated as data

;; Exercise 245.
;Develop the function=at-1.2-3-and-5.775? function.
;Given two functions from numbers to numbers, the function determines whether the two produce
;the same results for 1.2, 3, and -5.775.

;Mathematicians say that two functions are equal if they compute the same result when given
;the same input—for all possible inputs.

;Can we hope to define function=?, which determines whether two functions from numbers to numbers are equal?
;If so, define the function. If not, explain why and consider the implication
;that you have encountered the first easily definable idea for which you cannot define a function.

(define (function=at-1.2-3-and-5.775? f1 f2)
  (and (= (f1 1.2) (f2 1.2))
       (= (f1 3) (f2 3))
       (= (f1 5.775) (f2 5.775))))

; cannot define function=? because cannot test for every number

