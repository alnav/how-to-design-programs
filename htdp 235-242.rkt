;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |htdp 235-242|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Exercise 235.
;Use the contains? function to define functions that search for "atom", "basic", and "zoo", respectively.

; String List-of-strings -> Boolean
; check if list of strings contains string
(check-expect (contains? "dog" (list "dog" "cat")) #true)
(check-expect (contains? "bob" (list "bill" "john")) #false)

(define (contains? s los)
  (cond
    [(empty? los) #false]
    [else (or (string=? s (first los))
              (contains? s (rest los)))]))

;; Exercise 236. Create test suites for the following two functions:
; Lon -> Lon
; adds 1 to each item on l
(check-expect (add1* (list 3 2 9)) (list 4 3 10))
(check-expect (add1* (list 2 1)) (list 3 2))

#;
(define (add1* l)
  (cond
    [(empty? l) '()]
    [else
     (cons
       (add1 (first l))
       (add1* (rest l)))]))
     
; Lon -> Lon
; adds 5 to each item on l
(check-expect (plus5 (list 3 2 9)) (list 8 7 14))
(check-expect (plus5 (list 2 1)) (list 7 6))

#;
(define (plus5 l)
  (cond
    [(empty? l) '()]
    [else
     (cons
       (+ (first l) 5)
       (plus5 (rest l)))]))

; Then abstract over them.
;Define the above two functions in terms of the abstraction as one-liners and use the existing test
;suites to confirm that the revised definitions work properly.
;Finally, design a function that subtracts 2 from each number on a given list.

(define (add-list n l)
  (cond
    [(empty? l) '()]
    [else
     (cons
      (+ (first l) n)
      (add-list n (rest l)))]))

(define (add1* l) (add-list 1 l))
(define (plus5 l) (add-list 5 l))

; list -> list
; subtracts 2 from members of the list
(check-expect (sub2 (list 3 9 8)) (list 1 7 6))
(define (sub2 l) (add-list -2 l))

;; Exercise 237.
;Evaluate (squared>? 3 10) and (squared>? 4 10) in DrRacket. How about (squared>? 5 10)?

; Number Number -> Boolean
; is the area of a square with side x larger than c
(define (squared>? x c)
  (> (* x x) c))

(squared>? 3 10)
(squared>? 4 10)
(squared>? 5 10)

;; Exercise 238.
;Abstract the two functions in figure 89 into a single function.
;Both consume non-empty lists of numbers (Nelon) and produce a single number.
;The left one produces the smallest number in the list, and the right one the largest.

; Nelon -> Number
; determines the smallest 
; number on l
(define (inf l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (< (first l)
            (inf (rest l)))
         (first l)
         (inf (rest l)))]))
    

; Nelon -> Number
; determines the largest 
; number on l
(define (sup l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (> (first l)
            (sup (rest l)))
         (first l)
         (sup (rest l)))]))

; Function Nelon -> Number
(define (filter-list operator l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (operator (first l)
                   (filter-list operator (rest l)))
         (first l)
         (filter-list operator (rest l)))]))

;Define inf-1 and sup-1 in terms of the abstract function. Test them with these two lists:

(define l1 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
      12 11 10 9 8 7 6 5 4 3 2 1))
 
(define l2 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
      17 18 19 20 21 22 23 24 25))

(define (inf-1 l) (filter-list < l))
(define (sup-1 l) (filter-list > l))  

;Why are these functions slow on some of the long lists?
;Modify the original functions with the use of max, which picks the larger of two numbers,
;and min, which picks the smaller one.
;Then abstract again, define inf-2 and sup-2, and test them with the same inputs again.
;Why are these versions so much faster?

; Nelon -> Number
(define (inf-min l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (= (min (first l) (second l)) (first l))
         (first l)
         (inf-min (rest l)))]))

; Nelon -> Number
(define (sup-max l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (= (max (first l) (second l)) (first l))
         (first l)
         (sup-max (rest l)))]))

(define (filter-list2 operator l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (= (operator (first l) (second l)) (first l))
     (first l)
     (filter-list2 operator (rest l)))]))

(define (inf-2 l) (filter-list2 min l))
(define (sup-2 l) (filter-list2 max l))

;; Exercise 239.
;A list of two items is another frequently used form of data in ISL
;programming. Here is a data definition with two parameters:

; A [List X Y] is a structure: 
;   (cons X (cons Y '()))
 
;Instantiate this definition to describe the following classes of data:
;pairs of Numbers
; A [List num1 num2] is a structure:
;(cons num1 (cons num2 '()))

(define l1 (cons 3 (cons 5 '())))

;pairs of Numbers and 1Strings

; A [List num str] is a structure:
; (cons num (cons str '()))

(define l2 (cons 3 (cons "text" '())))

;pairs of Strings and Booleans.
; A [List str bool] is a structure:
; (cons str (cons bool '()))

(define l1 (cons "text" (cons #true '())))

;Also make one concrete example for each of these three data definitions.

;; Exercise 240.
;Here are two strange but similar data definitions:

; An LStr is one of: 
; – String
; – (make-layer LStr)

; An LNum is one of: 
; – Number
; – (make-layer LNum)

;Both data definitions rely on this structure-type definition:

(define-struct layer [stuff])

;Both define nested forms of data: one is about numbers and the other about strings.
;Make examples for both. Abstract over the two.
;Then instantiate the abstract definition to get back the originals.

(define L1 (make-layer (make-layer "string")))

(define L2 (make-layer (make-layer (make-layer 5))))

;;Exercise 241.
;Compare the definitions for NEList-of-temperatures and NEList-of-Booleans.
;Then formulate an abstract data definition NEList-of.

; An NEList-of-temperatures is one of: 
; – (cons CTemperature '())
; – (cons CTemperature NEList-of-temperatures)

; An NEList-of-booleans is one of: 
; – (cons CBoolean '())
; – (cons CBoolean NEList-of-booleans)

; Abstraction

; a NEList-of-X is one of
; - (cons X '())
; - (cons X NEList-of-X)

;; Exercise 242.

;Here is one more parametric data definition:

; A [Maybe X] is one of: 
; – #false 
; – X

;Interpret these data definitions:
;[Maybe String]: either false or a string
;[Maybe [List-of String]]: either false or a List-of-strings
;and [List-of [Maybe String]]; list of either falses, or strings

;What does the following function signature mean: 
;takes a string, and a list of strings, returns a list of string, or false

; String [List-of String] -> [Maybe [List-of String]]
; returns the remainder of los starting with s 
; #false otherwise 

(check-expect (occurs "a" (list "b" "a" "d" "e"))
              (list "d" "e"))
(check-expect (occurs "a" (list "b" "c" "d"))
              #f)

(define (occurs s los)
  (cond
    [(empty? los) #false]
    [else
     (if (= s (first los)) (rest los) (occurs s (rest los)))]))

;Work through the remaining steps of the design recipe.

               