;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname save) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Exercise 181.
;Use list to construct the equivalent of these lists:
(cons "a" (cons "b" (cons "c" (cons "d" '()))))
(list "a" "b" "c" "d")

(cons (cons 1 (cons 2 '())) '())
(list (list 1 2))

(cons "a" (cons (cons 1 '()) (cons #false '())))
(list "a" (list 1) #false)
      
(cons (cons "a" (cons 2 '())) (cons "hello" '()))
(list (list "a" 2) "hello")

;Also try your hand at this one:
(cons (cons 1 (cons 2 '()))
      (cons (cons 2 '())
            '()))
(list (list 1 2) (list 2))

;; Exercise 182.
;Use cons and '() to form the equivalent of these lists:
(list 0 1 2 3 4 5)
(cons 0 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '()))))))

(list (list "he" 0) (list "it" 1) (list "lui" 14))
(cons (cons "he" (cons 0 '()))
      (cons (cons "it" (cons 1 '()))
            (cons (cons "lui" (cons 14 '())) '())))
      
(list 1 (list 1 2) (list 1 2 3))
(cons 1 (cons (cons 1 (cons 2 '()))
              (cons (cons 1 (cons 2 (cons 3 '()))) '())))

;; Exercise 183.
;On some occasions lists are formed with cons and list.
(cons "a" (list 0 #false))
(list "a" 0 #false)

(list (cons 1 (cons 13 '())))
(list (list 1 13))

(cons (list 1 (list 13 '())) '())
(list (list 1 (list 13 '())))

(list '() '() (cons 1 '()))
(list '() '() (list 1))

(cons "a" (cons (list 1) (list #false '())))
(list "a" (list 1) #false '())

;Reformulate each of the following expressions using only cons or only list.

;; Exercise 184.
;Determine the values of the following expressions:
(list (string=? "a" "b") #false)
(list #false #false)

(list (+ 10 20) (* 10 20) (/ 10 20))
(list 30 200 0.5)

(list "dana" "jane" "mary" "laura")

;; Exercise 185.
;You know about first and rest from BSL, but BSL+ comes with even more selectors than that.
;Determine the values of the following expressions:
(first (list 1 2 3))
1

(rest (list 1 2 3))
(list 2 3)

(second (list 1 2 3))
2

;Find out from the documentation whether third and fourth exist.
;Yes they exist

;; Exercise 186.
;Take a second look at Intermezzo 1: Beginning Student Language,
;the intermezzo that presents BSL and its ways of formulating tests.
;One of the latter is check-satisfied, which determines whether an expression satisfies a certain property.
;Use sorted>? from exercise 145 to reformulate the tests for sort> with check-satisfied.
; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort> l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort> (rest l)))]))
 
; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers l 
(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (>= n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))

(define (sorted>? list)
  (cond
    [(empty? (rest list)) #true]
    [(and (> (first list) (first (rest list))) 
          (sorted>? (rest list))) #true]
    [else #false]))

(check-satisfied (sort> (list 3 8 12 2 1 85)) sorted>?)

;; Exercise 187.
;Design a program that sorts lists of game players by score:

(define-struct gp [name score])
; A GamePlayer is a structure: 
;    (make-gp String Number)
; interpretation (make-gp p s) represents player p who 
; scored a maximum of s points 
;Hint Formulate a function that compares two elements of GamePlayer.

; list-of-players -> list-of-players
; sort a list of players, by player score
(define (sort-player p)
  (cond
    [(empty? p) '()]
    [(cons? p) (insert-player (first p) (sort-player (rest p)))]))


