;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |htdp 231-234|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Exercise 231.
;Eliminate quote in favor of list from these expressions:
'(1 "a" 2 #false 3 "c")
(list 1 "a" 2 #false 3 "c")

'()
'()

;and this table-like shape:
'(("alan" 1000)
  ("barb" 2000)
  ("carl" 1500))

(list (list "alan" 1000)
      (list "barb" 2000)
      (list "carl" 1500))

;Now eliminate list in favor of cons where needed.

;; Exercise 232.

;Eliminate quasiquote and unquote from the following expressions so that they are written with list instead:
`(1 "a" 2 #false 3 "c")
(list 1 "a" 2 #false 3 "c")

;this table-like shape:
`(("alan" ,(* 2 500))
  ("barb" 2000)
  (,(string-append "carl" " , the great") 1500)
  ("dawn" 2300))

(list (list "alan" 1000)
      (list "barb" 2000)
      (list "carl, the great" 1500)
      (list "dawn" 2300))

(define title "ratings")
;and this second web page:
`(html
   (head
     (title ,title))
   (body
     (h1 ,title)
     (p "A second web page")))
;where

;Also write down the nested lists that the expressions produce.
;<html>
;<head>
;<title> "ratings" </title>
;</head>
;<body>
;<h1> "ratings" </h1>
;<p> "A second web page" </p>
;</body>
;</html>

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;; Exercise 233.
;Develop alternatives to the following expressions that use only list and produce the same values:

`(0 ,@'(1 2 3) 4)
(list 0 1 2 3 4)

;this table-like shape:
`(("alan" ,(* 2 500))
  ("barb" 2000)
  (,@'("carl" " , the great")   1500)
  ("dawn" 2300))

(list (list "alan" 1000)
      (list "barb" 2000)
      (list "carl" " , the great" 1500)
      (list "dawn" 2300))

;and this third web page:
; List-of-numbers -> ... nested list ...
; creates a row for an HTML table from l
(define (make-row l)
  (cond
    [(empty? l) '()]
    [else (cons (make-cell (first l))
                (make-row (rest l)))]))
 
; Number -> ... nested list ...
; creates a cell for an HTML table from a number 
(define (make-cell n)
  `(td ,(number->string n)))

`(html
   (body
     (table ((border "1"))
       (tr ((width "200"))
         ,@(make-row '( 1  2)))
       (tr ((width "200"))
         ,@(make-row '(99 65))))))

(list
 'html
 (list
  'body
  (list
   'table
   (list (list 'border "1"))
   (list 'tr (list (list 'width "200"))
         (list 'td "1")
         (list 'td "2"))
   (list 'tr (list (list 'width "200"))
         (list 'td "99")
         (list 'td "65")))))

;; Exercise 234.
; !!! TODO: incorrect row, no number

;Create the function make-ranking, which consumes a list of ranked song titles and produces a list representation of an HTML table. Consider this example:
(define one-list
  '("Asia: Heat of the Moment"
    "U2: One"
    "The White Stripes: Seven Nation Army"))

; list-of-strings -> list-of-list-of-strings
(define (ranking los)
  (reverse (add-ranks (reverse los))))

; list-of-strings -> list-of-list-of-strings
(define (add-ranks los)
  (cond
    [(empty? los) '()]
    [else (cons (list (length los) (first los))
                (add-ranks (rest los)))]))

; list-of-strings -> HTML
(define (make-ranking los)
  (cond
    [(empty? los) '()]
    [else
     (cons `(tr ,@(first (ranking los)))
           (make-ranking (rest los)))]))




