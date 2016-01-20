#lang racket

(require "../common/uw-api.rkt")

;; INTEGRITY STATEMENT (modify if neccessary)
;; I received help from the following sources:
;; None. I am the sole author of this work 

;; sign this statement by removing the line below and entering your name
;; Name: Yihan Wang
;; login ID: y2349wan

;;;;;;;;;;;;;;;;
;; INTERFACE: ;;
;;;;;;;;;;;;;;;;

;; A modulo for providing food menu of a particular meal in a particular day 

(provide food-menu)

;; (food-menu year week day meal) produces a list of food names of all outlets of a particular meal in a particular day from yaer week day meal
;; food-menuL Int Int Str Str -> (listof Str)
;; requires: day has to be one of "Monday" "Tuesday" "Wednesday" "Thursday" "Friday"
;;           meal has to be one of "lunch" "dinner"
;;           year and week must in the range of food service running days (e.g. 2015 1st week is an invalid enter, as there is no food service in that week)

;; EXAMPLES:

; (food-menu 2016 1 "Monday" "lunch")
; (food-menu 2015 2 "Tuesday" "dinner")
; (food-menu 2016 5 "Wednesday" "lunch")

;;;;;;;;;;;;;;;;;;;;;
;; IMPLEMENTATION: ;;
;;;;;;;;;;;;;;;;;;;;;

(define (food-menu year week day meal)
  (define food-lst (uw-api (string-append "/foodservices/" (number->string year) "/" (number->string week) "/menu")))
  (define outlet-lst1 (first (filter (lambda (x) (string=? "outlets" (first x))) food-lst)))
  (define outlet-lst2 (first (filter (lambda (x) (not (string? x))) outlet-lst1)))
  (cond [(empty? outlet-lst1) (error "the year and week entered are no in the range of food service opengin day")]
[else
  (define menu-lst (filter (lambda (x) (string=? "menu" (first x))) (foldr append empty outlet-lst2)))
  (define date-lst (filter (lambda (x) (not (string? x))) (foldr append empty menu-lst)))
  (define (day? lst day)
    (cond [(empty? lst) #f]
          [(and (string=? "day" (first (first lst))) (string=? day (second (first lst)))) #t]
          [else (day? (rest lst) day)]))
  (define day-lst (filter (lambda (x) (day? x day)) (foldr append empty date-lst)))
  (define meal-lst1 (filter (lambda (x) (string=? "meals" (first x)))(foldr append empty day-lst)))
  (define meal-lst2 (filter (lambda (x) (not (string? x))) (foldr append empty meal-lst1)))
  (define meal-lst (foldr append empty meal-lst2))
  (define product-lst (first (filter (lambda (x) (string=? meal (first x))) meal-lst)))
  (define product-lst2 (first (filter (lambda (x) (not (string? x))) product-lst)))
  (define product-lst3 (foldr append empty product-lst2))
  (define name-lst (filter (lambda (x) (string=? "product_name" (first x))) product-lst3))
  (map second name-lst)]))
         
         
         
         

  










