#lang racket
;; SweetByte DSL
;; A simple DSL for managing bakery items

;; Structure for a product
(struct item (name price stock) #:transparent)

;; Database (list of items)
(define inventory '())

;; DSL Commands
;; 1. Add a new sweet item
(define (add-item name price stock)
  (set! inventory (cons (item name price stock) inventory))
  (displayln (string-append "Added item: " name)))

;; 2. Restock an item
(define (restock-item name amount)
  (set! inventory
        (map (lambda (i)
               (if (equal? (item-name i) name)
                   (item name (item-price i) (+ (item-stock i) amount))
                   i))
             inventory))
  (displayln (string-append "Restocked: " name)))

;; 3. Sell an item (process order)
(define (sell-item name quantity)
  (set! inventory
        (map (lambda (i)
               (if (equal? (item-name i) name)
                   (if (>= (item-stock i) quantity)
                       (item name (item-price i) (- (item-stock i) quantity))
                       (begin
                         (displayln "Not enough stock!")
                         i))
                   i))
             inventory))
  (displayln (string-append "Processed order for: " name)))

;; 4. Show all items
(define (show-inventory)
  (for-each
   (lambda (i)
     (displayln
      (string-append
       (item-name i)
       " | Price: $"
       (number->string (item-price i))
       " | Stock: "
       (number->string (item-stock i)))))
   inventory))

;; Example Usage (DSL in action)
(add-item "Chocolate Cake" 15 10)
(add-item "Cupcake" 3 25)
(add-item "Macaron" 2 30)

(show-inventory)

(sell-item "Cupcake" 5)
(restock-item "Chocolate Cake" 5)

(show-inventory)

;; DSL Explanation

;; Purpose:
;; The SweetByte DSL is designed to help bakeries and dessert shops
;; manage their products, inventory, and sales efficiently.

;; How it supports the company:
;; SweetByte Inc. builds software for sweet treat businesses.
;; This DSL simplifies daily operations like adding items,
;; tracking stock, and processing customer orders.

;; Connection to course concepts:
;; - Demonstrates abstraction by creating custom commands (DSL)
;; - Uses structs and lists to model real-world data
;; - Applies functional programming (map, list processing)
;; - Shows how programming languages can be tailored to specific industries
