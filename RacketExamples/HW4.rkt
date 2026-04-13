#lang racket
; Exercises 1–5: Given worksheet exercises
; Exercises 6–10: New exercises (same topics)

; Exercise 11: Create a fruit list
(define fruits (list "apple" "banana" "cherry")) 
fruits 

; Exercise 2: Add an Element at the Beginning
; Task: Add "orange" to the beginning of fruits.
(define updated-fruits (cons "orange" fruits)) ; cons adds to the FRONT
updated-fruits ; display the updated list

; Exercise 3: Add an Element at the End
; Task: Add "grape" to the end of fruits.
(define fruits-with-grape (append fruits (list "grape"))) ; append adds to the END
fruits-with-grape ; display the updated list

; Exercise 4: Find the First and Rest of the List
; Task: Get the first element and the rest of fruits.
(define first-fruit (first fruits)) ; first gets the first item
(define rest-fruits (rest fruits))  ; rest gets everything after the first
first-fruit ; display first element
rest-fruits  ; display the remaining elements

; Exercise 5: Count the Number of Elements
; Task: Find the number of elements in fruits.
(define fruit-count (length fruits)) ; length counts how many items are in the list
fruit-count ; display the count

; Exercise 6: Create a List 
; Task: Create a list named colors that contains "red", "blue", "green".
(define colors (list "red" "blue" "green")) ; make the colors list
colors ; display the list

; Exercise 7: Add an Element at the Beginning 
; Task: Add "yellow" to the beginning of colors.
(define updated-colors (cons "yellow" colors)) ; add "yellow" to the front
updated-colors ; display the updated list

; Exercise 8: Add an Element at the End 
; Task: Add "purple" to the end of colors.
(define colors-with-purple (append colors (list "purple"))) ; add to the end
colors-with-purple ; display the updated list

; Exercise 9: Find the First and Rest 
; Task: Get the first element of colors and the rest of colors.
(define first-color (first colors)) ; first color in the list
(define rest-colors (rest colors))  ; everything after the first color
first-color ; display first element
rest-colors  ; display the rest of the list

; Exercise 10: Count the Number of Elements 
; Task: Find the number of elements in colors.
(define color-count (length colors)) ; count elements in colors
color-count ; display the count
