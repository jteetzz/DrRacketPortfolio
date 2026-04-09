#lang racket

; ╔══════════════════════════════════════════════════════╗
; ║   movie-recommender.rkt                              ║
; ║   Movie Recommendation DSL —                         ║
; ║   ONE FILE. Open in DrRacket and press RUN.          ║
; ╚══════════════════════════════════════════════════════╝


; ══════════════════════════════════════════
; STEP 1 — THE DATA BOX (struct)
; ══════════════════════════════════════════
; A struct is like a box with named slots.
; Our "movie" box has 4 slots:
;   title  : the movie name      (string)
;   genre  : the type of film    (string)
;   year   : release year        (number)
;   rating : score from 0 to 10  (number)

(struct movie (title genre year rating)
  #:transparent)

; To CREATE a movie:
;   (movie "Title" "genre" year rating)
;
; Racket auto-creates these reader functions:
;   (movie-title  m)  -> the title
;   (movie-genre  m)  -> the genre
;   (movie-year   m)  -> the year
;   (movie-rating m)  -> the rating


; ══════════════════════════════════════════
; STEP 2 — THE CONDITIONS (plain functions)
; ══════════════════════════════════════════
; Each condition:
;   - receives a movie (m)
;   - returns #t (true) or #f (false)

; ── Genre conditions ──────────────────────

(define (action? m)
  (equal? (movie-genre m) "action"))

(define (comedy? m)
  (equal? (movie-genre m) "comedy"))

(define (drama? m)
  (equal? (movie-genre m) "drama"))

(define (horror? m)
  (equal? (movie-genre m) "horror"))

(define (sci-fi? m)
  (equal? (movie-genre m) "sci-fi"))

(define (thriller? m)
  (equal? (movie-genre m) "thriller"))

; ── Era conditions ────────────────────────

(define (classic? m)
  ; Released before 1990
  (< (movie-year m) 1990))

(define (recent? m)
  ; Released after 2015
  (> (movie-year m) 2015))

(define (nineties? m)
  ; Released between 1990 and 1999
  (and (>= (movie-year m) 1990)
       (<  (movie-year m) 2000)))

; ── Rating conditions ─────────────────────

(define (masterpiece? m)
  ; Rating above 8.5
  (> (movie-rating m) 8.5))

(define (highly-rated? m)
  ; Rating above 7.5
  (> (movie-rating m) 7.5))

(define (decent? m)
  ; Rating above 6.0
  (> (movie-rating m) 6.0))

(define (poorly-rated? m)
  ; Rating below 5.0
  (< (movie-rating m) 5.0))


; ══════════════════════════════════════════
; STEP 3 — THE RULES
; Each rule = (list NAME CONDITION MESSAGE)
; ══════════════════════════════════════════

(define rule-classic-action
  (list
   "Classic action hit"
   (lambda (m) (and (action? m) (classic? m) (highly-rated? m)))
   "A timeless action classic — essential viewing!"))

(define rule-modern-action
  (list
   "Modern blockbuster"
   (lambda (m) (and (action? m) (recent? m) (highly-rated? m)))
   "A solid recent action film. Great for movie night."))

(define rule-masterpiece
  (list
   "Masterpiece"
   (lambda (m) (masterpiece? m))
   "Exceptional rating. One of the best films in its genre."))

(define rule-acclaimed-drama
  (list
   "Acclaimed drama"
   (lambda (m) (and (drama? m) (highly-rated? m)))
   "A highly praised drama. Perfect for a serious movie night."))

(define rule-fun-comedy
  (list
   "Fun comedy"
   (lambda (m) (and (comedy? m) (decent? m)))
   "A well-rated comedy. Great for relaxing and laughing."))

(define rule-classic-scifi
  (list
   "Sci-fi classic"
   (lambda (m) (and (sci-fi? m) (classic? m)))
   "A foundational sci-fi film. Hugely influential on the genre."))

(define rule-award-thriller
  (list
   "Award-worthy thriller"
   (lambda (m) (and (thriller? m) (recent? m) (masterpiece? m)))
   "A recent critically acclaimed thriller. Do not miss this one."))

(define rule-horror-warning
  (list
   "Warning: horror"
   (lambda (m) (horror? m))
   "Horror film — not for everyone. Check your tolerance first!"))

(define rule-low-rating
  (list
   "Caution: low score"
   (lambda (m) (poorly-rated? m))
   "Low critical score. Watch the trailer before committing."))

(define rule-nineties-gem
  (list
   "90s nostalgia pick"
   (lambda (m) (and (nineties? m) (highly-rated? m)))
   "A beloved 90s film. Nostalgic and highly entertaining."))

; ══════════════════════════════════════════
; NEW CONDITION: animated?
; Matches movies in the "animation" genre.
; ══════════════════════════════════════════

(define (animated? m)
  (equal? (movie-genre m) "animation"))

; ══════════════════════════════════════════
; NEW RULE: Family animation pick
; Fires when the movie is animated AND highly rated.
; ══════════════════════════════════════════

(define rule-family-animation
  (list
   "Family animation pick"
   (lambda (m) (and (animated? m) (highly-rated? m)))
   "A top-rated animated film. Perfect for all ages — or just adults who know better."))

; Collect ALL rules into one list
(define all-rules
  (list
   rule-masterpiece
   rule-classic-action
   rule-modern-action
   rule-acclaimed-drama
   rule-fun-comedy
   rule-classic-scifi
   rule-award-thriller
   rule-nineties-gem
   rule-horror-warning
   rule-low-rating
   rule-family-animation))   ; <-- new rule added here


; ══════════════════════════════════════════
; STEP 4 — THE ENGINE (evaluate)
; ══════════════════════════════════════════
; Loops through every rule.
; If the condition is true -> prints the message.

(define (evaluate m rules)
  (for ([rule rules])
    (define name      (first  rule))
    (define condition (second rule))
    (define message   (third  rule))
    (when (condition m)
      (displayln (string-append "  [" name "]"))
      (displayln (string-append "   " message))
      (newline))))


; ══════════════════════════════════════════
; STEP 5 — THE REPORT (print-report)
; ══════════════════════════════════════════

(define (print-report m)
  (define line (make-string 44 #\=))
  (displayln line)
  (displayln "  Movie Recommendation Report")
  (displayln line)
  (displayln (string-append "  Title  : " (movie-title m)))
  (displayln (string-append "  Genre  : " (movie-genre m)))
  (displayln (string-append "  Year   : " (number->string (movie-year m))))
  (displayln (string-append "  Rating : " (number->string (movie-rating m)) " / 10"))
  (displayln line)
  (newline)
  (evaluate m all-rules)
  (displayln line)
  (newline))


; ══════════════════════════════════════════
; THE MOVIES (original)
; ══════════════════════════════════════════

(define Lifeisbeutiful
  (movie "Life is beutiful" "action" 2010 8.8))

(define terminator
  (movie "The Terminator" "action" 1984 8.1))

(define batman
  (movie "Batman" "thriller" 2019 8.5))

(define airplane
  (movie "Airplane!" "comedy" 1980 7.7))

(define beetlejuice
  (movie "Beetlejuice" "horror" 2019 7.1))

(define metropolis
  (movie "Metropolis" "sci-fi" 1927 8.3))

(define the-matrix
  (movie "The Matrix" "action" 1999 8.7))

(define clueless
  (movie "Clueless" "comedy" 1995 6.9))

; ══════════════════════════════════════════
; NEW MOVIE 1: Interstellar
; A recent, highly rated sci-fi drama.
; Tests: recent?, sci-fi?, masterpiece?
; ══════════════════════════════════════════

(define interstellar
  (movie "Interstellar" "sci-fi" 2014 8.7))

; ══════════════════════════════════════════
; NEW MOVIE 2: Spider-Man: Into the Spider-Verse
; Uses the new "animation" genre and
; the new rule-family-animation rule.
; ══════════════════════════════════════════

(define spider-verse
  (movie "Spider-Man: Into the Spider-Verse" "animation" 2018 8.4))


; ══════════════════════════════════════════
; RUN THE REPORTS
; ══════════════════════════════════════════

(print-report Lifeisbeutiful)
(print-report terminator)
(print-report batman)
(print-report airplane)
(print-report beetlejuice)
(print-report metropolis)
(print-report the-matrix)
(print-report clueless)
(print-report interstellar)      ; <-- new movie 1
(print-report spider-verse)      ; <-- new movie 2


; ══════════════════════════════════════════
; ADD YOUR OWN MOVIE HERE:
;
; (define my-movie
;   (movie "Your Title" "action" 2023 8.0))
;
; (print-report my-movie)
;
; Available genres:
;   "action"  "comedy"  "drama"
;   "horror"  "sci-fi"  "thriller"
;   "animation"                       <-- new!
; ══════════════════════════════════════════
