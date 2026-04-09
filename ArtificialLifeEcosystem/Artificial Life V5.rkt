#lang racket
(require 2htdp/universe)
(require 2htdp/image)

;; DATA STRUCTURES
(struct pos (x y) #:transparent)
(struct agent (pos energy) #:transparent)
(struct food (pos) #:transparent)
(struct predator (pos energy) #:transparent)
(struct obstacle (pos) #:transparent)

;; World holds all entities
(struct world (agents foods predators obstacles) #:transparent)

;; CONSTANTS
(define WIDTH 400)
(define HEIGHT 400)
(define AGENT-SPEED 5)
(define PREDATOR-SPEED 5)
(define INITIAL-AGENT-ENERGY 100)
(define INITIAL-PREDATOR-ENERGY 100)
(define OBSTACLE-SIZE 10)
(define FOOD-SIZE 5)
(define AGENT-RADIUS 5)

;; HELPERS: Distance and Position Math

(define (distance p1 p2)
  (sqrt (+ (sqr (- (pos-x p1) (pos-x p2)))
           (sqr (- (pos-y p1) (pos-y p2))))))

;; Clamp a value between lo and hi
(define (clamp v lo hi)
  (max lo (min hi v)))

;; Move current-pos toward target-pos at given speed
(define (move-toward current-pos target-pos speed)
  (let* ([dx (- (pos-x target-pos) (pos-x current-pos))]
         [dy (- (pos-y target-pos) (pos-y current-pos))]
         [dist (distance current-pos target-pos)]
         [norm (if (> dist 0) (/ speed dist) 0)])
    (pos (clamp (+ (pos-x current-pos) (* dx norm)) 0 WIDTH)
         (clamp (+ (pos-y current-pos) (* dy norm)) 0 HEIGHT))))

;; Move in a random direction (fallback when no targets)
(define (random-move current-pos speed)
  (let* ([angle (* (random 628) 0.01)] ; 0 to ~2*pi
         [new-x (+ (pos-x current-pos) (* speed (cos angle)))]
         [new-y (+ (pos-y current-pos) (* speed (sin angle)))])
    (pos (clamp new-x 0 WIDTH)
         (clamp new-y 0 HEIGHT))))

;; HELPERS: Finding Nearest Targets

;; Returns the element of lst with minimum (key-fn elem), or #f if empty
(define (min-by lst key-fn)
  (if (empty? lst)
      #f
      (foldl (lambda (item best)
               (if (< (key-fn item) (key-fn best)) item best))
             (first lst)
             (rest lst))))

(define (closest-food agent-pos foods)
  (min-by foods (lambda (f) (distance agent-pos (food-pos f)))))

(define (closest-agent pred-pos agents)
  (min-by agents (lambda (a) (distance pred-pos (agent-pos a)))))

(define (closest-predator agent-pos predators)
  (min-by predators (lambda (p) (distance agent-pos (predator-pos p)))))

;; OBSTACLE COLLISION
;; Checks if a position overlaps any obstacle (10x10 square)

(define (in-obstacle? p obstacles)
  (ormap (lambda (obs)
           (let* ([ox (pos-x (obstacle-pos obs))]
                  [oy (pos-y (obstacle-pos obs))])
             (and (< (abs (- (pos-x p) ox)) (+ OBSTACLE-SIZE AGENT-RADIUS))
                  (< (abs (- (pos-y p) oy)) (+ OBSTACLE-SIZE AGENT-RADIUS)))))
         obstacles))

;; Returns new-pos if it does not collide, otherwise keeps current-pos
(define (safe-pos current-pos new-pos obstacles)
  (if (in-obstacle? new-pos obstacles)
      current-pos
      new-pos))

;; AGENT AI — IMPROVED: Flee Predators + Seek Food
;;
;; The improved AI gives agents TWO behaviors:
;;   1. FLEE: If a predator is within 60 pixels, the agent
;;      moves AWAY from it — survival takes priority over food.
;;   2. SEEK: Otherwise, move toward the nearest food as usual.
;;
;; This creates emergent "chase" dynamics: predators herd agents,
;; agents disperse, and weaker agents at the edges are caught first.

(define FLEE-RADIUS 60) ; how close a predator must be to trigger fleeing

(define (agent-ai a foods predators obstacles)
  (let* ([apos (agent-pos a)]
         ;; Check for nearby predators within flee radius
         [nearby-preds (filter (lambda (p) (< (distance apos (predator-pos p)) FLEE-RADIUS))
                               predators)]
         [new-pos
          (cond
            ;; FLEE: move away from the nearest predator
            [(not (empty? nearby-preds))
             (let* ([nearest-pred (closest-predator apos nearby-preds)]
                    [ppos (predator-pos nearest-pred)]
                    ;; Invert the direction: move away instead of toward
                    [away-pos (pos (clamp (- (* 2 (pos-x apos)) (pos-x ppos)) 0 WIDTH)
                                   (clamp (- (* 2 (pos-y apos)) (pos-y ppos)) 0 HEIGHT))])
               (safe-pos apos (move-toward apos away-pos AGENT-SPEED) obstacles))]
            ;; SEEK: move toward nearest food
            [(not (empty? foods))
             (let ([tf (closest-food apos foods)])
               (safe-pos apos (move-toward apos (food-pos tf) AGENT-SPEED) obstacles))]
            ;; WANDER: random movement if no food and no threat
            [else
             (safe-pos apos (random-move apos AGENT-SPEED) obstacles)])])
    (agent new-pos (- (agent-energy a) 1))))

;; PREDATOR AI — IMPROVED: Prioritize Weakest Agent
;;
;; Basic AI would chase the nearest agent. Our improved predator
;; instead targets the WEAKEST agent (lowest energy), because
;; weak agents are slower to respond and easier to catch.
;; This makes predators smarter hunters.

(define (weakest-agent agents)
  (min-by agents (lambda (a) (agent-energy a))))

(define (predator-ai p agents obstacles)
  (let* ([ppos (predator-pos p)]
         [target (weakest-agent agents)] ; target weakest, not nearest
         [new-pos
          (if target
              (safe-pos ppos (move-toward ppos (agent-pos target) PREDATOR-SPEED) obstacles)
              (safe-pos ppos (random-move ppos PREDATOR-SPEED) obstacles))])
    (predator new-pos (- (predator-energy p) 2)))) ; predators lose 2 energy/tick

;; FOOD: Eating and Spawning
;; An agent eats food if within 10 pixels; gains 50 energy
(define (agent-eat-food a foods)
  (let ([near (filter (lambda (f) (< (distance (agent-pos a) (food-pos f)) 10)) foods)])
    (if (empty? near)
        a
        (agent (agent-pos a) (+ (agent-energy a) 50)))))

;; Foods consumed by any agent this tick
(define (eaten-foods agents foods)
  (filter (lambda (f)
            (ormap (lambda (a) (< (distance (agent-pos a) (food-pos f)) 10)) agents))
          foods))

;; Food regenerates: 2% chance per tick = (random 50) == 0
(define (maybe-spawn-food foods)
  (if (= (random 50) 0)
      (cons (food (pos (random WIDTH) (random HEIGHT))) foods)
      foods))

;; PREDATOR HUNTING
;; Predator catches an agent within 15 pixels → gains 100 energy
(define (predator-hunt pred agents)
  (let ([kills (filter (lambda (a) (< (distance (predator-pos pred) (agent-pos a)) 15))
                       agents)])
    (if (empty? kills)
        pred
        (predator (predator-pos pred) (+ (predator-energy pred) 100)))))

;; Remove agents caught by ANY predator
(define (surviving-agents agents predators)
  (filter (lambda (a)
            (not (ormap (lambda (p) (< (distance (predator-pos p) (agent-pos a)) 15))
                        predators)))
          agents))

;; REPRODUCTION
;; Agents: energy > 150 -> split into two, each with half energy
;; Predators: energy > 200 -> split into two, each with half energy

(define (reproduce-agents agents)
  (foldl (lambda (a acc)
           (if (> (agent-energy a) 150)
               (let ([half (/ (agent-energy a) 2)])
                 (cons (agent (agent-pos a) half)
                       (cons (agent (pos (clamp (+ (pos-x (agent-pos a)) 5) 0 WIDTH)
                                        (clamp (+ (pos-y (agent-pos a)) 5) 0 HEIGHT))
                                    half)
                             acc)))
               (cons a acc)))
         '()
         agents))

(define (reproduce-predators predators)
  (foldl (lambda (p acc)
           (if (> (predator-energy p) 200)
               (let ([half (/ (predator-energy p) 2)])
                 (cons (predator (predator-pos p) half)
                       (cons (predator (pos (clamp (+ (pos-x (predator-pos p)) 5) 0 WIDTH)
                                           (clamp (+ (pos-y (predator-pos p)) 5) 0 HEIGHT))
                                       half)
                             acc)))
               (cons p acc)))
         '()
         predators))

;; WORLD UPDATE (TICK)
(define (update-world w)
  (let* ([agents    (world-agents w)]
         [foods     (world-foods w)]
         [preds     (world-predators w)]
         [obstacles (world-obstacles w)]

         ;; 1. Move agents with improved AI (flee + seek)
         [moved-agents (map (lambda (a) (agent-ai a foods preds obstacles)) agents)]

         ;; 2. Agents eat nearby food
         [fed-agents (map (lambda (a) (agent-eat-food a foods)) moved-agents)]

         ;; 3. Remove eaten foods; possibly spawn new food (2% per tick)
         [eaten  (eaten-foods fed-agents foods)]
         [remaining-foods (filter (lambda (f) (not (member f eaten))) foods)]
         [new-foods (maybe-spawn-food remaining-foods)]

         ;; 4. Move predators with improved AI (chase weakest)
         [moved-preds (map (lambda (p) (predator-ai p fed-agents obstacles)) preds)]

         ;; 5. Predators hunt: gain energy for catches
         [hunting-preds (map (lambda (p) (predator-hunt p fed-agents)) moved-preds)]

         ;; 6. Remove agents that were caught by predators
         [alive-agents (surviving-agents fed-agents hunting-preds)]

         ;; 7. Remove agents with 0 energy (starved)
         [alive-agents2 (filter (lambda (a) (> (agent-energy a) 0)) alive-agents)]

         ;; 8. Remove predators with 0 energy (starved)
         [alive-preds (filter (lambda (p) (> (predator-energy p) 0)) hunting-preds)]

         ;; 9. Agent reproduction (energy > 150)
         [repro-agents (reproduce-agents alive-agents2)]

         ;; 10. Predator reproduction (energy > 200)
         [repro-preds (reproduce-predators alive-preds)])

    (world repro-agents new-foods repro-preds obstacles)))

;; COLOR BY ENERGY
;; Agents: Red ≤50, Yellow 51–100, Green >100
(define (agent-color energy)
  (cond [(<= energy 50)  "red"]
        [(<= energy 100) "yellow"]
        [else            "green"]))

;; Predators: Purple ≤75, Orange 76–150, Blue >150
(define (predator-color energy)
  (cond [(<= energy 75)  "purple"]
        [(<= energy 150) "orange"]
        [else            "blue"]))

;; DRAWING
(define (draw-world w)
  (let* ([bg (empty-scene WIDTH HEIGHT "black")]

         ;; Draw obstacles (gray 10x10 squares)
         [bg-obs (foldl (lambda (obs img)
                          (place-image (square OBSTACLE-SIZE "solid" "gray")
                                       (pos-x (obstacle-pos obs))
                                       (pos-y (obstacle-pos obs))
                                       img))
                        bg
                        (world-obstacles w))]

         ;; Draw food (small green squares)
         [bg-food (foldl (lambda (f img)
                           (place-image (square FOOD-SIZE "solid" "lime")
                                        (pos-x (food-pos f))
                                        (pos-y (food-pos f))
                                        img))
                         bg-obs
                         (world-foods w))]

         ;; Draw predators (circles colored by energy, with energy label)
         [bg-preds (foldl (lambda (p img)
                            (let* ([energy (predator-energy p)]
                                   [x (pos-x (predator-pos p))]
                                   [y (pos-y (predator-pos p))]
                                   [col (predator-color energy)])
                              (place-image
                               (text (number->string (inexact->exact (round energy))) 7 "white")
                               x (- y 12)
                               (place-image (circle 7 "solid" col) x y img))))
                          bg-food
                          (world-predators w))]

         ;; Draw agents (circles colored by energy, with energy label)
         [bg-agents (foldl (lambda (a img)
                             (let* ([energy (agent-energy a)]
                                    [x (pos-x (agent-pos a))]
                                    [y (pos-y (agent-pos a))]
                                    [col (agent-color energy)])
                               (place-image
                                (text (number->string (inexact->exact (round energy))) 7 "white")
                                x (- y 10)
                                (place-image (circle AGENT-RADIUS "solid" col) x y img))))
                           bg-preds
                           (world-agents w))]

         ;; HUD: agent and predator counts
         [count-text (string-append
                      "Agents: " (number->string (length (world-agents w)))
                      "  Predators: " (number->string (length (world-predators w)))
                      "  Food: " (number->string (length (world-foods w))))]
         [bg-hud (place-image (text count-text 10 "white") 130 10 bg-agents)])

    bg-hud))

;; INITIAL WORLD SETUP
;; 4 agents, 5 food items, 2 predators, 5 obstacles
(define INITIAL-OBSTACLES
  (list (obstacle (pos 80  80))
        (obstacle (pos 200 150))
        (obstacle (pos 320 80))
        (obstacle (pos 100 300))
        (obstacle (pos 280 280))))

(define (make-initial-world)
  (world
   ;; 4 agents
   (list (agent (pos 50  50)  INITIAL-AGENT-ENERGY)
         (agent (pos 350 50)  INITIAL-AGENT-ENERGY)
         (agent (pos 50  350) INITIAL-AGENT-ENERGY)
         (agent (pos 350 350) INITIAL-AGENT-ENERGY))
   ;; 5 food items placed away from obstacles
   (list (food (pos 130 130))
         (food (pos 260 130))
         (food (pos 130 260))
         (food (pos 260 260))
         (food (pos 200 200)))
   ;; 2 predators starting near center
   (list (predator (pos 180 180) INITIAL-PREDATOR-ENERGY)
         (predator (pos 220 220) INITIAL-PREDATOR-ENERGY))
   ;; 5 static obstacles
   INITIAL-OBSTACLES))

(define initial-world (make-initial-world))

;; RUN SIMULATION
(big-bang initial-world
  [to-draw   draw-world]
  [on-tick   update-world 0.05]) ; ~20 fps
