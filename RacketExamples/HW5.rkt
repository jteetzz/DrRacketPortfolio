#lang plait

; 1 Shape
(define-type Shape
  (Circle [r : Number])
  (Rectangle [w : Number] [h : Number])
  (Triangle [b : Number] [h : Number]))

(define s1 (Circle 3))
(define s2 (Rectangle 4 5))
(define s3 (Triangle 10 2))

(define (shape-area [s : Shape]) : Number
  (type-case Shape s
    [(Circle r) (* 3.14 (* r r))]
    [(Rectangle w h) (* w h)]
    [(Triangle b h) (/ (* b h) 2)]))

(shape-area s1)
(shape-area s2)
(shape-area s3)


; 2 Vehicles
(define-type Vehicle
  (Car [mpg : Number])
  (Truck [capacity : Number])
  (Bike [electric : Boolean]))

(define v1 (Car 30))
(define v2 (Truck 5000))
(define v3 (Bike #t))

(define (vehicle-info [v : Vehicle]) : String
  (type-case Vehicle v
    [(Car mpg) "Fuel Efficient Car"]
    [(Truck capacity) "Heavy Duty Truck"]
    [(Bike electric) (if electric "Electric Bike" "Regular Bike")]))

(vehicle-info v1)
(vehicle-info v2)
(vehicle-info v3)


; 3 Pets
(define-type Pet
  (Dog [age : Number])
  (Cat [lives : Number])
  (Bird [canFly : Boolean]))

(define p1 (Dog 5))
(define p2 (Cat 9))
(define p3 (Bird #f))

(define (pet-sound [p : Pet]) : String
  (type-case Pet p
    [(Dog age) "Woof"]
    [(Cat lives) "Meow"]
    [(Bird canFly) "Chirp"]))

(pet-sound p1)
(pet-sound p2)
(pet-sound p3)


; 4 Character Game
(define-type Character
  (Warrior [strength : Number])
  (Mage [mana : Number])
  (Archer [speed : Number]))

(define c1 (Warrior 100))
(define c2 (Mage 80))
(define c3 (Archer 60))

(define (attack-power [c : Character]) : Number
  (type-case Character c
    [(Warrior strength) (* strength 2)]
    [(Mage mana) (+ mana 50)]
    [(Archer speed) (+ speed 20)]))

(attack-power c1)
(attack-power c2)
(attack-power c3)


; 5 Person-BMI
(define-type Person
  (Human [weight : Number] [height : Number]))

(define person1 (Human 150 65))

(define (bmi [p : Person]) : Number
  (type-case Person p
    [(Human weight height)
     (/ (* weight 703) (* height height))]))

(bmi person1)


; 6 Superheroes
(define-type Superhero
  (IronMan [energy : Number])
  (Superman [strength : Number])
  (Flash [speed : Number]))

(define h1 (IronMan 80))
(define h2 (Superman 100))
(define h3 (Flash 120))

(define (hero-power [h : Superhero]) : Number
  (type-case Superhero h
    [(IronMan energy) energy]
    [(Superman strength) strength]
    [(Flash speed) speed]))

(hero-power h1)
(hero-power h2)
(hero-power h3)


; 7 Fruits
(define-type Fruit
  (Apple [sweetness : Number])
  (Lemon [sourness : Number])
  (Banana [ripeness : Number]))

(define f1 (Apple 7))
(define f2 (Lemon 9))
(define f3 (Banana 8))

(define (fruit-rating [f : Fruit]) : String
  (type-case Fruit f
    [(Apple sweetness) "Sweet"]
    [(Lemon sourness) "Sour"]
    [(Banana ripeness) "Ripe"]))

(fruit-rating f1)
(fruit-rating f2)
(fruit-rating f3)


; 8 Sports
(define-type Sport
  (Soccer [players : Number])
  (Basketball [quarters : Number])
  (Tennis [sets : Number]))

(define sp1 (Soccer 11))
(define sp2 (Basketball 4))
(define sp3 (Tennis 5))

(define (sport-type [s : Sport]) : String
  (type-case Sport s
    [(Soccer players) "Team Sport"]
    [(Basketball quarters) "Court Sport"]
    [(Tennis sets) "Racquet Sport"]))

(sport-type sp1)
(sport-type sp2)
(sport-type sp3)


; 9 Movie (my example)
(define-type Movie
  (Action [rating : Number])
  (Comedy [laughs : Number])
  (Drama [length : Number]))

(define m1 (Action 8))
(define m2 (Comedy 15))
(define m3 (Drama 120))

(define (movie-genre [m : Movie]) : String
  (type-case Movie m
    [(Action rating) "Exciting"]
    [(Comedy laughs) "Funny"]
    [(Drama length) "Emotional"]))

(movie-genre m1)
(movie-genre m2)
(movie-genre m3)


; 10 Drinks (my example)
(define-type Drink
  (Coffee [caffeine : Number])
  (Juice [sugar : Number])
  (Water [cold : Boolean]))

(define d1 (Coffee 95))
(define d2 (Juice 30))
(define d3 (Water #t))

(define (drink-type [d : Drink]) : String
  (type-case Drink d
    [(Coffee caffeine) "Energy Boost"]
    [(Juice sugar) "Sweet Drink"]
    [(Water cold) (if cold "Cold Water" "Room Temp Water")]))

(drink-type d1)
(drink-type d2)
(drink-type d3)