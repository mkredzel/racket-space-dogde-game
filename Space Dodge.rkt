#lang racket/gui

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GRAPHICAL USER INTERFACE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define $window-width 300)
(define $window-height 600)
(define $window-padding 20)
(define $big-buttons-width 300)
(define $big-buttons-height 50)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; IMAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define $license-img1 (read-bitmap (build-path "images" "license.jpg")))
(define $rules-img1 (read-bitmap (build-path "images" "rules.jpg")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; STARTING WINDOW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define $main-window
  (new frame%
       [label "Space Dodge Game"]
       [width $window-width]
       [height $window-height]
       [border $window-padding]
       [stretchable-width #f]
       [stretchable-height #f]
       [alignment '(center center)]))

(new button%
     [parent $main-window]
     [label "Start game"]
     [min-width $big-buttons-width]
     [min-height $big-buttons-height]
     [callback (lambda (o e) (and (send myframe2 show #t)(send $main-window show #f)))])

(new button%
     [parent $main-window]
     [label "Rules"]
     [min-width $big-buttons-width]
     [min-height $big-buttons-height]
     [callback (lambda (o e) (and (send $rules-window show #t)(send $main-window show #f)))])

(new button%
     [parent $main-window]
     [label "License"]
     [min-width $big-buttons-width]
     [min-height $big-buttons-height]
     [callback (lambda (o e) (and (send $license-window show #t)(send $main-window show #f)))])

(new button%
     [parent $main-window]
     [label "Exit"]
     [min-width $big-buttons-width]
     [min-height $big-buttons-height]
     [callback (lambda (o e) (send $main-window show #f))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NEW GAME WINDOW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define myframe2 (new frame%
                      [label "Nickname"]
                      [width 200] [height 120]))

(define namefield (new text-field%
                       [label "Name"] [parent myframe2] [init-value ""]))

(define $horizontal-buttons-panel
  (new horizontal-panel%
       [parent myframe2]
       [min-width 300]))

(new button% [ parent $horizontal-buttons-panel ] [label "Submit"]
     [min-width 150]
     [min-height 40]
     [callback (lambda (o e) (and   (send namefield get-value)
                                    (send myframe2 show #f)
                                    (main 0)
                                    ))])

(new button% [ parent $horizontal-buttons-panel ] [label "Return"]
     [min-width 150]
     [min-height 40]
     [callback (lambda (o e) (and (send myframe2 show #f)(send $main-window show #t)))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LICENSE WINDOW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define $license-window
  (new frame%
       [label "License"]
       [width $window-width]
       [height $window-height]
       [border $window-padding]
       [stretchable-width #f]
       [stretchable-height #f]
       [alignment '(center center)]))

(new message% [parent $license-window] [label $license-img1])


(new button%
     [parent $license-window]
     [label "Return"]
     [min-width $big-buttons-width]
     [min-height $big-buttons-height]
     [callback (lambda (o e)
                 (and (send $license-window show #f)
                      (send $main-window show #t)))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RULES WINDOW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define $rules-window
  (new frame%
       [label "Rules"]
       [width $window-width]
       [height $window-height]
       [border $window-padding]
       [stretchable-width #f]
       [stretchable-height #f]
       [alignment '(center center)]))

(new message% [parent $rules-window] [label $rules-img1])

(new button%
     [parent $rules-window]
     [label "Return"]
     [min-width $big-buttons-width]
     [min-height $big-buttons-height]
     [callback (lambda (o e)
                 (and (send $rules-window show #f)
                      (send $main-window show #t)))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GUI RUN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(send $main-window show #t)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define s-pos-x 76)
(define s-pos-y 360)
(define s-pos-sqsize 50)
(define gameTimer 180)
(define points 0)
(define rockList (list))
(define fps 0.0083)
(define level 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; STRUCTURES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct rock (x y sqsize) #:transparent #:mutable)

(struct spacecraft (x y) #:transparent #:mutable)
(define s1 (spacecraft s-pos-x s-pos-y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TIMER WITH POINTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (game s)
  
  (set! points (add1 points))
  (set! gameTimer (add1 gameTimer))
  
  (cond
   [(> gameTimer 200) (begin (spawn-rock (+ 26 (* 50 (sub1 (random 1 4)))) -20) (set! gameTimer 40))])

  (cond
   [(= (remainder points 50) 0) (begin (set!  level (add1 level)))])
  
  (define imgs (list))
  (define posns (list))

  (set! imgs (append imgs (list (text (send namefield get-value) 18 "white"))))
  (set! posns (append posns (list (make-posn 35 15))))

  (set! imgs (append imgs (list (text (number->string level) 14 "red"))))
  (set! posns (append posns (list (make-posn 100 15))))

  (set! imgs (append imgs (list (text "level" 14 "red"))))
  (set! posns (append posns (list (make-posn 75 15))))

  (set! imgs (append imgs (list (text (number->string points) 18 "white"))))
  (set! posns (append posns (list (make-posn 120 15))))

  (set! imgs (append imgs (list (rectangle 360 55 "solid" "indigo"))))
  (set! posns (append posns (list (make-posn 0 0))))
  
  (define k 0)
  (for ((tempRock rockList))
    (set! imgs (append imgs (list (read-bitmap (build-path "images" "rock.png")))) 
          )

    (set! posns (append posns (list (make-posn (rock-x tempRock) (rock-y tempRock)))))

    )

  (set! k 0)
  (define tempList (list))
  (for ((tempRock rockList))
    (when (< (rock-y tempRock) 409) (set! tempList (append tempList (list tempRock))))
    )
  (set! rockList tempList)

  (set! imgs (append imgs (list (read-bitmap (build-path "images" "spacecraft.png")))))  
  (set! posns (append posns (list (make-posn (spacecraft-x s1) s-pos-y))))

  (place-images imgs posns (read-bitmap (build-path "images" "canvas.jpg")))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FALLING ROCKS FUNCTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define falling
  (lambda (t)
    (define k 0)
    (define rockTemp 0)
    (for ((rockTemp rockList))
      (add1 k)
      (set-rock-y! rockTemp (+ (rock-y rockTemp) 1))
      (list-set rockList k rockTemp)
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SPAWNING RANDOM ROCKS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define spawn-rock
  (lambda (x y)
    (set! rockList (append rockList (list (rock x y 50))))
    )
  )
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MOVEMENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (move x key)
  (cond
    [(and (key=? key "left") (> (spacecraft-x s1) 26))
     (set-spacecraft-x! s1 (- (spacecraft-x s1) 50)) ]
    
    [(and (key=? key "right") (< (spacecraft-x s1) 126))
     (set-spacecraft-x! s1 (+ (spacecraft-x s1) 50)) ]
    
    [else x]))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GAME OVER ON COLLISION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (game-over w)
  (define return #f)
  (for ((tempRock rockList))
    [when (and (= (rock-x tempRock) (spacecraft-x s1)) (> (rock-y tempRock) (- (spacecraft-y s1) 47)))
      (set! return #t)]
    )
  return
  )

(define end-picture (λ (x)
                      (place-image (text "Game Over" 28 "black")
                                   75 50
                                   (game x))
                      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BIG BANG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(define (main initial)
  (big-bang initial
    [to-draw game]
    [on-key move]
    [on-tick falling fps]
    [stop-when game-over end-picture]
    )
  )