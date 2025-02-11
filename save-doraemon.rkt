;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname save-doraemon) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;;;;;;;;;**********          Programming Fundamentals Group Project          **********;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*****     SAVE DORAEMON     *****;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;**********     Hun Rim, George Batyrev, Giorgio Bonetto, Nicola Fontana   **********;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     VERSION 1.0    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; LIBRARIES UTILIZED
;________________________________________________________________________________________________

(require 2htdp/universe)
(require 2htdp/image)
;________________________________________________________________________________________________

; For better clarity each segment  of code is labelled by a tag, to undertand the conceptual relevance

;_________________________________________BACKGROUND DETAILS___________________________________________

;; Background dimentions of the game / tag: BACKGROUND

(define WIDTH 1040)
(define HEIGHT 600)
(define BACKGROUND (empty-scene WIDTH HEIGHT))

; Quit system / tag: BACKGORUND

;;Quit Written 
(define QUIT (bitmap "quit.png"))

;; The decorative backgrounds images / tag: BACKGROUND 
(define NIGHT (bitmap "night.jpeg"))
(define DAY (bitmap "day.jpeg"))


;; Change between the light/dark mode / tag: BACKGROUND

;; Data type

;theme : Image -> Image
; it change the background-image every 1000 ticks
; header : (define (theme gui) image)

;; Example

(check-expect (theme case-1) DAY)
(check-expect (theme (make-GUI 300 (make-posn 500 (random -200 150)) (make-posn 1000 (random -200 150)) #f 1000 #f 0 #f I1)) NIGHT)


;; Code

(define (theme gui)
  (cond
    [(= (remainder (quotient (GUI-score gui) 1000) 2) 0) DAY]     ;; Nightmode
    [(= (remainder (quotient (GUI-score gui) 1000) 2) 1) NIGHT])) ;;Daymode


;________________________________________________________________________________________________

;; The moving object up and down / tag: DORAEMON

(define DORAEMON (bitmap "doraemon.png"))
(define CRASH (bitmap "crash.png"))
(define EASTER (scale (/ 2 15) (bitmap "superdo.png")))
(define END-SIGN (scale (/ 2 1) (bitmap "game-over.png")))
(define X 100)
(define ITEM (bitmap "dora.png"))

;; Data type

;character : Image -> Image
;  it change the image of the avatar when it crush in to pillar
; header : (define (character gui) image)

;; Example

(check-expect (character case-1) DORAEMON)
(check-expect (character (make-GUI 300 (make-posn 500 (random -200 150)) (make-posn 1000 (random -200 150)) #t 0 #f 0 #f I1)) CRASH)
(check-expect (character (make-GUI 300 (make-posn 500 (random -200 150)) (make-posn 1000 (random -200 150)) #t 2200 #f 0 #f I1)) EASTER)

;; Code

(define (character gui)
  (cond
    [(and (>= (GUI-score gui) 2112) (>= 2205 (GUI-score gui))) EASTER] ;; Easter Egg doraemon
    [(boolean=? (GUI-dead gui) #f) DORAEMON]
    [else CRASH]))

;________________________________________________________________________________________________


;; UPPER TUBE is a constant

(define TUBE-2 (overlay (overlay/offset (rotate 90 (rectangle 23 10 "solid" "Light Sky Blue"))
                        -74 450
                        (square 0.01 "solid" "white"))
                (overlay/offset (rotate 90 (rectangle 23 63 "solid" "Royal Blue"))
                        -48 450
                        (square 0.01 "solid" "white"))
                (overlay/offset (rotate 90 (rectangle 23 80 "solid" "Midnight Blue"))
                        0 450
                        (square 0.01 "solid" "white"))
                        (overlay/offset (rotate 90 (rectangle 24 81 "solid" "white"))
                        0 450
                        (square 0.01 "solid" "white"))
                (overlay/offset (rectangle 12 450 "solid" "Midnight Blue")
                        65 0
                        (square 0.01 "solid" "white"))
                (overlay/offset (rectangle 9 450 "solid" "Light Sky Blue")
                        -67 0
                        (square 0.01 "solid" "white"))
                (rectangle 70 450 "solid" "Royal Blue")
                        (rectangle 71 (+ 1 450) "solid" "white")))

;________________________________________________________________________________________________

;; LOWER TUBE is a constant

(define TUBE-1 (overlay (overlay/offset (rotate 90 (rectangle 23 10 "solid" "Light Sky Blue"))
                        -74 (- 450)
                        (square 0.01 "solid" "white"))
                (overlay/offset (rotate 90 (rectangle 23 63 "solid" "Royal Blue"))
                        -48 (- 450)
                        (square 0.01 "solid" "white"))
                (overlay/offset (rotate 90 (rectangle 23 80 "solid" "Midnight Blue"))
                        0 (- 450)
                        (square 0.01 "solid" "white"))
                        (overlay/offset (rotate 90 (rectangle 24 81 "solid" "white"))
                        0 (- 450)
                        (square 0.01 "solid" "white"))
                (overlay/offset (rectangle 12 450 "solid" "Midnight Blue")
                        65 0
                        (square 0.01 "solid" "white"))
                (overlay/offset (rectangle 9 450 "solid" "Light Sky Blue")
                        -67 0
                        (square 0.01 "solid" "white"))
                (rectangle 70 450 "solid" "Royal Blue")
                        (rectangle 71 (+ 1 450) "solid" "white")))


;________________________________________________________________________________________________

; Graphical User Interface (GUI) deatail / tag: MECHANICS 

;;Data type 

; a GUI is a structure (make-GUI current-state obstacle obstacle1 dead score game))
;    where current-state   : Number
;          obstacle    : Posn
;          obstacle1   : Posn
;          score       : Number
;          game        : Boolean
;          timer       : Number
;          quit?       : Boolean
;          item        : State
; it is the GUI of the game and it act as a center of control of the game 

(define-struct GUI (current-state obstacle obstacle1 dead score game timer quit? item))

; a ItemDetails is a structure (make-ItemDetails cord time pillar type))
;    where cord      : Posn
;          time      : Number
;          pillar    : Number
;          type      : Number
; It controls the buff-item and nerf-item

(define-struct ItemDetails (cord time pillar type))


(define I1 (make-ItemDetails (make-posn (random 1100 10000) (random 50 550)) 0 220 (random 1 3)))
(define I2 (make-ItemDetails (make-posn 4000 340) 0 220 0))
(define I3 (make-ItemDetails (make-posn 4000 340) 22 220 0))


;; Example of GUI
(define case-1 (make-GUI 300 (make-posn 500 (random -200 150)) (make-posn 1000 (random -200 150)) #f 0 #f 0 #f I1))
(define case-2 (make-GUI 300 (make-posn 500 100) (make-posn 1000 100) #f 0 #t 0 #f I1))
(define case-3 (make-GUI 300 (make-posn (- 1) 100) (make-posn 499 100) #f 0 #f 0 #f I1))
(define case-4 (make-GUI 300 (make-posn 15 85) (make-posn 156 44) #t 0 #t 0 #f I1))
(define case-5 (make-GUI 300 (make-posn 432 789) (make-posn 637 399) #f 0 #f 0 #f I1))
(define case-6 (make-GUI 300 (make-posn 239 13) (make-posn 599 99) #f 0 #f 0 #f (make-ItemDetails (make-posn (random 2000 10000) (random 50 550)) 170 220 0)))

(define case-17 (make-GUI 300 (make-posn 432 789) (make-posn 637 399) #f 0 #f 0 #f I2))
(define case-18 (make-GUI 300 (make-posn 432 789) (make-posn 637 399) #f 0 #t 0 #f I2))
(define case-19 (make-GUI 300 (make-posn 432 789) (make-posn 637 399) #t 0 #t 0 #f I2))
(define case-20 (make-GUI 300 (make-posn 432 789) (make-posn 637 399) #t 0 #f 0 #f I2))
(define case-21 (make-GUI 700 (make-posn 500 100) (make-posn 1000 100) #f 0 #t 0 #f I2))
(define case-22 (make-GUI 700 (make-posn 500 100) (make-posn 1000 100) #f 0 #t 0 #f I3))


(define case-d1 (make-GUI 300 (make-posn 200 100) (make-posn 599 99) #t 500 #f 0 #f I1))
(define case-d2 (make-GUI 300 (make-posn 200 100) (make-posn 599 99) #t 1200 #f 0 #f I1))
(define case-d3 (make-GUI 300 (make-posn 200 100) (make-posn 599 99) #t 3100 #f 0 #f I1))
(define case-d4 (make-GUI 300 (make-posn 200 100) (make-posn 599 99) #t 310 #f 32 #f I1))

;________________________________GAME-SCREEN LOGIC AND MECHANICS____________________________________

; Scoring system / tag: MECHANICS

;; Data type

; score : State -> Text
; it take the score of the game
;header : (define (SCORE gui) (text 40 "black")

;; Example

(check-expect (SCORE case-1) (text/font "SCORE: 0000" 30 "black" #f "roman" 'slant 'normal #f))
(check-expect (SCORE (make-GUI 300 (make-posn 500 (random -200 150)) (make-posn 1000 (random -200 150)) #f 1020 #f 0 #f I1)) (text/font "SCORE: 1020" 30 "white" #f "roman" 'slant 'normal #f))
(check-expect (SCORE (make-GUI 300 (make-posn 500 (random -200 150)) (make-posn 1000 (random -200 150)) #f 120 #f 0 #f I1)) (text/font "SCORE: 0120" 30 "black" #f "roman" 'slant 'normal #f))
(check-expect (SCORE (make-GUI 300 (make-posn 500 (random -200 150)) (make-posn 1000 (random -200 150)) #f 20 #f 0 #f I1)) (text/font "SCORE: 0020" 30 "black" #f "roman" 'slant 'normal #f))
(check-expect (SCORE (make-GUI 300 (make-posn 500 (random -200 150)) (make-posn 1000 (random -200 150)) #f 2001 #f 0 #f I1)) (text/font "SCORE: 2001" 30 "black" #f "roman" 'slant 'normal #f))

;; Code 

(define (SCORE gui)
  (cond
    [(> 10 (GUI-score gui)) (text/font (string-append "SCORE: 000" (number->string (GUI-score gui)))
                                       30 "black" #f 'roman 'slant 'normal #f)]
    [(> 100 (GUI-score gui)) (text/font (string-append "SCORE: 00" (number->string (GUI-score gui)))
                                       30 "black" #f 'roman 'slant 'normal #f)]
    [(> 1000 (GUI-score gui)) (text/font (string-append "SCORE: 0" (number->string (GUI-score gui)))
                                       30 "black" #f 'roman 'slant 'normal #f)]
    [(= (remainder (quotient (GUI-score gui) 1000) 2) 0) (text/font (string-append "SCORE: " (number->string (GUI-score gui)))
                                                                              30 "black" #f "roman" 'slant 'normal #f)]     ;; Nightmode SCORE (BLACK)
    [(= (remainder (quotient (GUI-score gui) 1000) 2) 1) (text/font (string-append "SCORE: " (number->string (GUI-score gui)))
                                                                              30 "white" #f "roman" 'slant 'normal #f)]))   ;;Daymode SCORE (WHITE)
;________________________________________________________________________________________________

; Respawn system / tag: LOGIC

;; Data type

;respawn : State ->  State
; It spawn the pilar
;header : (define (respawn gui) GUI)

;; Example

(check-expect (respawn case-2) (make-GUI 300
                              (make-posn 500 100)
                              (make-posn 1000 100) #f 0 #t 0 #f I1))

;; Code - respawn of pillar
(define (respawn gui)
  (cond
    [(> 0 (posn-x (GUI-obstacle gui)))               ;; respawn of first pillar
     (make-GUI (GUI-current-state gui)
               (make-posn 1000 (random -200 150))
               (GUI-obstacle1 gui)
               #f
               (GUI-score gui)
               (GUI-game gui)
               (GUI-timer gui)
               #f
               (GUI-item gui))]
    [(> 0 (posn-x (GUI-obstacle1 gui)))              ;; respawn of second pillar
     (make-GUI (GUI-current-state gui)
               (GUI-obstacle gui)
               (make-posn 1000 (random -200 150))   
               #f
               (GUI-score gui)
               (GUI-game gui)
               (GUI-timer gui)
               #f
               (GUI-item gui))]
    [else gui]))

;________________________________________________________________________________________________

; BOTTOM PILLAR / tag: LOGIC

;Spawn the pillar one above the other
(define (bottom-pillar gui)
  (+ (posn-y (GUI-obstacle gui)) 450 (ItemDetails-pillar (GUI-item (buff-collision gui)))))   ;; Deciding the y-coordinate of bottom pillar (pillar 1)
                                                                                              ;; = Top pillar + 450 + gap between pillar
(define (bottom-pillar1 gui)
  (+ (posn-y (GUI-obstacle1 gui)) 450 (ItemDetails-pillar (GUI-item (buff-collision gui)))))  ;; Deciding the y-coordinate of bottom pillar (pillar 2)


;________________________________________________________________________________________________

; Detector/collision system / tag: LOGIC

;; Data type

;x-detector : State -> Boolean
;  it check if the Doraemon is in range of obstacle (x-axis)

(define (x-detector gui)
  (cond
    [(and (> 168 (posn-x (GUI-obstacle gui))) (> (posn-x (GUI-obstacle gui)) 32)) #t] ;; 168 = X + 68, 32 = X - 68, 68 = minimum distance between DORAEMON + pillar
    [else #f]))

;x1-detector : State -> Boolean
;  it check if the Doraemon is in range of obstacle1 (x-axis)

(define (x1-detector gui)
  (cond
    [(and (> 168 (posn-x (GUI-obstacle1 gui))) (> (posn-x (GUI-obstacle1 gui)) 32)) #t] ;; 168 = X + 68, 32 = X - 68, 68 = minimum distance between DORAEMON + pillar
    [else #f]))

;y-detector : State -> Boolean
; it check if the Doraemon is in range of obstacle (y-axis)

(define (y-detector gui)
  (cond
    [(and (> (GUI-current-state gui)
             (+ (posn-y (GUI-obstacle gui)) 254))               ;; 254 = Minimum distance between DORAEMON's center and pillar's center (y-axis) 
          (> (- (bottom-pillar gui) 254)
             (GUI-current-state gui)))#f]
    [else #t]))

;y1-detector : State -> Boolean
;  it check if the Doraemon is in range of obstacle1 (y-axis)

(define (y1-detector gui)
  (cond
    [(and (> (GUI-current-state gui)
             (+ (posn-y (GUI-obstacle1 gui)) 254))              ;; 254 = Minimum distance between DORAEMON's center and pillar's center (y-axis) 
          (> (- (bottom-pillar1 gui) 254)
             (GUI-current-state gui)))#f]
    [else #t]))

;; Example 

(check-expect (x-detector case-1) #f)
(check-expect (x-detector (make-GUI 300 (make-posn 150 100) (make-posn 300 500) #f 0 #f 0 #f I1)) #t)
(check-expect (x1-detector case-1) #f)
(check-expect (x1-detector (make-GUI 300 (make-posn 150 100) (make-posn 150 500) #f 0 #f 0 #f I1)) #t)
(check-expect (y-detector (make-GUI 300 (make-posn 150 20) (make-posn 150 20) #f 0 #f 0 #f I1)) #f)
(check-expect (y-detector case-2) #t)
(check-expect (y1-detector (make-GUI 300 (make-posn 150 20) (make-posn 150 20) #f 0 #f 0 #f I1)) #f)
(check-expect (y1-detector case-2) #t)

;________________________________________________________________________________________________

; Detector/collision system / tag: LOGIC

;; Data type
;collision-detection : State -> State
; it detect if the avatar collide with one pillar
;header : (define (collision-detector case-1) case-1)

;; Example

(check-expect (collision-detector case-1) case-1)
(check-expect (collision-detector case-2) case-2)
(check-expect (collision-detector case-3) case-3)
(check-expect (collision-detector (make-GUI 300 (make-posn 150 10) (make-posn 300 10) #f 0 #f 0 #f I1)) (make-GUI 300 (make-posn 150 10) (make-posn 300 10) #f 0 #f 0 #f I1))
(check-expect (collision-detector (make-GUI 300 (make-posn 150 10) (make-posn 150 10) #f 0 #f 0 #f I1)) (make-GUI 300 (make-posn 150 10) (make-posn 150 10) #f 0 #f 0 #f I1))
(check-expect (collision-detector (make-GUI 300 (make-posn 150 100) (make-posn 150 100) #f 0 #f 0 #f I1)) (make-GUI 300 (make-posn 150 100) (make-posn 150 100) #t 0 #t 0 #f I1))

; Code - Collision detection

(define (collision-detector gui)   ;; COMBINES ALL RESULT of x and y detector to determine if DORAEMON crashed
  (cond
    [(or (and (boolean=? #t (x-detector gui))
          (boolean=? #t (y-detector gui)))
         (and (boolean=? #t (x1-detector gui))
          (boolean=? #t (y1-detector gui))))
     (make-GUI (GUI-current-state gui) (GUI-obstacle gui) (GUI-obstacle1 gui) #t (GUI-score gui) #t (GUI-timer gui) #f (GUI-item gui))]
     [else gui]))

;________________________________________________________________________________________________

; Buff system / tag: LOGIC

;; Data type
; buff-collision : State -> State
; header : (define (buff-collision gui) ...)

(define case-7 (make-GUI 500 (make-posn 400 200) (make-posn 900 200) #f 0 #f 0 #f (make-ItemDetails (make-posn 100 400) 0 220 0)))


;; Example

(check-expect (buff-collision case-7) case-7)


;; Code - Buff collision

(define (x-buff gui)
  (cond
    [(and (> 130 (posn-x (ItemDetails-cord (GUI-item gui))))
          (> (posn-x (ItemDetails-cord (GUI-item gui))) 70)) #t]  ;; 70 = X - DISTANCE BETWEEN DORAEMON and Item (DORAYAKI)
    [else #f]))

(define (y-buff gui)
  (cond
    [(and (> (+ (GUI-current-state gui) 35) (posn-y (ItemDetails-cord (GUI-item gui))))
          (> (posn-y (ItemDetails-cord (GUI-item gui))) (- (GUI-current-state gui) 35))) #t]   ;; 35 = Minimum distance between DORAEMON and Item (DORAYAKI)
    [else #f]))

(define (buff-location gui)
  (cond
    [(> 0 (posn-x (ItemDetails-cord (GUI-item gui))))   ;; RESPAWNS Item when it's position reaches 0
     (make-GUI
      (GUI-current-state gui)
      (GUI-obstacle gui)
      (GUI-obstacle1 gui)
      (GUI-dead gui)
      (GUI-score gui)
      (GUI-game gui)
      (GUI-timer gui)
      (GUI-quit? gui)
      (make-ItemDetails
       (make-posn (random 1100 10000) (random 50 550))
       (ItemDetails-time (GUI-item gui))
       (ItemDetails-pillar (GUI-item gui))
       (random 1 3)))]
    [(and (boolean=? #t (x-buff gui))
          (boolean=? #t (y-buff gui)))  ;; CHANGES TIME COUNTER of ITEM to 1 from 0 so the ITEM's affect starts applying
     (make-GUI
      (GUI-current-state gui)
      (GUI-obstacle gui)
      (GUI-obstacle1 gui)
      (GUI-dead gui)
      (GUI-score gui)
      (GUI-game gui)
      (GUI-timer gui)
      (GUI-quit? gui)
      (make-ItemDetails
       (make-posn (random 1100 10000) (random 50 550))
       1
       (ItemDetails-pillar (GUI-item gui))
       (ItemDetails-type (GUI-item gui))))]
    [else gui]))


(define (buff-collision gui)
  (cond
    [(= (ItemDetails-time (GUI-item (buff-location gui))) 131)  ;; ITEM EFFECT IS RESET WHEN TIME COUNTER REACHES 0
     (make-GUI
      (GUI-current-state gui)
      (GUI-obstacle gui)
      (GUI-obstacle1 gui)
      (GUI-dead gui)
      (GUI-score gui)
      (GUI-game gui)
      (GUI-timer gui)
      (GUI-quit? gui)
      (make-ItemDetails
       (make-posn (random 1100 10000) (random 50 550))
       0
       220
       (random 1 3)))]
    [(and (and (> 131 (ItemDetails-time (GUI-item gui))) (> (ItemDetails-time (GUI-item gui)) 0)) (= (ItemDetails-type (GUI-item gui)) 1)) ;; SETS GAP TO 170 Pixel for 5 seconds
     (make-GUI
      (GUI-current-state gui)
      (GUI-obstacle gui)
      (GUI-obstacle1 gui)
      (GUI-dead gui)
      (GUI-score gui)
      (GUI-game gui)
      (GUI-timer gui)
      (GUI-quit? gui)
      (make-ItemDetails
       (make-posn (random 1100 10000) (random 50 550))
       (ItemDetails-time (GUI-item gui))
       170
       (ItemDetails-type (GUI-item gui))))]
    [(and (> 131 (ItemDetails-time (GUI-item (buff-location gui)))) (> (ItemDetails-time (GUI-item (buff-location gui))) 0) (= (ItemDetails-type (GUI-item gui)) 2))
     (make-GUI                                                                                                                            ;; SETS GAP TO 280 Pixel for 5 seconds
      (GUI-current-state gui)
      (GUI-obstacle gui)
      (GUI-obstacle1 gui)
      (GUI-dead gui)
      (GUI-score gui)
      (GUI-game gui)
      (GUI-timer gui)
      (GUI-quit? gui)
      (make-ItemDetails
       (make-posn (random 1100 10000) (random 50 550))
       (ItemDetails-time (GUI-item gui))
       270
       (ItemDetails-type (GUI-item gui))))]
    [else gui]))

;________________________________________________________________________________________________

; Pilar / tag: MECHANIC

;; Pilar Function
(define (pillars gui)  ;; RENDERS ALL THE PILLARS TO THE BACKGROUND
  (place-image TUBE-1 (posn-x (GUI-obstacle gui)) (posn-y (GUI-obstacle gui))
               (place-image TUBE-2 (posn-x (GUI-obstacle gui)) (bottom-pillar gui)
                            (place-image TUBE-1 (posn-x (GUI-obstacle1 gui)) (posn-y (GUI-obstacle1 gui))
                                         (place-image TUBE-2 (posn-x (GUI-obstacle1 gui)) (bottom-pillar1 gui)
                                                      (place-image (theme gui) 520 300 BACKGROUND))))))
;________________________________________________________________________________________________

;; Background of the game / tag: MECHANIC
;; Data type
;render : State -> Image
; It renders the game
;header : (define (render case-1) (loading-screen case-1))

;; Example

(check-expect (render case-1) (loading-screen case-1))
(check-expect (render case-2) (place-image (text/font "SCORE: 0000" 30 "black" #f "roman" 'slant 'normal #f) 850 90
              (place-image (character case-2) X (GUI-current-state case-2) (pillars case-2))))
(check-expect (render case-d1) (ending-screen case-d1))
(check-expect (render case-d2) (ending-screen case-d2))
(check-expect (render case-d3) (ending-screen case-d3))


; Code - Render
(define (render gui)  ;; TO-DRAW FUNCTION CONDITIONS
  (cond
    [(and (boolean=? (GUI-dead (collision-detector gui)) #f) (boolean=? (GUI-game gui) #f)) (loading-screen gui)]  ;; RENDERS STARTING SCREEN
    [(and (boolean=? (GUI-dead gui) #t) (boolean=? (GUI-game gui) #f)) (ending-screen gui)]                        ;; RENDERS GAME-OVER SCREEN
    [else (place-image (SCORE gui) 850 90                                                                          ;; RENDERS GAME SCREEN 
                       (place-image ITEM (posn-x (ItemDetails-cord (GUI-item gui))) (posn-y (ItemDetails-cord (GUI-item gui)))
                                    (place-image (character gui) X (GUI-current-state gui) (pillars gui))))]))
;________________________________________________________________________________________________

; Moving GUI / tag: LOGIC

;; Data  type

;tick  : State -> State
;it controls every non-static value in the game
; header : (define (tick case-1)


;  Example

(check-expect (tick case-17) (make-GUI 300
                              (make-posn 432 789)
                              (make-posn 637 399)
                              #false 1 #false 0 #false
                              (make-ItemDetails (make-posn 4000 340) 0 220 0)))
(check-expect (tick case-18) (make-GUI 306
                                       (make-posn 424 789)
                                       (make-posn 629 399)
                                       #false 1 #true 0 #false
                                       (make-ItemDetails (make-posn 3992 340)
                                                         0 220 0)))
(check-expect (tick case-19) (make-GUI 307
                              (make-posn 432 789)
                              (make-posn 637 399)
                              #true 0 #true 0 #false
                              (make-ItemDetails (make-posn 4000 340) 0 220 0)))
(check-expect (tick case-20) (make-GUI 300
                              (make-posn 432 789)
                              (make-posn 637 399)
                              #true 0 #false 1 #false
                              (make-ItemDetails (make-posn 4000 340) 0 220 0)))
(check-expect (tick case-22) (make-GUI 706
                                       (make-posn 492 100)
                                       (make-posn 992 100)
                                       #false 1 #true 0 #false
                                       (make-ItemDetails (make-posn 3992 340)
                                                         23 220 0)))

; Code - MOVING GUI
(define (tick gui)
  (cond
    [(and (and (boolean=? (GUI-dead (collision-detector gui)) #f) (boolean=? (GUI-game gui) #t))  ;; GAME MODE (WITH ITEM) (BEFORE COLLISION)
          (> 132 (ItemDetails-time (GUI-item (buff-location gui))) 0))
     (make-GUI
      (+ (GUI-current-state (respawn gui)) 6)
      (make-posn (- (posn-x (GUI-obstacle (respawn gui))) 8) (posn-y (GUI-obstacle (respawn gui))))
      (make-posn (- (posn-x (GUI-obstacle1 (respawn gui))) 8) (posn-y (GUI-obstacle1 (respawn gui))))
      #f
      (+ 1 (GUI-score gui))
      #t
      (GUI-timer gui)
      #f
      (make-ItemDetails (make-posn (- (posn-x (ItemDetails-cord (GUI-item (buff-location gui)))) 8) (posn-y (ItemDetails-cord (GUI-item (buff-location gui)))))
                        (+ 1 (ItemDetails-time (GUI-item (buff-collision gui))))
                        (ItemDetails-pillar (GUI-item (buff-collision gui)))
                        (ItemDetails-type (GUI-item (buff-collision gui)))))]
    [(and (boolean=? (GUI-dead (collision-detector gui)) #f) (boolean=? (GUI-game gui) #t) (= (ItemDetails-time (GUI-item gui)) 0)) ;;GAME MODE (WITHOUT ITEM) (BEFORE COLLISION)
     (make-GUI
      (+ (GUI-current-state (respawn gui)) 6)
      (make-posn (- (posn-x (GUI-obstacle (respawn gui))) 8) (posn-y (GUI-obstacle (respawn gui))))
      (make-posn (- (posn-x (GUI-obstacle1 (respawn gui))) 8) (posn-y (GUI-obstacle1 (respawn gui))))
      #f
      (+ 1 (GUI-score gui))
      #t
      (GUI-timer gui)
      #f
      (make-ItemDetails (make-posn (- (posn-x (ItemDetails-cord (GUI-item (buff-location gui)))) 8) (posn-y (ItemDetails-cord (GUI-item (buff-location gui)))))
                        (ItemDetails-time (GUI-item gui))
                        (ItemDetails-pillar (GUI-item gui))
                        (ItemDetails-type (GUI-item gui))))]
    [(and (boolean=? (GUI-dead (collision-detector gui)) #t) (boolean=? (GUI-game gui) #t))  ;; GAME MODE (AFTER COLLISION)
     (make-GUI
      (+ (GUI-current-state (respawn gui)) 7)
      (GUI-obstacle gui)
      (GUI-obstacle1 gui)
      #t
      (GUI-score gui)
      (GUI-game (end gui))
      (GUI-timer gui)
      #f
      (GUI-item gui))]
    [(and (boolean=? (GUI-dead (collision-detector gui)) #t) (boolean=? (GUI-game (end gui)) #f))  ;; STARTING SCREEN
     (make-GUI
      (GUI-current-state gui)
      (GUI-obstacle gui)
      (GUI-obstacle1 gui)
      #t
      (GUI-score gui)
      #f
      (+ 1 (GUI-timer (end gui)))
      #f
      (GUI-item gui))]
    [else                   ;; GAME-OVER SCREEN
     (make-GUI
      (GUI-current-state gui)
      (GUI-obstacle gui)
      (GUI-obstacle1 gui)
      #f
      (+ 1 (GUI-score gui))
      #f
      (GUI-timer gui)
      #f
      (GUI-item gui))]))
;________________________________________________________________________________________________

; BUTTON RESPONSE / tag: MECHANIC

;; Data type

;key-handler : State KeyEvent -> State
; it makes  the avatar  move in rensponse to the 'key' being pressed
;header : (define (key-handler gui KeyEvent) gui)

;; Example

(check-expect (key-handler case-17 "up") (make-GUI 210
                                                   (make-posn 432 789)
                                                   (make-posn 637 399)
                                                   #false 0 #false 0 #false
                                                   (make-ItemDetails (make-posn 4000 340) 0 220 0)))
(check-expect (key-handler case-17 "\r") (make-GUI 300
                                                   (make-posn 432 789)
                                                   (make-posn 637 399)
                                                   #false 0 #true 0 #false
                                                   (make-ItemDetails (make-posn 4000 340) 0 220 0)))
(check-expect (key-handler case-18 "\r") (make-GUI 300
                                                   (make-posn 432 789)
                                                   (make-posn 637 399)
                                                   #false 0 #true 0 #false
                                                   (make-ItemDetails
                                                    (make-posn 4000 340) 0 220 0)))
(check-expect (key-handler case-19 "q") (shared ((-1- (make-posn 432 789)))
                                          (make-GUI 300 -1- (make-posn 637 399) -1-
                                                    #true 0 0 #true
                                                    (make-ItemDetails (make-posn 4000 340)
                                                                      0 220 0))))

(check-expect (key-handler case-20 "up") (make-GUI 300
                                                   (make-posn 432 789)
                                                   (make-posn 637 399)
                                                   #true 0 #false 0 #false
                                                   (make-ItemDetails (make-posn 4000 340)
                                                                     0 220 0)))


; Code - Key-Pressed
(define (key-handler gui KeyEvent)
  (cond
    [(and (or (string=? "up" KeyEvent) (string=? " " KeyEvent)) (boolean=? (GUI-dead (collision-detector gui)) #f))  ;; BUTTON TO MAKE DORAEMON JUMP
     (make-GUI (- (GUI-current-state (respawn gui)) 90)
               (GUI-obstacle (respawn gui))
               (GUI-obstacle1 (respawn gui))
               #f
               (GUI-score (respawn gui))
               (GUI-game gui)
               (GUI-timer gui)
               #f
               (GUI-item gui))]
    [(and (string=? KeyEvent "\r") (boolean=? (GUI-dead gui) #f) (boolean=? (GUI-game gui) #f)) (make-GUI (GUI-current-state (respawn gui))  ;; MOVES TO GAME FROM
                                                                                                         (GUI-obstacle (respawn gui))        ;; STARTING SCREEN
                                                                                                         (GUI-obstacle1 (respawn gui))
                                                                                                         #f
                                                                                                         0
                                                                                                         #t
                                                                                                         (GUI-timer gui)
                                                                                                         #f
                                                                                                         (GUI-item gui))]
    [(string=? "q" KeyEvent) (make-GUI (GUI-current-state gui)  ;; STOPS GAME
                                       (GUI-obstacle gui)
                                       (GUI-obstacle1 gui)
                                       (GUI-obstacle gui)
                                       (GUI-game gui)
                                       (GUI-score gui)
                                       (GUI-timer gui)
                                       #t
                                       (GUI-item gui))]
    [else gui]))

;___________________________________________STARTING SCREEN_________________________________________
;; LOGOS - COSTANTS / tag: BACKGROUND

(define LOGO (scale (/ 1.2 1) (bitmap "logotitle.png")))

(define (msg gui)
  (text/font "Press 'ENTER' to play" 20 (col-val-msg gui)
        #f 'decorative 'normal 'light #t))

(define (tip gui)
  (text/font (string-append "Tip: " tips) 20 (col-val-msg gui)
        #f 'roman 'normal 'light #f))

;; Data type

; a List<String> is one of:
;  - '()                         ; empty list
;  - (cons String List<String>)  ; nonempty list
; interpretation: a list of Strings

(define TIPS (list "Press 'SPACE' or 'UP' to jump"
                   "You can click multiple times to jump higher"
                   "Avoid eating items as it will harden the game"
                   "HAVE FUN!!!"))


;________________________________________________________________________________________________

; Tips / tag: BACKGROUND
;; Data type

;tips : Number -> List<String>
; it takes a random number and it gives you the respective item of the list
;header (define tips) "")

;; Examples


;Random number 'z' 
(define Z (random 1 5))

(define tips ;; RANDOMLY CHOOSES ONE TIP FROM THE LIST
  (cond
    [(= Z 1) (first TIPS)]
    [(= Z 2) (second TIPS)]
    [(= Z 3) (third TIPS)]
    [else (fourth TIPS)]))

(define inline (ellipse 700 50 "solid" "white"))
(define outline (ellipse 700 50 "outline" "black"))


;; Data type
;bubble : State -> Image
;it take a state and it show you a message whith a figure in the background 'inline/outline'
(define (bubble gui)
  (place-image (tip gui) 350 25
               (place-image inline 350 25 outline)))  ;; TIPS AREA in STARTING SCREEN


;________________________________________________________________________________________________

;; Loading Screen / tag: BACKGROUND

;; Data type

;col-val-msg : State -> String
;it change the color of the message in the first page of the game
;header : (define (col-val-msg gui) "black")

; Example

(check-expect (col-val-msg case-2) "black")
(check-expect (col-val-msg (make-GUI 300 (make-posn 500 100) (make-posn 1000 100) #f 999 #t 0 #f I1)) "transparent")

; Code

(define (col-val-msg gui)  ;; SWTICHES COLOR OF MESSAGE BETWEEN BLACK AND TRANSPARENT
  (cond
    [(< (remainder (GUI-score gui) 40) 33) "black"]
    [else "transparent"]))


;________________________________________________________________________________________________

; Game finished system / tag: MECHANIC

;; Data type
;loading-screen : State -> Image
; it assemply all the image of the game
;header : (define (loading-screen gui) case-1)

;; Example

(check-expect (loading-screen case-1) (place-image LOGO 520 200
                                                   (place-image (msg case-1) 520 400
                                                                (place-image (bubble case-1) 520 500
                                                                             (place-image DORAEMON 100 160
                                                                                          (place-image QUIT 200 560
                                                                                                       (place-image DAY 520 300 BACKGROUND)))))))
;; Code - Loading-screen compiler
(define (loading-screen gui)  ;; STRUCTURE FOR LOADING SCREEN IN THE BEGINNING
  (place-image LOGO 520 200
               (place-image (msg gui) 520 400
                            (place-image (bubble gui) 520 500
                                         (place-image DORAEMON 100 160
                                                      (place-image QUIT 200 560
                                                                   (place-image DAY 520 300 BACKGROUND)))))))


;_________________________________________GAME-OVER SCREEN_______________________________________

; Game-over system / tag: LOGIC

;; Data type
;end : State -> State
;it stop the game when the avatar collide into a pillar
;header : (define (end gui) gui)

;; Example

(check-expect (end case-2) case-2)
(check-expect (end case-21) (make-GUI 700 (make-posn 500 100) (make-posn 1000 100)
 #true 0 #false 0 #false
 (make-ItemDetails (make-posn 4000 340) 0 220 0)))

;; Code - ENDING SCREEN
(define (end gui)
  (cond
    [(or (> (GUI-current-state gui) 600) (> 0 (GUI-current-state gui)))  ;; RENDERS ENDING SCREEN WHEN y value of DORAEMON is out of 0 to 600 range
     (make-GUI
      (GUI-current-state gui)
      (GUI-obstacle gui)
      (GUI-obstacle1 gui)
      #t
      (GUI-score gui)
      #f
      (GUI-timer gui)
      #f
      (GUI-item gui))]
    [else gui]))
;________________________________________________________________________________________________

; Scoring system / tag: MECHANICS

;; Data type

;game-over : State -> String
; it shows a different written based on your score
;header : (define (game-over gui) "")

;; Example

(check-expect (game-over case-d1) BANG)
(check-expect (game-over case-d2) BANG)
(check-expect (game-over case-d4) SKIT)


;; Code - Game-Over
(define BANG (scale (/ 2 1) (bitmap "bang.png")))
(define SKIT (scale (/ 2 1) (bitmap "skit.png")))
(define TRASH (scale (/ 2 1) (bitmap "trash.png")))
(define GJ (scale (/ 2 1) (bitmap "gj.png")))
(define TRY (scale (/ 2 1) (bitmap "try.png")))

(define (game-over gui)
  (cond
    [(and (boolean=? (GUI-dead (end gui)) #t) (boolean=? (GUI-game (end gui)) #f) (> 1000 (GUI-score gui))) ;; WHEN SCORE IS BELOW 1000, TAUNTS USER
     (cond
       [(> 10 (GUI-timer gui)) BANG]
       [(> 15 (GUI-timer gui)) ""]
       [(> 25 (GUI-timer gui)) BANG]
       [(> 30 (GUI-timer gui)) ""]
       [(> 40 (GUI-timer gui)) SKIT]
       [(> 45 (GUI-timer gui)) ""]
       [(> 55 (GUI-timer gui)) SKIT]
       [(> 75 (GUI-timer gui)) TRASH]
       [else (string-append "Score: " (number->string (GUI-score gui)))])]
    [(and (boolean=? (GUI-dead (end gui)) #t) (boolean=? (GUI-game (end gui)) #f) (>= 3000 (GUI-score gui) 1000)) ;; WHEN SCORE IS BETWEEN 1000 to 2000
     (cond                                                                                                        ;; ENCOURAGE USER
       [(> 10 (GUI-timer gui)) BANG]
       [(> 15 (GUI-timer gui)) ""]
       [(> 25 (GUI-timer gui)) BANG]
       [(> 30 (GUI-timer gui)) ""]
       [(> 40 (GUI-timer gui)) SKIT]
       [(> 45 (GUI-timer gui)) ""]
       [(> 55 (GUI-timer gui)) SKIT]
       [(> 75 (GUI-timer gui)) TRY]
       [else (string-append "Score: " (number->string (GUI-score gui)))])]
    [(and (boolean=? (GUI-dead (end gui)) #t) (boolean=? (GUI-game (end gui)) #f) (> (GUI-score gui) 3000))  ;; WHEN SCORE IS ABOVE 2000 COMPLIMENT USER
     (cond
       [(> 10 (GUI-timer gui)) BANG]
       [(> 15 (GUI-timer gui)) ""]
       [(> 25 (GUI-timer gui)) BANG]
       [(> 30 (GUI-timer gui)) ""]
       [(> 40 (GUI-timer gui)) SKIT]
       [(> 45 (GUI-timer gui)) ""]
       [(> 55 (GUI-timer gui)) SKIT]
       [(> 75 (GUI-timer gui)) GJ]
       [else (string-append "Score: " (number->string (GUI-score gui)))])]))

;________________________________________________________________________________________________

; IMAGE / WRITTEN - LOGIC
(define (col-retry gui)
  (if (> (remainder (GUI-timer gui) 20) 11) WHITE_RETRY RETRY))  ;; CONVERGES "RETRY" BUTTON BETWEEN BLACK AND WHITE COLOR

(define RETRY (bitmap "retry.png"))
(define WHITE_RETRY (bitmap "white_quit.png"))
(define BUT (place-image (rectangle 116 28 "solid" "transparent") 60 15 (rectangle 120 30 "solid" "transparent")))

(define (button gui) (place-image BUT 60 15 (col-retry gui)))  ;; RETRY BUTTON
;________________________________________________________________________________________________

; Ending screen / tag: MECHANIC

;; Data type

;ending screen gui -> Image
; records the progress 
(define (ending-screen gui)
     (place-image (if (string? (game-over gui)) (text/font (game-over gui) 45 "black" #f "roman" 'normal 'bold #f) (game-over gui)) 520 450
                  (place-image QUIT 200 560
                               (place-image END-SIGN 520 200
                                            (place-image (button gui) 960 560 DAY)))))


(define (handle-mouse gui x-mouse y-mouse mouse-event)
  (cond
    [(and (boolean=? (GUI-dead gui) #t) (boolean=? (GUI-game gui) #f) (string=? "button-down" mouse-event) (> 1020 x-mouse 900) (> 575 y-mouse 545))
     (make-GUI 300 (make-posn 500 (random -200 150)) (make-posn 1000 (random -200 150)) #f 0 #f 0 #f I1)]
    [else gui]))


;________________________________________________________________________________________________

; Quit / tag: LOGIC

;; Data type

; State -> State
; returns to the satrt

(define (QUIT? gui)
  (GUI-quit? gui))
;________________________________________________________________________________________________

;;; Trigger
(define (main gui)
  (big-bang gui
    [to-draw render]
    [on-tick tick]
    [on-key key-handler]
    [on-mouse handle-mouse]
    [stop-when QUIT?]))