;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname model2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

;;;;;;;;;;**********          Programming Fundamentals Group Project          **********;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*****     SAVE DORAEMON     *****;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;**********     Hun Rim, George Batyrev, Giorgio Bonetto, Nicola Fontana   **********;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     VERSION 1.0    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; LIBRARIES UTILIZED


(require 2htdp/universe)
(require 2htdp/image)

;; BACKGROUND
(define WIDTH 1040)
(define HEIGHT 600)
(define BACKGROUND (empty-scene WIDTH HEIGHT))


;; DORAEMON
(define DORAEMON (bitmap "doraemon.png"))  ;; image name: doraemon.png
(define crash (bitmap "crash.png"))
(define easter (bitmap "superdo.png"))
(define end-sign (scale (/ 2 1) (bitmap "game-over.png")))
(define ITEM (bitmap "dora.png"))

(define (character gui)
  (cond
    [(and (>= (GUI-score gui) 2112) (>= 2205 (GUI-score gui))) easter] ;; Easter Egg doraemon
    [(boolean=? (GUI-dead gui) #f) DORAEMON]
    [else crash]))

;; BACKGROUND IMAGE
(define night (bitmap "night.jpeg")) ;; image name: background.jpeg
(define day (bitmap "day.jpeg"))     ;; image name: daymode.jpeg

(define (theme gui)
  (cond
    [(= (remainder (quotient (GUI-score gui) 1000) 2) 0) day]  ;; Nightmode
    [(= (remainder (quotient (GUI-score gui) 1000) 2) 1) night])) ;;Daymode


;Starting x-value for DORAEMON
(define x 100) 


;;Graphical User Interface (GUI) deatail
(define-struct GUI (current-state obstacle obstacle1 dead score game timer quit? item))
(define-struct ItemDetails (cord time pillar type))
(define I1 (make-ItemDetails (make-posn (random 1010 10000) (random 50 550)) 0 220 0))
;; Example of GUI
(define case-1 (make-GUI 300 (make-posn 500 (random -200 150)) (make-posn 1000 (random -200 150)) #f 0 #f 0 #f I1))

;; Score
(define (SCORE gui)
  (cond
    [(> 10 (GUI-score gui)) (text/font (string-append "SCORE: 000" (number->string (GUI-score gui)))
                                       30 "black" #f 'roman 'slant 'normal #f)]
    [(> 100 (GUI-score gui)) (text/font (string-append "SCORE: 00" (number->string (GUI-score gui)))
                                       30 "black" #f 'roman 'slant 'normal #f)]
    [(> 1000 (GUI-score gui)) (text/font (string-append "SCORE: 0" (number->string (GUI-score gui)))
                                       30 "black" #f 'roman 'slant 'normal #f)]
    [(= (remainder (quotient (GUI-score gui) 1000) 2) 0) (text/font (string-append "SCORE: " (number->string (GUI-score gui)))
                                                                              30 "black" #f "roman" 'slant 'normal #f)]   ;; Nightmode
    [(= (remainder (quotient (GUI-score gui) 1000) 2) 1) (text/font (string-append "SCORE: " (number->string (GUI-score gui)))
                                                                              30 "white" #f "roman" 'slant 'normal #f)]))   ;;Daymode

;; respawn of pillar
(define (respawn gui)
  (cond
    [(> 0 (posn-x (GUI-obstacle gui)))
     (make-GUI (GUI-current-state gui)
               (make-posn 1000 (random -200 150))
               (GUI-obstacle1 gui)
               #f
               (GUI-score gui)
               (GUI-game gui)
               (GUI-timer gui)
               #f
               (GUI-item gui))]
    [(> 0 (posn-x (GUI-obstacle1 gui)))
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

;; Check if the Doraemon is in range of the range of pillars (x-axis)
(define (x-detector gui)
  (cond
    [(and (> 168 (posn-x (GUI-obstacle gui))) (> (posn-x (GUI-obstacle gui)) 32)) #t];; 390= x+90, 210=x-90, 90= width of DORAEMON + width of pillar
    [else #f]))

(define (x1-detector gui)
  (cond
    [(and (> 168 (posn-x (GUI-obstacle1 gui))) (> (posn-x (GUI-obstacle1 gui)) 32)) #t];; 390= x+90, 210=x-90, 90= width of DORAEMON + width of pillar
    [else #f]))

;; Check if the Doraemon is in range of the range of pillars (y-axis)
(define (y-detector gui)
  (cond
    [(and (> (GUI-current-state gui)
             (+ (posn-y (GUI-obstacle gui)) 254))
          (> (- (bottom-pillar gui) 254)
             (GUI-current-state gui)))#f]
    [else #t]))

(define (y1-detector gui)
  (cond
    [(and (> (GUI-current-state gui)
             (+ (posn-y (GUI-obstacle1 gui)) 254))
          (> (- (bottom-pillar1 gui) 254)
             (GUI-current-state gui)))#f]
    [else #t]))


;; Collision detection
(define (collision-detector gui)
  (cond
    [(or (and (boolean=? #t (x-detector gui))
          (boolean=? #t (y-detector gui)))
         (and (boolean=? #t (x1-detector gui))
          (boolean=? #t (y1-detector gui))))
     (make-GUI (GUI-current-state gui) (GUI-obstacle gui) (GUI-obstacle1 gui) #t (GUI-score gui) #t (GUI-timer gui) #f (GUI-item gui))]
     [else gui]))

(define (bottom-pillar gui)
  (+ (posn-y (GUI-obstacle gui)) 450 (ItemDetails-pillar (GUI-item (buff-collision gui)))))

(define (bottom-pillar1 gui)
  (+ (posn-y (GUI-obstacle1 gui)) 450 (ItemDetails-pillar (GUI-item (buff-collision gui)))))


;;; Buff collision

(define (buff-collision gui)
  (cond
    [(not (and (> 138 (posn-x (ItemDetails-cord (GUI-item gui))))
          (> (posn-x (ItemDetails-cord (GUI-item gui))) 62)
          (> (GUI-current-state gui) (+ (posn-y (ItemDetails-cord (GUI-item gui))) 47))
          (> (- (posn-y (ItemDetails-cord (GUI-item gui))) 47) (GUI-current-state gui))))
     (cond
       [(and (= (ItemDetails-type (GUI-item gui)) 1) (> 131 (ItemDetails-time (GUI-item gui))))
        (make-GUI (GUI-current-state gui)
                  (GUI-obstacle gui)
                  (GUI-obstacle1 gui)
                  (GUI-dead gui)
                  (GUI-score gui)
                  (GUI-game gui)
                  (GUI-timer gui)
                  (GUI-quit? gui)
                  (make-ItemDetails (make-posn (random 2000 10000) (random 50 550))
                             (ItemDetails-time (GUI-item gui))
                             270
                             (ItemDetails-type (GUI-item gui))))] ;; BUFF
       [(and (= (ItemDetails-type (GUI-item gui)) 2) (> 131 (ItemDetails-time (GUI-item gui))))
        (make-GUI (GUI-current-state gui)
                  (GUI-obstacle gui)
                  (GUI-obstacle1 gui)
                  (GUI-dead gui)
                  (GUI-score gui)
                  (GUI-game gui)
                  (GUI-timer gui)
                  (GUI-quit? gui)
                  (make-ItemDetails (make-posn (random 2000 10000) (random 50 550))
                             (ItemDetails-time (GUI-item gui))
                             170
                             (ItemDetails-type (GUI-item gui))))] ;; NERF
       [else (make-GUI (GUI-current-state gui)
                       (GUI-obstacle gui)
                       (GUI-obstacle1 gui)
                       (GUI-dead gui)
                       (GUI-score gui)
                       (GUI-game gui)
                       (GUI-timer gui)
                       (GUI-quit? gui)
                       (make-ItemDetails (make-posn (random 2000 10000) (random 50 550))
                                  0
                                  220
                                  (random 1 3)))])] ;; NORMAL
    [else gui]))


;;; render
(define quit (text/font "Press 'Q' to quit" 20 "black" #f 'roman 'normal 'bold #t))
  
(define (pillars gui)
  (place-image TUBE-1 (posn-x (GUI-obstacle gui)) (posn-y (GUI-obstacle gui))
               (place-image TUBE-2 (posn-x (GUI-obstacle gui)) (bottom-pillar gui)
                            (place-image TUBE-1 (posn-x (GUI-obstacle1 gui)) (posn-y (GUI-obstacle1 gui))
                                         (place-image TUBE-2 (posn-x (GUI-obstacle1 gui)) (bottom-pillar1 gui)
                                                      (place-image (theme gui) 520 300 BACKGROUND))))))

(define (render gui)
  (cond
    [(and (boolean=? (GUI-dead (collision-detector gui)) #f) (boolean=? (GUI-game gui) #f)) (loading-screen gui)]
    [(and (boolean=? (GUI-dead gui) #t) (boolean=? (GUI-game gui) #f)) (ending-screen gui)]
    [else (place-image (SCORE gui) 850 90
                       (place-image ITEM (posn-x (ItemDetails-cord (GUI-item gui))) (posn-y (ItemDetails-cord (GUI-item gui)))
                                    (place-image (character gui) x (GUI-current-state gui) (pillars gui))))]))


;; MOVING DORAEMON UP AND DOWN
(define (tick gui)
  (cond
    [(and (boolean=? (GUI-dead (collision-detector gui)) #f) (boolean=? (GUI-game gui) #t))
     (make-GUI
      (+ (GUI-current-state (respawn gui)) 6)
      (make-posn (- (posn-x (GUI-obstacle (respawn gui))) 8) (posn-y (GUI-obstacle (respawn gui))))
      (make-posn (- (posn-x (GUI-obstacle1 (respawn gui))) 8) (posn-y (GUI-obstacle1 (respawn gui))))
      #f
      (+ 1 (GUI-score gui))
      #t
      (GUI-timer gui)
      #f
      (make-ItemDetails (make-posn (- (posn-x (ItemDetails-cord (GUI-item gui))) 8) (posn-y (ItemDetails-cord (GUI-item gui))))
                        (ItemDetails-pillar (GUI-item gui))
                        (+ 1 (ItemDetails-time (GUI-item gui)))
                        (ItemDetails-type (GUI-item gui))))]
    [(and (boolean=? (GUI-dead (collision-detector gui)) #t) (boolean=? (GUI-game gui) #t))
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
    [(and (boolean=? (GUI-dead (collision-detector gui)) #t) (boolean=? (GUI-game (end gui)) #f))
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
    [else ;; Crash Mode
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


;; Key-Pressed
(define (key-handler gui KeyEvent)
  (cond
    [(and (or (string=? "up" KeyEvent) (string=? " " KeyEvent)) (boolean=? (GUI-dead (collision-detector gui)) #f))
     (make-GUI (- (GUI-current-state (respawn gui)) 90)
               (GUI-obstacle (respawn gui))
               (GUI-obstacle1 (respawn gui))
               #f
               (GUI-score (respawn gui))
               (GUI-game gui)
               (GUI-timer gui)
               #f
               (GUI-item gui))]
    [(and (string=? KeyEvent "\r") (boolean=? (GUI-dead gui) #f) (boolean=? (GUI-game gui) #f)) (make-GUI (GUI-current-state (respawn gui))
                                                                                                         (GUI-obstacle (respawn gui))
                                                                                                         (GUI-obstacle1 (respawn gui))
                                                                                                         #f
                                                                                                         0
                                                                                                         #t
                                                                                                         (GUI-timer gui)
                                                                                                         #f
                                                                                                         (GUI-item gui))]
    [(string=? "q" KeyEvent) (make-GUI (GUI-current-state gui)
                                       (GUI-obstacle gui)
                                       (GUI-obstacle1 gui)
                                       (GUI-obstacle gui)
                                       (GUI-game gui)
                                       (GUI-score gui)
                                       (GUI-timer gui)
                                       #t
                                       (GUI-item gui))]
    [else gui]))


;;; Loading Screen

;; Color wave for titles
(define (col-val-logo gui)
  (cond
    [(< (remainder (GUI-score gui) 56) 8) "medium red"]
    [(< (remainder (GUI-score gui) 56) 16) "light red"]
    [(< (remainder (GUI-score gui) 56) 24) "dark purple"]
    [(< (remainder (GUI-score gui) 56) 32) "light purple"]
    [(< (remainder (GUI-score gui) 56) 40) "medium orange"]
    [(< (remainder (GUI-score gui) 56) 48) "light orange"]
    [else "medium cyan"]))

(define (col-val-msg gui)
  (cond
    [(< (remainder (GUI-score gui) 40) 33) "black"]
    [else "transparent"]))


;; LOGOS
(define (LOGO gui)
  (beside (text/font "SAVE" 60 (col-val-logo gui)
                     #f 'roman 'italic 'bold #f) LOGO-2))

(define LOGO-2
  (text/font " DORAEMON" 60 "navy"
        #f 'roman 'normal 'bold #f))


(define (msg gui)
  (text/font "Press 'ENTER' to play" 20 (col-val-msg gui)
        #f 'decorative 'normal 'light #t))

(define (tip gui)
  (text/font (string-append "Tip: " tips) 20 (col-val-msg gui)
        #f 'roman 'normal 'light #f))

(define TIPS (list "Press 'SPACE' or 'UP' to jump"
                   "You can click multiple times to jump higher"
                   "Take buffs to make the pillar gaps bigger"
                   "Avoid nerfs which decreases the pillar gaps"
                   "HAVE FUN!!!"))

(define z (random 1 6))

(define tips
  (cond
    [(= z 1) (first TIPS)]
    [(= z 2) (second TIPS)]
    [(= z 3) (third TIPS)]
    [(= z 4) (fourth TIPS)]
    [else (fifth TIPS)]))

(define inline (ellipse 700 50 "solid" "white"))
(define outline (ellipse 700 50 "outline" "black"))
(define (bubble gui)
  (place-image (tip gui) 350 25
               (place-image inline 350 25 outline)))


;; Loading-screen compiler
(define (loading-screen gui)
  (place-image (LOGO gui) 520 200
               (place-image (msg gui) 520 400
                            (place-image (bubble gui) 520 500
                                         (place-image DORAEMON 170 200
                                                      (place-image quit 100 560
                                                                   (place-image day 520 300 BACKGROUND)))))))



;;; ENDING SCREEN
(define (end gui)
  (cond
    [(or (> (GUI-current-state gui) 600) (> 0 (GUI-current-state gui)))
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

(define (game-over gui)
  (cond
    [(and (boolean=? (GUI-dead (end gui)) #t) (boolean=? (GUI-game (end gui)) #f) (> 1000 (GUI-score gui)))
     (cond
       [(> 10 (GUI-timer gui)) "BANG!!!"]
       [(> 15 (GUI-timer gui)) ""]
       [(> 25 (GUI-timer gui)) "BANG!!!"]
       [(> 30 (GUI-timer gui)) ""]
       [(> 40 (GUI-timer gui)) "SKIT!!!"]
       [(> 45 (GUI-timer gui)) ""]
       [(> 55 (GUI-timer gui)) "SKIT!!!"]
       [(> 70 (GUI-timer gui)) "TRASHHH!!!!"]
       [else (string-append "Score: " (number->string (GUI-score gui)))])]
    [(and (boolean=? (GUI-dead (end gui)) #t) (boolean=? (GUI-game (end gui)) #f) (> (GUI-score gui) 1000))
     (cond
       [(> 10 (GUI-timer gui)) "BANG!!!"]
       [(> 15 (GUI-timer gui)) ""]
       [(> 25 (GUI-timer gui)) "BANG!!!"]
       [(> 30 (GUI-timer gui)) ""]
       [(> 40 (GUI-timer gui)) "SKIT!!!"]
       [(> 45 (GUI-timer gui)) ""]
       [(> 55 (GUI-timer gui)) "SKIT!!!"]
       [(> 70 (GUI-timer gui)) "Try HARDER!!!"]
       [else (string-append "Score: " (number->string (GUI-score gui)))])]
    [(and (boolean=? (GUI-dead (end gui)) #t) (boolean=? (GUI-game (end gui)) #f) (> (GUI-score gui) 3000))
     (cond
       [(> 10 (GUI-timer gui)) "BANG!!!"]
       [(> 15 (GUI-timer gui)) ""]
       [(> 25 (GUI-timer gui)) "BANG!!!"]
       [(> 30 (GUI-timer gui)) ""]
       [(> 40 (GUI-timer gui)) "SKIT!!!"]
       [(> 45 (GUI-timer gui)) ""]
       [(> 55 (GUI-timer gui)) "SKIT!!!"]
       [(> 70 (GUI-timer gui)) "GOOD JOB!!!"]
       [else (string-append "Score: " (number->string (GUI-score gui)))])]))

(define retry (text/font "RETRY? :)" 20 "black" #f 'roman 'slant 'bold #f))

(define but (place-image (rectangle 116 28 "solid" "snow") 60 15 (rectangle 120 30 "solid" "black")))
(define button (place-image retry 60 15 but))

(define (ending-screen gui)
  (place-image (text/font (game-over gui) 45 "black" #f "roman" 'normal 'bold #f) 520 450
               (place-image quit 100 560
                            (place-image end-sign 520 200
                                         (place-image button 960 530 day)))))


;;;TUBES

;; UPPER TUBE
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


;; LOWER TUBE
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


;;; QUIT?
(define (QUIT? gui)
  (GUI-quit? gui))

;;; Trigger
(define (main gui)
  (big-bang gui
    [to-draw render]
    [on-tick tick]
    [on-key key-handler]
    [stop-when QUIT?]))