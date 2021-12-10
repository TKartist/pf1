;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname model1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

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

(define (character gui)
  (cond
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
(define-struct GUI (current-state obstacle obstacle1 dead score game))


;; Example of GUI
(define case-1 (make-GUI 300 (make-posn 500 (random -200 150)) (make-posn 1000 (random -200 150)) #f 0 #f))

;; Score
(define (SCORE gui)
  (cond
    [(> 10 (GUI-score gui)) (text (string-append "000" (number->string (GUI-score gui))) 40 "black")]
    [(> 100 (GUI-score gui)) (text (string-append "00" (number->string (GUI-score gui))) 40 "black")]
    [(> 1000 (GUI-score gui)) (text (string-append "0" (number->string (GUI-score gui))) 40 "black")]
    [(= (remainder (quotient (GUI-score gui) 1000) 2) 0) (text (number->string (GUI-score gui)) 40 "black")] ;; Nightmode
    [(= (remainder (quotient (GUI-score gui) 1000) 2) 1) (text (number->string (GUI-score gui)) 40 "white")])) ;;Daymode

;; respawn of pillar
(define (respawn gui)
  (cond
    [(> 0 (posn-x (GUI-obstacle gui)))
     (make-GUI (GUI-current-state gui)
               (make-posn 1000 (random -200 150))
               (GUI-obstacle1 gui)
               #f
               (GUI-score gui)
               (GUI-game gui))]
    [(> 0 (posn-x (GUI-obstacle1 gui)))
     (make-GUI (GUI-current-state gui)
               (GUI-obstacle gui)
               (make-posn 1000 (random -200 150))
               #f
               (GUI-score gui)
               (GUI-game gui))]
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
     (make-GUI (GUI-current-state gui) (GUI-obstacle gui) (GUI-obstacle1 gui) #t (GUI-score gui) #t)]
     [else gui]))

(define (bottom-pillar gui)
  (+ (posn-y (GUI-obstacle gui)) 450 220))

(define (bottom-pillar1 gui)
  (+ (posn-y (GUI-obstacle1 gui)) 450 220))


;; render
(define (pillars gui)
  (place-image TUBE-1 (posn-x (GUI-obstacle gui)) (posn-y (GUI-obstacle gui))
                                         (place-image TUBE-2 (posn-x (GUI-obstacle gui)) (bottom-pillar gui)
                                                      (place-image TUBE-1 (posn-x (GUI-obstacle1 gui)) (posn-y (GUI-obstacle1 gui))
                                                                   (place-image TUBE-2 (posn-x (GUI-obstacle1 gui)) (bottom-pillar1 gui)
                                                                                (place-image (theme gui) 520 300 BACKGROUND))))))

(define (render gui)
  (cond
    [(boolean=? (GUI-game gui) #f) (loading-screen gui)]
    [else (place-image (SCORE gui) 850 60
               (place-image (character gui) x (GUI-current-state gui) (pillars gui)))]))


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
      #t)]
    [(and (boolean=? (GUI-dead (collision-detector gui)) #t) (boolean=? (GUI-game gui) #t))
     (make-GUI
      (+ (GUI-current-state (respawn gui)) 7)
      (GUI-obstacle gui)
      (GUI-obstacle1 gui)
      #t
      (GUI-score gui)
      #t)]
    [else
     (make-GUI
      (GUI-current-state gui)
      (GUI-obstacle gui)
      (GUI-obstacle1 gui)
      #f
      (+ 1 (GUI-score gui))
      #f)]))


;; Key-Pressed
(define (key-handler gui KeyEvent)
  (cond
    [(and (or (string=? "up" KeyEvent) (string=? " " KeyEvent)) (boolean=? (GUI-dead (collision-detector gui)) #f))
         (make-GUI (- (GUI-current-state (respawn gui)) 90)
                   (GUI-obstacle (respawn gui))
                   (GUI-obstacle1 (respawn gui))
                   #f (GUI-score (respawn gui))
                   (GUI-game gui))]
    [(and (string=? KeyEvent "\r") (boolean=? (GUI-game gui) #f)) (make-GUI (GUI-current-state (respawn gui))
                                           (GUI-obstacle (respawn gui))
                                           (GUI-obstacle1 (respawn gui))
                                           #f
                                           0
                                           #t)]
    [else gui]))

;; END WHEN?
(define (end gui)
  (cond
    [(or (> (GUI-current-state gui) 600) (> 0 (GUI-current-state gui))) #t]
    [else #f]))

(define (end? gui)
  (end gui))


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


;; Loading Screen

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

(define (LOGO gui)
  (beside (text "SAVE" 60 (col-val-logo gui)) LOGO-2))

(define LOGO-2
  (text " DORAEMON" 60 "royal blue"))


(define (msg gui)
  (text "press 'enter' to play" 20 (col-val-msg gui)))

(define (loading-screen gui)
  (place-image (LOGO gui) 520 200
               (place-image (msg gui) 520 400
                            (place-image DORAEMON 225 200
                                         (place-image day 520 300 BACKGROUND)))))



;;; Trigger
(define (main gui)
  (big-bang gui
    [to-draw render]
    [on-tick tick]
    [on-key key-handler]
    [stop-when end?]))