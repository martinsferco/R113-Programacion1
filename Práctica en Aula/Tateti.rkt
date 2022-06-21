;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Tateti) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;Definimos una estructura que nos determina el estado de cada una de las celdas
(define-struct Board-state [x1y1 x2y1 x3y1
                            x1y2 x2y2 x3y2
                            x1y3 x2y3 x3y3 turn])

(define ESCENE (empty-scene 600 600))

(define LINE (rectangle 600 10 "solid" "black"))

;Definimos el tablero de juego
(define GAME-SCENE (place-image LINE 300 200
                                  (place-image LINE 300 400
                                               (place-image (rotate 90 LINE) 200 300
                                                            (place-image (rotate 90 LINE) 400 300
                                                                         ESCENE)))))
;Definimos las fichas de los jugadores
(define PLAYER1 (circle 70 "solid" "lightblue"))

(define PLAYER2 (circle 70 "solid" "lightgreen"))

(define NOPLAYER (circle 70 "solid" "white"))

(define INITIAL-GAME (make-Board-state 0 0 0
                                       0 0 0
                                       0 0 0 1))


;ficha: Board-state Number Number -> Image
(define (ficha game x y)
  (cond [(= x 1) (cond [(= y 1) (if (= 0 (Board-state-x1y1 game)) NOPLAYER (if (= 1 (Board-state-x1y1 game)) PLAYER1 PLAYER2))]
                       [(= y 2) (if (= 0 (Board-state-x1y2 game)) NOPLAYER (if (= 1 (Board-state-x1y2 game)) PLAYER1 PLAYER2))]
                       [(= y 3) (if (= 0 (Board-state-x1y3 game)) NOPLAYER (if (= 1 (Board-state-x1y3 game)) PLAYER1 PLAYER2))])]
        [(= x 2)(cond  [(= y 1) (if (= 0 (Board-state-x2y1 game)) NOPLAYER (if (= 1 (Board-state-x2y1 game)) PLAYER1 PLAYER2))]
                       [(= y 2) (if (= 0 (Board-state-x2y2 game)) NOPLAYER (if (= 1 (Board-state-x2y2 game)) PLAYER1 PLAYER2))]
                       [(= y 3) (if (= 0 (Board-state-x2y3 game)) NOPLAYER (if (= 1 (Board-state-x2y3 game)) PLAYER1 PLAYER2))])]
        [else   (cond  [(= y 1) (if (= 0 (Board-state-x3y1 game)) NOPLAYER (if (= 1 (Board-state-x3y1 game)) PLAYER1 PLAYER2))]
                       [(= y 2) (if (= 0 (Board-state-x3y2 game)) NOPLAYER (if (= 1 (Board-state-x3y2 game)) PLAYER1 PLAYER2))]
                       [(= y 3) (if (= 0 (Board-state-x3y3 game)) NOPLAYER (if (= 1 (Board-state-x3y3 game)) PLAYER1 PLAYER2))])])
  )

;game-render: Board-state -> Image
(define (game-render game)
  (place-image (ficha game 1 1) 100 100
               (place-image (ficha game 1 2) 100 300
                            (place-image (ficha game 1 3) 100 500
                                         (place-image (ficha game 2 1) 300 100
                                                      (place-image (ficha game 2 2) 300 300
                                                                   (place-image (ficha game 2 3) 300 500
                                                                                (place-image (ficha game 3 1) 500 100
                                                                                             (place-image (ficha game 3 2) 500 300
                                                                                                          (place-image (ficha game 3 3) 500 500
                                                                                                                       GAME-SCENE)))))))))
  )

                       

;change-turn: Number -> Number
(define (change-turn t)
  (if (= t 1) 2 1)
  )

;create-board: Board-state Number Number Number
(define (create-board game x y turn)
  (cond [(= x 1) (cond [(= y 1) (make-Board-state turn (Board-state-x2y1 game) (Board-state-x3y1 game)
                                                  (Board-state-x1y2 game) (Board-state-x2y2 game) (Board-state-x3y2 game)
                                                  (Board-state-x1y3 game) (Board-state-x2y3 game) (Board-state-x3y3 game) (change-turn (Board-state-turn game)))]
                       
                       [(= y 2) (make-Board-state (Board-state-x1y1 game) (Board-state-x2y1 game) (Board-state-x3y1 game)
                                                  turn (Board-state-x2y2 game) (Board-state-x3y2 game)
                                                  (Board-state-x1y3 game) (Board-state-x2y3 game) (Board-state-x3y3 game) (change-turn (Board-state-turn game)))]
                       
                       [(= y 3) (make-Board-state (Board-state-x1y1 game) (Board-state-x2y1 game) (Board-state-x3y1 game)
                                                  (Board-state-x1y2 game) (Board-state-x2y2 game) (Board-state-x3y2 game)
                                                  turn (Board-state-x2y3 game) (Board-state-x3y3 game) (change-turn (Board-state-turn game)))])]
        
        [(= x 2)(cond  [(= y 1) (make-Board-state (Board-state-x1y1 game) turn (Board-state-x3y1 game)
                                                  (Board-state-x1y2 game) (Board-state-x2y2 game) (Board-state-x3y2 game)
                                                  (Board-state-x1y3 game) (Board-state-x2y3 game) (Board-state-x3y3 game) (change-turn (Board-state-turn game)))]
                       
                       [(= y 2) (make-Board-state (Board-state-x1y1 game) (Board-state-x2y1 game) (Board-state-x3y1 game)
                                                  (Board-state-x1y2 game) turn (Board-state-x3y2 game)
                                                  (Board-state-x1y3 game) (Board-state-x2y3 game) (Board-state-x3y3 game) (change-turn (Board-state-turn game)))]
                       
                       [(= y 3) (make-Board-state (Board-state-x1y1 game) (Board-state-x2y1 game) (Board-state-x3y1 game)
                                                  (Board-state-x1y2 game) (Board-state-x2y2 game) (Board-state-x3y2 game)
                                                  (Board-state-x1y3 game) turn (Board-state-x3y3 game) (change-turn (Board-state-turn game)))])]
        
        [else   (cond  [(= y 1) (make-Board-state (Board-state-x1y1 game) (Board-state-x2y1 game) turn
                                                  (Board-state-x1y2 game) (Board-state-x2y2 game) (Board-state-x3y2 game)
                                                  (Board-state-x1y3 game) (Board-state-x2y3 game) (Board-state-x3y3 game) (change-turn (Board-state-turn game)))]
                       
                       [(= y 2) (make-Board-state (Board-state-x1y1 game) (Board-state-x2y1 game) (Board-state-x3y1 game)
                                                  (Board-state-x1y2 game) (Board-state-x2y2 game) turn
                                                  (Board-state-x1y3 game) (Board-state-x2y3 game) (Board-state-x3y3 game) (change-turn (Board-state-turn game)))]
                       
                       [(= y 3) (make-Board-state (Board-state-x1y1 game) (Board-state-x2y1 game) (Board-state-x3y1 game)
                                                  (Board-state-x1y2 game) (Board-state-x2y2 game) (Board-state-x3y2 game)
                                                  (Board-state-x1y3 game) (Board-state-x2y3 game) turn (change-turn (Board-state-turn game)))])])
  )


;mouse-controler: Board-state Number Number String -> Board-state
(define (mouse-controler game x y s)
  (if (not (string=? s "button-down")) game
      (cond [(< x 200) (cond [(< y 200) (if (not (= 0 (Board-state-x1y1 game))) game (create-board game 1 1 (Board-state-turn game)))]
                             [(< y 400) (if (not (= 0 (Board-state-x1y2 game))) game (create-board game 1 2 (Board-state-turn game)))]
                             [else      (if (not (= 0 (Board-state-x1y3 game))) game (create-board game 1 3 (Board-state-turn game)))])]
            
            [(< x 400) (cond [(< y 200) (if (not (= 0 (Board-state-x2y1 game))) game (create-board game 2 1 (Board-state-turn game)))]
                             [(< y 400) (if (not (= 0 (Board-state-x2y2 game))) game (create-board game 2 2 (Board-state-turn game)))]
                             [else      (if (not (= 0 (Board-state-x2y3 game))) game (create-board game 2 3 (Board-state-turn game)))])]
            
            [else      (cond [(< y 200) (if (not (= 0 (Board-state-x3y1 game))) game (create-board game 3 1 (Board-state-turn game)))]
                             [(< y 400) (if (not (= 0 (Board-state-x3y2 game))) game (create-board game 3 2 (Board-state-turn game)))]
                             [else      (if (not (= 0 (Board-state-x3y3 game))) game (create-board game 3 3 (Board-state-turn game)))])]))
  )


;check-columns: Board-state -> Boolean
(define (check-columns game)
  (or (and (not (= 0 (Board-state-x1y1 game))) (= (Board-state-x1y1 game) (Board-state-x1y2 game) (Board-state-x1y3 game)))
      (and (not (= 0 (Board-state-x2y1 game))) (= (Board-state-x2y1 game) (Board-state-x2y2 game) (Board-state-x2y3 game)))
      (and (not (= 0 (Board-state-x3y1 game))) (= (Board-state-x3y1 game) (Board-state-x3y2 game) (Board-state-x3y3 game))))
  )

;check-rows: Board-state -> Boolean
(define (check-rows game)
  (or (and (not (= 0 (Board-state-x1y1 game))) (= (Board-state-x1y1 game) (Board-state-x2y1 game) (Board-state-x3y1 game)))
      (and (not (= 0 (Board-state-x1y2 game))) (= (Board-state-x1y2 game) (Board-state-x2y2 game) (Board-state-x3y2 game)))
      (and (not (= 0 (Board-state-x1y3 game))) (= (Board-state-x1y3 game) (Board-state-x2y3 game) (Board-state-x3y3 game))))
  )

;check-diagonals: Board-state -> Boolean
(define (check-diagonals game)
  (or (and (not (= 0 (Board-state-x1y1 game))) (= (Board-state-x1y1 game) (Board-state-x2y2 game) (Board-state-x3y3 game)))
      (and (not (= 0 (Board-state-x1y3 game))) (= (Board-state-x1y3 game) (Board-state-x2y2 game) (Board-state-x3y1 game))))
  )

;check-winner: Board-state -> Boolean
(define (check-winner game)
  (or (check-columns game) (check-rows game) (check-diagonals game)))

(define (winner-msg game)
  (overlay (text (string-append "Ganó el jugador " (number->string (change-turn (Board-state-turn game))))
                 70 "black")
           ESCENE))


(big-bang INITIAL-GAME
  [to-draw game-render]
  [on-mouse mouse-controler]
  [stop-when check-winner winner-msg]
  )










