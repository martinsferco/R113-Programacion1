;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname PingPong) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define-struct Game [p1 p2 x y vx vy])

(define ESCENE (empty-scene 100 600))

(define PLAYER (rectangle 10 50 "solid" "black"))

(define PLAYER-MOVEMENT 2)

(define BALL-VELOCITY)


;render-game: Game -> Image
(define (render-game game))



;keyboard-controler: Game String -> Game
(define (keyboard-controler game k)
  (cond [(string=? k "up") (make-Game (Game-p1 game) (- (Game-p2 game) PLAYER-MOVEMENT))]
        [(string=? k "down")]

        [(string=? k "w")
        [(string=? k "s")]])
  )