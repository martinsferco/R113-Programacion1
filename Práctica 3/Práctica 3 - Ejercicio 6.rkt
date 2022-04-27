;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Práctica 3 - Ejercicio 6|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;__________________________________________
;LICENCIATURA EN CIENCIAS DE LA COMPUTACIÓN
;PROGRAMACIÓN I - 2022
;PRÁCTICA 3 - Ejercicio 6

;Martín Sferco
;Comisión 1 - Grupo 7
;__________________________________________

;El Estado será un String y representará la cadena de caracteres
;que hayamos escrito en ese momento.

(define ANCHO-ESCENA 800)
(define ALTO-ESCENA 60)

(define SIZE-FONT 20)
(define COLOR-FONT "indigo")


;muestra-texto: Estado -> Image
;Recibe el estado actual del sistema y la tecla presionada,
;y nos devuelve una imagen con el texto correspondiente al estado.

(define (muestra-texto estado)
  (place-image/align (text estado SIZE-FONT COLOR-FONT)
             10 (/ ALTO-ESCENA 2) "left" "center"
             (empty-scene ANCHO-ESCENA ALTO-ESCENA))
  )

;remover-ultimo: String -> String
;Dado un estado, nos devuelve el el estado con el último caracter removido.

(define (remover-ultimo estado)
  (substring estado 0 (- (string-length estado) 1) )
  )


;teclear: Estado Tecla -> Estado
;Dado el estado actual y la tecla presionada, devuelve un Estado nuevo, con la tecla
;presionada, adicionada al final.

(check-expect (teclear " " "\b") "")
(check-expect (teclear "" "\b") "")
(check-expect (teclear "A " "\b") "A")
(check-expect (teclear "hola" "\b") "hol")
(check-expect (teclear "hola" "a") "holaa")
(check-expect (teclear "hola" "A ") "holaA ")

(define (teclear estado tecla)
  (cond [(string=? tecla "\b") (if (= 0 (string-length estado))
                                   estado
                                   (remover-ultimo estado))]
        [else (string-append estado tecla)]
        )
  
  )


(define TEXTO-INICIAL "")

(big-bang TEXTO-INICIAL
  [to-draw muestra-texto]
  [on-key teclear]
  )