;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Práctica 4 - Ejercicio 12|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;__________________________________________
;LICENCIATURA EN CIENCIAS DE LA COMPUTACIÓN
;PROGRAMACIÓN I - 2022
;PRÁCTICA 4 - Ejercicio 12

;Martín Sferco
;Comisión 1 - Grupo 7
;__________________________________________

;Representaremos a una persona con una estructura Persona que definiremos
;a continuacion.

(define-struct Persona [nombre-completo peso unidadP altura unidadH])
;Persona es del tipo (String Number String Number String)
;1er campo: el nombre y apellido.
;2do campo: el valor numérico de su peso.
;3er campo: un string que representa la unidad en la cual está
           ;dado el peso (valores posibles: "g" o "kg").
;4to campo: el valor numérico de la estatura.
;5to campo: un string que representa la unidad en la cual está
           ;dada la estatura (valores posibles: "m" o "cm").


;pasaje-peso: Persona -> Number
;Dada una persona, nos devuelve su peso en kg.
;Sabemos que 1kg = 1000g.

(check-expect (pasaje-peso (make-Persona "Martin" 100 "kg" 1.7 "m")) 100)
(check-expect (pasaje-peso (make-Persona "Martin" 70000 "g" 1.7 "m")) 70)

(define (pasaje-peso persona)
  (if (string=? (Persona-unidadP persona) "kg")
      (Persona-peso persona)
      (/ (Persona-peso persona) 1000)
      )
  )

;pasaje-altura: Persona -> Number
;Dada una persona, nos devuelve su altura en metros.
;Sabemos que 1m = 100cm.

(check-expect (pasaje-altura (make-Persona "Martin" 100 "kg" 1.7 "m")) 1.7)
(check-expect (pasaje-altura (make-Persona "Martin" 70000 "g" 170 "cm")) 1.7)

(define (pasaje-altura persona)
  (if (string=? (Persona-unidadH persona) "m")
      (Persona-altura persona)
      (/ (Persona-altura persona) 100)
      )
  )


;IMC: Persona -> Number
;Recibe una persona y calcula su IMC de acuerdo a formula
;correspondiente. Las unidades de masa y altura tienen que estar en
;kg y m respectivamente.

(check-error (IMC "Martin" ) "Tipo de dato invalido")
(check-within (IMC (make-Persona "Martin" 80 "kg" 1.8 "m")) 24.7 0.1)
(check-within (IMC (make-Persona "Martin" 80000 "g" 1.8 "m")) 24.7 0.1)
(check-within (IMC (make-Persona "Martin" 80 "kg" 180 "cm")) 24.7 0.1)
(check-within (IMC (make-Persona "Martin" 80000 "g" 180 "cm")) 24.7 0.1)

(define (IMC persona)
  (if (not (Persona? persona))
      (error "Tipo de dato invalido")
      (/ (pasaje-peso persona) (sqr (pasaje-altura persona)))
      )
  )

