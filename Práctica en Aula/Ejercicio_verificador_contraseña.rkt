;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicio_verificador_contraseña) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;______________________________________________
;LICENCIATURA EN CIENCIAS DE LA COMPUTACIÓN
;PROGRAMACIÓN I - 2022
;PRÁCTICA EN AULA - Ejercicio de verificación Contraseña

;Martín Sferco
;Comisión 1 - Grupo 7
;______________________________________________

;CONSIGNA: Diseñar una función que dada una contraseña, determine si ésta es
;correta o no, de acuerdo a unas reglas dadas.
;______________________________________________

;Representaremos a la contra con un string
;La respuesta sobre el éxito o no de una contra, se representa con un string

;contra: String -> String
;Dada una cadena que representa una contra, determina si está cumpliendo con las reglas.
;Si no cumple, mostrar cuales reglas fallaron.

(check-expect (contra "Nicolas2010") "Contra correcta")
(check-expect (contra "Nico") "No cumple R1")
(check-expect (contra "nico") "No se cumplen R1 R2")
(check-expect (contra "2010nico") "No se cumplen R2 R4")
(check-expect (contra "2NICO") "No se cumplen R1 R3 R4")


(define (contra c)
  (cond [(string=? (verificador_regla c) "") "Contra correcta"]
        [(= (string-length (verificador_regla c)) 3) (string-append "No cumple" (verificador_regla c))]
        [else (string-append "No se cumplen" (verificador_regla c))])
  )




;verificador_regla: String -> String
;Dada una cadena que representa una contra, devuelve cuales fueron las reglas que fallaron,
;en forma de String.

(check-expect (verificador_regla "Nicolas2010") "")
(check-expect (verificador_regla "Nico") " R1")
(check-expect (verificador_regla "nico") " R1 R2")
(check-expect (verificador_regla "2010nico") " R2 R4")

(define (verificador_regla c)
  (string-append (if (> (string-length c) 6) "" " R1")
                 (if (string=? c (string-downcase c)) " R2" "")
                 (if (string=? c (string-upcase c)) " R3" "")
                 (if (string-numeric? (string-ith c 0)) " R4" "")
                 )
  )