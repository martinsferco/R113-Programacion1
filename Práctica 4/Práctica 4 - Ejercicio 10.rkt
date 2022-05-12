;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Práctica 4 - Ejercicio 10|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;__________________________________________
;LICENCIATURA EN CIENCIAS DE LA COMPUTACIÓN
;PROGRAMACIÓN I - 2022
;PRÁCTICA 4 - Ejercicio 10

;Martín Sferco
;Comisión 1 - Grupo 7
;__________________________________________

;Definiremos una estructura con la que representaremos a un estudiante.

(define-struct Estudiante [nombre promedio asistencia])
;La estructura es del tipo (String Number Number)
;El primer campo representa el nombre del estudiante.
;El segundo campo representa el promedio de sus calificaciones (0 a 10).
;El tercer campo representa el porcentaje de asistencia (0 a 100).



;estado-asistencia: Estudiante -> Boolean
;Recibe un Estudiante, y nos devuelve #t si cumple con el minimo del
;60% de asistencia a clase. En caso contrario devuelve #f.

(check-expect (estado-asistencia (make-Estudiante "Martin" 10 100)) #t)
(check-expect (estado-asistencia (make-Estudiante "Martin" 10 60)) #t)
(check-expect (estado-asistencia (make-Estudiante "Martin" 10 50)) #f)

(define (estado-asistencia estudiante)
  (>= (Estudiante-asistencia estudiante) 60)
  )

;estado-notas: Estudiante -> String
;Recibe un estudiante y nos devuelve su estado de acuerdo al promedio
;de clasificaciones que tenga.
;No tomamos en cuenta la asistencia que tenga.

(check-expect (estado-notas (make-Estudiante "Martin" 10 100)) "Promovido")
(check-expect (estado-notas (make-Estudiante "Martin" 7 100)) "Regular")
(check-expect (estado-notas (make-Estudiante "Martin" 5 100)) "Libre")

(define (estado-notas estudiante)
  (cond [(< (Estudiante-promedio estudiante) 6) "Libre"]
        [(< (Estudiante-promedio estudiante) 8) "Regular"]
        [else "Promovido"])
  )

;condicion: Estudiante -> String
;Toma un estudiante y nos devuelve en que condicion se encuentra
;siguiente las siguientes reglas:

;Si el/la estudiante tiene un porcentaje de inasistencia mayor al 40% queda automáticamente libre, sin importar el promedio de sus calificaciones.
;Si el/la estudiante tiene una asistencia mayor o igual al 60%:
    ;y tiene una nota inferior a 6, también se considera libre.
    ;y tiene una nota mayor o igual a 6 y menor estricta que 8, se considera regular.
    ;y una nota mayor o igual a 8, se considera promovido/a.

;En caso de que no se ingrese un estudiante nos devuelve un error de tipo.

(check-expect (condicion (make-Estudiante "Martin" 10 100)) "Promovido")
(check-expect (condicion (make-Estudiante "Martin" 10 50)) "Libre")
(check-expect (condicion (make-Estudiante "Martin" 5 100)) "Libre")
(check-expect (condicion (make-Estudiante "Martin" 7 100)) "Regular")
(check-error (condicion "Martin") "condicion: expects an Estudiante")

(define (condicion estudiante)
  (if (not (Estudiante? estudiante))
      (error "condicion: expects an Estudiante")
      
      (if (estado-asistencia estudiante)
          (estado-notas estudiante)
          "Libre"
          )
      )
  )