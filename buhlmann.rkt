#lang scheme

; Buhlmann ZH-L16 Decompression Model
; *** This version only calculates Nitrogen loads, not Helium **

(define (feet-to-atm depth-ft)
  (+ 1 (* depth-ft 0.0295)))

(define (atm-to-feet atm)
  (/ (- atm 1) 0.0295))

(define compartment-half-times
  '(4.0 8.0 12.5 18.5 27.0 38.3 54.3 77.0
        109.0 146.0 187.0 239.0 305.0 390.0
        498.0 635.0))

; calculate the partial preassure of N2 in a single tissue compartment
(define (compartment-ppn initial-ppn gas-ppn segment-time p-atm half-time)
  (+ initial-ppn
     (* (- 1 (expt 2 (/ (* -1 segment-time) half-time)))
        (- (* p-atm gas-ppn ) initial-ppn)))
  )

; iterate through all the compartments
(define (process-compartments  comp-list gas-ppn p-atm segment-time)
  (map (lambda (x y)
         (compartment-ppn x gas-ppn segment-time p-atm y))
       comp-list compartment-half-times)
  )

; iterate through the profile
(define (process-profile comp-list profile)
  (map (lambda (p)
         (begin
           ; check for deco violations
           (if( negative? (- (feet-to-atm (list-ref p 0))
              (atm-to-feet (ascent-ceiling-atm comp-list))))
                (printf "*** Deco Violation ***\n") #f
                 )
           ; process the next step in the profile
           (set! comp-list (process-compartments comp-list
                                               (list-ref p 2)
                                               (feet-to-atm (list-ref p 0))
                                               (list-ref p 1)))
            )) profile)
  comp-list ; return the tissue compartment loads
  )


; calculate the current ceiling in atmospheres
(define (ascent-ceiling-atm comp-list)
  (let ((a-val (map (lambda (x) 
                      (* 2 (expt x (/ -1 3)))
                      )  compartment-half-times))
        (b-val (map (lambda (x) 
                      (- 1.005 (expt x (/ -1 2)))
                      )  compartment-half-times)))
    (argmax max (map (lambda (x a b)
                       (* ( - x a) b))
                     comp-list a-val b-val))
    ))

;; main ---------------------

; profile is a list of (depth-in-ft time-in-min gas-ppn)
; TODO: don't hard code this
(define dive-profile
  '((30.0 1 0.79)
    (60.0 1 0.79)
    (90.0 1 0.79)
    (230.0 9 0.79)  
    ;(15.0 9 0.79)     
    )
  )


; initialize the ppn in all tissue compartments to that of air
(let ((compartments (build-list 16 (lambda (x) (+ .79 (* x 0))))))
  
  ; model the dive
  (set! compartments  (process-profile compartments dive-profile))
  
  ; calculate the resulting ascent ceiling
  (let ((result (ascent-ceiling-atm compartments)))
    
    ; result is the ascent ceiling in atms.
    ; any number above 1.0 indicates a deco obligation.
    (printf "Current ceiling ~a atm \n"
            (real->decimal-string result 3))
    
    (if (> result 1)
        (printf "*** Deco Required ***\nCeiling depth: ~a ft \n"
                (real->decimal-string (atm-to-feet result) 3))
        (printf "No decompression necessary\n")
        )
    )
  
  )


