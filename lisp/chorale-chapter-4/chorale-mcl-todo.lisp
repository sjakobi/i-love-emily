;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
:: Composition from a database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;
#| Calling (transpose-to-bach-range ((0 60 1000 4 96) (0 64 1000 3 96)  . . .
 transpose-to-bach-range returned ((0 64 1000 4 96) (0 68 1000 3 96) . . .|#
;;;;;

(defun TRANSPOSE-TO-BACH-RANGE (events)
  "As its name suggests."
  (let* ((high-low (highest-lowest-notes events))
         (intervals-off (list (- 83 (first high-low))
                              (- 40 (second high-low))))
         (middle (put-it-in-the-middle intervals-off)))
    (progn 
           (transpose middle events))))

;;;;;
#|  Calling (put-it-in-the-middle (4 4)) 
  put-it-in-the-middle returned 2 values :
       4
       0|#
;;;;;

(defun PUT-IT-IN-THE-MIDDLE (extremes)
  "Gets the average."
  (round (/ (+ (second extremes)(first extremes)) 2)))

;;;;;
#| Calling (highest-lowest-notes ((0 60 1000 4 96) (0 64 1000 3 96) . . .
  highest-lowest-notes returned (79 36)|#
;;;;;

(defun HIGHEST-LOWEST-NOTES (events)
  "Returns the highest and lowest pitches of its arg."
  (list (first (my-sort #'> (loop for event in (get-channel 1 events)
                                  collect (second event))))
        (first (my-sort #'< (loop for event in (let ((test (get-channel 4 events)))
                                                 (if (null test)(get-channel 2 events) test))
                                  collect (second event))))))

;;;;;
#| Calling (delay-for-upbeat ((0 60 1000 1 124))) 
 delay-for-upbeat returned ((3000 60 1000 1 124))
|#
;;;;;

(defun DELAY-FOR-UPBEAT (events)
  "Delays the upbeat."
  (reset-events-to events 3000))

;;;;;
#|Calling (reset-events-to ((0 60 1000 1 124)) 3000) 
  reset-events-to returned ((3000 60 1000 1 124))|#
;;;;;

(defun RESET-EVENTS-TO (events begin-time)
  "Resets the events for the delayed beat."
  (loop for event in (set-to-zero  events)
        collect (cons (+ begin-time (first event))(rest event))))
