;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
:: Composition from a database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
