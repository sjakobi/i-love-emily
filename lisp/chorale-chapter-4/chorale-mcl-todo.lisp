
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;     Chorale Function/Chapter 4      ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;           platform dependent        ;;;;;
                   ;;;;;          code to run chorale        ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;

;;;this loads the data

(setq *minimum-stack-overflow-size* 3048576)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
:: Composition from a database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;
#| Calling (find-triad-beginning) 
 find-triad-beginning returned b43500b-14|#
;;;;;

(defun FIND-TRIAD-BEGINNING ()
  "Returns the db with a triad beginning."
  (let* ((test (choose-one (eval (first (eval *composer*))))) ; λ - randomly choose a bach beat symbol
         (on-beat (get-on-beat (events (eval test))(very-first (events (eval test)))))
         (pcs (create-pitch-class-set (get-pitches on-beat))))
      (if (and (triad? on-beat)
               (or (members-all '(0 4 8) pcs)
                   (members-all '(0 4 7) pcs)
                   (members-all '(0 5 8) pcs)
                   (members-all '(2 7 11) pcs))
               (<= (third (first (events (eval test)))) 1000)
               (equal (length (events (eval test))) 4))
        test
        (find-triad-beginning)))) ; λ - recursively call until triad found
                                  ;     this seems rather inefficient

;;;;;
#|  Calling (members-all (0 4 8) (2 7 11)) 
  members-all returned nil|#
;;;;;

(defun MEMBERS-ALL (arrows target)
  "Checks to see if arrows are all in target."
  (cond ((null arrows) t)
        ((member (first arrows) target)
         (members-all (rest arrows) target))
        (t ())))

;;;;;
#|  Calling (GET-NOTE-TIMING (24000 59 1000 4 96) 24000) 
  GET-NOTE-TIMING returned 1000|#
;;;;;

(defun GET-NOTE-TIMING (event time)
  "grunt work for get-beat-length"
  (- (+ (first event)(third event)) time))

;;;;;
#| Calling (get-pitches ((0 48 1000 4 96))) 
 get-pitches returned (48)|#
;;;;;

(defun GET-PITCHES (events)
  "Gets the pitches from its arg."
  (loop for event in events
        collect (second event)))

;;;;;
#| Calling (find-events-duration ((53000 52 500 4 96) (53000 67 500 3 96) (53000 76 1000 2 96) (53000 84 1000 1 96) (53500 54 500 4 96) (53500 69 500 3 96))) 
 find-events-duration returned 1000|#
;;;;;

(defun FIND-EVENTS-DURATION (events &optional (duration 0))
  "Returns the events duration."
  (cond ((null events) duration)
        ((equal (fourth (first events)) 1)
         (find-events-duration (rest events) (+ duration (third (first events)))))
        (t (find-events-duration (rest events) duration))))

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
#|Calling (cadence-collapse ((0 48 1000 4 96) (0 60 1000 3 96) (0 67 1000 2 96) (0 76 1000 1 96) . . . 
cadence-collapse returned ((0 48 1000 4 96) (0 60 1000 3 96) . . . .|#
;;;;;

(defun CADENCE-COLLAPSE (events)
  "Ensures the final chord will not have offbeats."
  (apply #'append (collapse (collect-beats (sortcar #'< events)))))

;;;;;
#|Calling (collapse (((0 48 1000 4 96) (0 60 1000 3 96) (0 67 1000 2 96)  . . .
collapse returned (((0 48 1000 4 96) (0 60 1000 3 96)  . . . |#
;;;;;

(defun COLLAPSE (beats)
  "Collapses the final cadence."
  (cond ((null beats)())
        ((and (equal (length (first beats)) 4)
              (equal (third (first (first beats))) 2000))
         (cons (make-1000s (first beats))
               (collapse (reset (rest beats) 1000))))
        (t (cons (first beats)
                 (collapse (rest beats))))))

;;;;;
#|   Calling (make-1000s ((32000 52 2000 4 96) (32000 59 2000 3 96) (32000 64 2000 2 96) (32000 67 2000 1 96))) 
   make-1000s returned ((32000 52 1000 4 96) (32000 59 1000 3 96) (32000 64 1000 2 96) (32000 67 1000 1 96))|#
;;;;;

(defun MAKE-1000S (beat)
  "Makes all of the beat's durations into 1000s."
  (loop for event in beat
        collect (append (firstn 2 event) '(1000) (nthcdr 3 event))))

;;;;;
#|   Calling (reset (((34000 52 500 4 96) (34000 64 500 3 96) (34000 67 500 2 96) (34000 72 1000 1 96) . . .
reset returned (((33000 52 500 4 96) (33000 64 500 3 96)  . . .|#
;;;;;

(defun RESET (beats subtraction)
  "Resets the beats appropriately."
  (if (null beats)()
      (cons (loop for event in (first beats)
                  collect (append (list (- (first event) subtraction)) (cdr event)))
            (reset (rest beats) subtraction))))

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
