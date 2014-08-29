
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
#| Calling (GET-LONG-PHRASES (6000 15000)) 
 GET-LONG-PHRASES returned NIL|#
;;;;;

(defun GET-LONG-PHRASES (distances)
  "Returns phrases of greater than 120000 duration."
  (cond ((null (rest distances))())
        ((> (- (second distances)(first distances)) 12000)
         (cons (firstn 2 distances)
               (get-long-phrases (rest distances))))
        (t (get-long-phrases (rest distances)))))

;;;;;
#|    Calling (Discover-cadences ((4000)) ((3000 61 1000 1 96) (3000 69 1000 2 96) (3000 69 1000 3 96) (3000 69 1000 4 96))) 
    Discover-cadences returned ((3000 61 1000 1 96) (3000 69 1000 2 96) (3000 69 1000 3 96) (3000 69 1000 4 96))|#
;;;;;

(defun DISCOVER-CADENCES (missing-cadence-locations ordered-events)
  "Makes an appropriate cadence possible."
  (if (null missing-cadence-locations) ordered-events
      (discover-cadences (rest missing-cadence-locations) 
                     (discover-cadence (first missing-cadence-locations) ordered-events))))

;;;;;
#|  Calling (Discover-cadence (4000) ((3000 61 1000 1 96) (3000 69 1000 2 96) (3000 69 1000 3 96) (3000 69 1000 4 96))) 
  Discover-cadence returned ((3000 61 1000 1 96) (3000 69 1000 2 96) (3000 69 1000 3 96) (3000 69 1000 4 96))|#
;;;;;

(defun DISCOVER-CADENCE (missing-cadence-locations ordered-events)
  "Discovers an appropriate cadence."
  (let* ((relevant-events (get-region (first missing-cadence-locations)(second missing-cadence-locations) ordered-events))
         (places-for-cadence (find-cadence-place relevant-events))
         (best-location-for-new-cadence (if places-for-cadence (find-best-on-time places-for-cadence) nil)))
    (if (null best-location-for-new-cadence) ordered-events
    (sortcar #'< 
             (append (resolve (get-region best-location-for-new-cadence (+ best-location-for-new-cadence 1000) relevant-events))
                     (remove-region best-location-for-new-cadence (+ best-location-for-new-cadence 1000) ordered-events))))))

;;;;;
#| Calling (FIND-CADENCE-START-TIMES ((3000 61 1000 1 96) (3000 69 1000 2 96) (3000 69 1000 3 96) (3000 69 1000 4 96))) 
 FIND-CADENCE-START-TIMES returned (3000)|#
;;;;;

(defun FIND-CADENCE-START-TIMES (ordered-events)
  "Finds the cadence start times."
  (let ((distance-to-cadence (distance-to-cadence ordered-events)))
    (cond ((null ordered-events)())
          ((null distance-to-cadence)
           (find-cadence-start-times (rest ordered-events)))
          (t (cons distance-to-cadence
                   (find-cadence-start-times (clear-to distance-to-cadence ordered-events)))))))

;;;;;
#| Calling (DISTANCE-TO-CADENCE ((3000 61 1000 1 96) (3000 69 1000 2 96) (3000 69 1000 3 96) (3000 69 1000 4 96))) 
  FIND-2000S returned NIL
 DISTANCE-TO-CADENCE returned 3000|#
;;;;;

(defun DISTANCE-TO-CADENCE (ordered-events)
  "Returns the distance tocadence of the arg."
  (let ((quarter-note-distance (find-1000s ordered-events))
        (half-note-distance (find-2000s ordered-events)))
    (cond ((and (null quarter-note-distance)(null half-note-distance)) ())
          ((null quarter-note-distance) half-note-distance)
          ((null half-note-distance) quarter-note-distance)
          (t (if (> quarter-note-distance half-note-distance) half-note-distance
                 quarter-note-distance)))))

;;;;;
#| Calling (FIND-1000S ((3000 61 1000 1 96) (3000 69 1000 2 96) (3000 69 1000 3 96) (3000 69 1000 4 96))) 
 FIND-1000S returned 3000|#
;;;;;

(defun FIND-1000S (ordered-events &optional (start-time (very-first ordered-events)))
  "Returns the ontime if the ordered events are all duration 1000."
  (cond ((null ordered-events)())
        ((and (let ((channel-1-event (first (get-channel 1 ordered-events))))
                (and (equal (third channel-1-event) 1000)
                     (equal start-time (first channel-1-event))))
              (let ((channel-1-event (first (get-channel 2 ordered-events))))
                (and (equal (third channel-1-event) 1000)
                     (equal start-time (first channel-1-event))))
              (let ((channel-1-event (first (get-channel 3 ordered-events))))
                (and (equal (third channel-1-event) 1000)
                     (equal start-time (first channel-1-event))))
              (let ((channel-1-event (first (get-channel 4 ordered-events))))
                (and (equal (third channel-1-event) 1000)
                     (equal start-time (first channel-1-event)))))
         start-time)
        (t (find-1000s (rest ordered-events)))))

;;;;;
#| Calling (FIND-2000S ((3000 61 1000 4 96) (3000 69 1000 3 96))) 
 FIND-2000S returned NIL|#
;;;;;

(defun FIND-2000S (ordered-events &optional (start-time (very-first ordered-events)))
  "Returns events of 2000 duration."
  (cond ((null ordered-events)())
        ((and (let ((channel-1-event (first (get-channel 1 ordered-events))))
                (and (equal (third channel-1-event) 2000)
                     (equal start-time (first channel-1-event))))
              (let ((channel-1-event (first (get-channel 2 ordered-events))))
                (and (equal (third channel-1-event) 2000)
                     (equal start-time (first channel-1-event))))
              (let ((channel-1-event (first (get-channel 3 ordered-events))))
                (and (equal (third channel-1-event) 2000)
                     (equal start-time (first channel-1-event))))
              (let ((channel-1-event (first (get-channel 4 ordered-events))))
                (and (equal (third channel-1-event) 2000)
                     (equal start-time (first channel-1-event)))))
         start-time)
        (t (find-2000s (rest ordered-events)))))


;;;;;
#| Calling (GET-BEAT-LENGTH ((24000 59 1000 4 96) (24000 62 1000 3 96) (24000 67 1000 2 96) (24000 74 1000 1 96))) 
 GET-BEAT-LENGTH returned 1000|#
;;;;;

(defun GET-BEAT-LENGTH (events)
  "this is used in re-time for setting the new time!
   requires that the first in events be sorted to current time!"
  (let ((time (very-first events)))
    (first (my-sort #'> (loop for event in events
                              collect (get-note-timing event time))))))

;;;;;
#|  Calling (GET-NOTE-TIMING (24000 59 1000 4 96) 24000) 
  GET-NOTE-TIMING returned 1000|#
;;;;;

(defun GET-NOTE-TIMING (event time)
  "grunt work for get-beat-length"
  (- (+ (first event)(third event)) time))


;;;;;
#| Calling (REMOVE-REGION 0 4000 ((3000 61 1000 4 96) (3000 69 1000 3 96))) 
 REMOVE-REGION returned NIL|#
;;;;;

(defun REMOVE-REGION (begin-time end-time events)
  "Removes the region boardered by begin and end times."
  (cond ((null events)())
        ((and (>= (very-first events) begin-time)
              (< (very-first events) end-time))
         (remove-region begin-time end-time (rest events)))
        (t (cons (first events)
                 (remove-region begin-time end-time (rest events))))))

;;;;;
#| Calling (GET-REGION 0 4000 ((3000 61 1000 4 96) (3000 69 1000 3 96))) 
 GET-REGION returned ((3000 61 1000 4 96) (3000 69 1000 3 96))|#
;;;;;

(defun GET-REGION (begin-time end-time events)
  "Returns the region boardered by begin and end times."
  (cond ((null events)())
        ((and (>= (very-first events) begin-time)
              (< (very-first events) end-time))
         (cons (first events)
               (get-region begin-time end-time (rest events))))
        (t (get-region begin-time end-time (rest events)))))

;;;;;
#| Calling (RESOLVE ((3000 61 1000 4 96) (3000 69 1000 3 96))) 
 RESOLVE returned ((3000 61 1000 4 96) (3000 69 1000 3 96))|#
;;;;;

(defun RESOLVE (beat &optional (on-time (very-first beat)))
  "Resolves the beat if necessary."
  (cond ((null beat)())
        ((equal (third (first beat)) 1000)
         (cons (first beat)
               (resolve (rest beat) on-time)))
        (t (let ((test (get-on-beat (get-channel (fourth (first beat)) beat) on-time)))
             (cons (if (>= (third (first test)) 1000)(first test)
                       (append (firstn 2 (first test)) '(1000) (nthcdr 3 (first test))))
                   (resolve (remove-all (get-channel (fourth (first beat)) beat) beat) on-time))))))

;;;;;
#| Calling (FIND-BEST-ON-TIME (0 1000 2000)) 
 FIND-BEST-ON-TIME returned 1000|#
;;;;;

(defun FIND-BEST-ON-TIME (on-times)
  "Finds the best ontime."
  (find-closest (+ (/ (- (my-last on-times)(first on-times)) 2)(first on-times)) on-times))

;;;;;
#| Calling (FIND-CADENCE-PLACE ((3000 61 1000 4 96) (3000 69 1000 3 96))) 
 FIND-CADENCE-PLACE returned NIL|#
;;;;;

(defun FIND-CADENCE-PLACE (ordered-events)
  "Returns the best place for a first cadence."
  (let ((beats (collect-beats ordered-events)))
    (loop for beat in beats
          if (and (on-beat (firstn 4 beat)(very-first beat))
                  (triad? (firstn 4 beat))
                  (not-beyond-1000 beat))
          collect (very-first beat))))

;;;;;
#|  Calling (NOT-BEYOND-1000 ((3000 61 1000 4 96) (3000 69 1000 3 96))) 
  NOT-BEYOND-1000 returned T|#
;;;;;

(defun NOT-BEYOND-1000 (beat &optional (channel 1))
  "Returns t if the beat does not contain events beyond the incept time."
  (cond ((equal channel 5) t)
        ((not-beyond (get-channel channel beat))
         (not-beyond-1000 beat (+ channel 1)))
        (t ())))

;;;;;
#|  Calling (NOT-BEYOND ((3000 61 1000 4 96) (3000 69 1000 3 96))) 
  NOT-BEYOND returned NIL|#
;;;;;

(defun NOT-BEYOND (channel-events)
  "Returns events beyond the initial ontime."
  (if (not (> (apply #'+ (mapcar #'third channel-events)) 1000)) t))

;;;;;
#| Calling (ON-BEAT ((3000 61 1000 4 96) (3000 69 1000 3 96)) 3000) 
 ON-BEAT returned T|#
;;;;;

(defun ON-BEAT (events ontime)
  "Returns t if the events conform to ontime."
  (cond ((null events) t)
        ((and (thousandp (very-first events))(equal (very-first events) ontime))
         (on-beat (rest events) ontime))
        (t ())))

;;;;;
#|(find-closest 6 '(1 2 3 5 7 8))
       7|#
;;;;;

(defun FIND-CLOSEST (number list)
  "finds the closest number in list to number."
  (let ((test (loop for item in list
                    collect (abs (- number item)))))
    (nth (choose-one (positions (first (my-sort '< test)) test)) list)))

;;;;;
#|(positions 1 '(4 5 3 1 2 1 4 5))
       (3 5)|#
;;;;;

(defun POSITIONS (number list)
  "Shows the positions of number in list."
  (let ((position ()))
    (loop until (null (member number list))
          do (setf position (position number list))
          collect position
          do (setf list  (substitute 'x number list 
                                     :end (1+ position))))))

;;;;;
#|(remove-all '(1 2 3) '(3 4 2 1  5 6 3))
      (4 5 6)|#
;;;;;

(defun REMOVE-ALL (stuff other-stuff)
  "Removes all of the stuff from the other-stuff."
  (loop when (null stuff) return other-stuff
        do (setf other-stuff (remove (first stuff) other-stuff :test 'equal))
        do (setf stuff (rest stuff))))

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
#|Calling (re-time (((15000 60 1000 4 96) (15000 64 1000 3 96) . . .
re-time returned (((0 60 1000 4 96) (0 64 1000 3 96) . . .|#
;;;;;

(defun RE-TIME (event-lists &optional (current-time 0))
  "Re-times the beats to fit together."
  (if (null event-lists)()
      (cons (loop for event in (set-to-zero (first event-lists))
                  collect (cons (+ (first event) current-time) (rest event)))
            (re-time (rest event-lists) (+ current-time 
                                           (get-beat-length 
                                            (first event-lists)))))))

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

;;;;;
#|Calling (ensure-necessary-cadences ((0 52 1000 4 96) (0 60 1000 3 96) . . .
 ensure-necessary-cadences returned ((0 52 1000 4 96) (0 60 1000 3 96) . . .|#
;;;;;

(defun ENSURE-NECESSARY-CADENCES (ordered-events)
  "Ensures the cadences are proper."
  (let ((cadence-start-times (find-cadence-start-times ordered-events)))
    (Discover-cadences (get-long-phrases (if (not (zerop (first cadence-start-times))) (cons 0 cadence-start-times) cadence-start-times))
                   ordered-events)))

;;;;;
#|Calling (check-for-parallel ((0 48 1000 4 96) (0 64 1000 3 96) . . .
check-for-parallel returned t|#
;;;;;

(defun CHECK-FOR-PARALLEL (events)
  "Checks for parallel motion."
  (let ((sorted-pitches-by-beat (loop for beat in (collect-beats (firstn 30 (sortcar #'< events)))
                                      collect (get-pitches (get-on-beat beat (very-first beat))))))
    (and (equal (length (first sorted-pitches-by-beat)) 4)
         (equal (length (second sorted-pitches-by-beat)) 4)
         (or (and (plusp (- (first (first sorted-pitches-by-beat))
                            (first (second sorted-pitches-by-beat))))
                  (plusp (- (second (first sorted-pitches-by-beat))
                            (second (second sorted-pitches-by-beat))))
                  (plusp (- (third (first sorted-pitches-by-beat))
                            (third (second sorted-pitches-by-beat))))
                  (plusp (- (fourth (first sorted-pitches-by-beat))
                            (fourth (second sorted-pitches-by-beat)))))
             (and (minusp (- (first (first sorted-pitches-by-beat))
                             (first (second sorted-pitches-by-beat))))
                  (minusp (- (second (first sorted-pitches-by-beat))
                             (second (second sorted-pitches-by-beat))))
                  (minusp (- (third (first sorted-pitches-by-beat))
                             (third (second sorted-pitches-by-beat))))
                  (minusp (- (fourth (first sorted-pitches-by-beat))
                             (fourth (second sorted-pitches-by-beat)))))))))
