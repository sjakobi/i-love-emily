;;; λ - Code that has been translated to Haskell already

                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;
                   ;;;;;            By David Cope            ;;;;;
                   ;;;;;     Chorale Function/Chapter 4      ;;;;;
                   ;;;;;             COMMON LISP             ;;;;;
                   ;;;;;           platform dependent        ;;;;;
                   ;;;;;          code to run chorale        ;;;;;
                   ;;;;;               function              ;;;;;
                   ;;;;;COMPUTER MODELS OF MUSICAL CREATIVITY;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mutable variables
;; λ - Most of them are actually local variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defVar *LEXICONS* ())      ; global variables collecting lexicons. Seemingly unused.

(defVar *RULES-STORAGE* ()) ; local variable for GET-RULES recursive call

(defVar DAVIDCOPE ())       ; local variable for splash page
(defVar COPE-TEXT ())
(defVar DAVIDCOPE1 ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Splash window
;; λ - no need to translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defClass ALL-ABOUT-WINDOW (window)
  nil
  (:default-initargs :window-type :double-edge-box 
    :window-title "Chorale"
    :view-position #@(40 40)
    :view-size #@(300 180)
    :close-box-p ()
    :auto-position :centermainscreen))

;;;; λ - splash window
(defMethod INITIALIZE-INSTANCE ((window ALL-ABOUT-WINDOW) &rest initargs)
  (apply #'call-next-method window initargs)
  (add-subviews window 
    (setq davidcope (make-instance 'static-text-dialog-item
                      :dialog-item-text "Chorale"
                      :view-position '#@(77 26)
                      :view-size #@(322 54)
                      :view-font '("times" 46)))
    (setq davidcope (make-instance 'static-text-dialog-item
                      :dialog-item-text "Please be patient."
                      :view-position '#@(97 76)
                      :view-size #@(322 54)
                      :view-font '("times" 16)))
    (setq davidcope1 (make-instance 'static-text-dialog-item
                       :dialog-item-text "Lots of data must be loaded."
                       :view-position '#@(62 96)
                       :view-size #@(322 54)
                       :view-font '("times" 16)))
    (setq Cope-text (make-instance 'static-text-dialog-item
                      :dialog-item-text "© David Cope 1992 -"
                      :view-position '#@(80 140)
                      :view-size #@(334 54)
                      :view-font '("times" 16 :bold))))
  (set-part-color davidcope :text *red-color*)
  (set-part-color davidcope1 :text *red-color*)
  (set-part-color Cope-text :text *dark-green-color*))

(defVar *ABOUT-WINDOW* (make-instance 'ALL-ABOUT-WINDOW))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building database from pieces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun CREATE-COMPLETE-DATABASE (db-names &optional (counter 1))
  "Loads and makes proper objects out of the db-names arg."
  (if (null db-names) t
      (progn (let ((beats (remove-nils (collect-beats (set-to-zero 
                                                       (sortcar #'< (eval (first db-names)))))))
                   (name ())
                   (start t))
               (loop until (null beats)
                     do (setq name (make-name (first db-names) counter))
                     do 
                     
                     (let ((start-notes (get-onset-notes (first beats)))
                           (destination-notes (get-onset-notes (second beats)))
                           (events (first beats)))
                       (set name
                            (make-instance 'beat-it :start-notes start-notes
                                           :destination-notes destination-notes
                                           :events events
                                           :voice-leading (first 
                                                           (my-push (cons (get-rules start-notes destination-notes name)
                                                                          (list name (very-first (sortcar #'< events))))
                                                                    (concat *composer* '- 'rules)))
                                           :speac ())))
                     do (setf counter (1+ counter))
                     do (setf beats (rest beats))
                     ; here
                     do (put-beat-into-lexicon name)
                     
                     do (my-push name (concat *composer* '- 'compose-beats))
                     do (if start (my-push name (concat *composer* '- 'start-beats)))
                     do (setf start nil)))
             (create-complete-database (rest db-names)))))

;;;;;
#| Calling (remove-nils (nil nil nil 1 nil)) 
 remove-nils returned (1)|#
;;;;;

(defun REMOVE-NILS (stuff)
  "Removes the nils from the stuff."
  (cond ((null stuff)())
        ((null (first stuff))
         (remove-nils (rest stuff)))
        (t (cons (first stuff)
                 (remove-nils (rest stuff))))))

;;;;;
#|  Calling (MAKE-LEXICON-NAME (57 60 69 76)) 
  MAKE-LEXICON-NAME returned BACH-57-60-69-76|#
;;;;;

(defun MAKE-LEXICON-NAME (note-numbers &optional (names *mix-names*))
  "Creates the appropriate lexicon name for the object."
  (cond ((null *mix*)(implode (cons *composer* (cons '- (hyphenate note-numbers)))))
        ((null names)(implode (cons *composer* (cons '- (hyphenate note-numbers)))))
        ((boundp (implode (cons (first names) (cons '- (hyphenate note-numbers)))))
         (implode (cons (first names) (cons '- (hyphenate note-numbers)))))
        (t (make-lexicon-name note-numbers (mix (rest names))))))

;;;;;
#| Calling (HYPHENATE (4 7 1)) 
 HYPHENATE returned (4 - 7 - 1)|#
;;;;;

(defun HYPHENATE (note-numbers)
  "Hyphenates the numbers in its arg."
  (if (null note-numbers)()
      (append (if (null (rest note-numbers))
                (list (first note-numbers))
                (list (first note-numbers) '-))
              (hyphenate (rest note-numbers)))))

;;;;;
#|  Calling (MAKE-LEXICON-NAME (57 60 69 76)) 
  EXIST-LEXICON returned NIL|#
;;;;;

(defun EXIST-LEXICON (lexicon-name)
  "Sees if the lexicon exists."
  (boundp lexicon-name))

;;;;;
#| Calling (MAKE-NAME B206B 1) 
 MAKE-NAME returned B206B-1|#
;;;;;

(defun MAKE-NAME (db-name counter)
  "Simple synonym for imploding the database name and number."
  (implode (cons db-name  (list '- counter))))

;;;;;
#| Calling (PUT-BEAT-INTO-LEXICON B206B-1) 
 PUT-BEAT-INTO-LEXICON returned (BACH-57-60-69-76)|#
;;;;;

(defun PUT-BEAT-INTO-LEXICON (beat-name)
  "Puts the beat arg into the appropriate lexicon."
  (let ((lexicon-name (make-lexicon-name (start-notes (eval beat-name)))))
    (if (and (exist-lexicon lexicon-name)
             (not (member beat-name (beats (eval lexicon-name)) :test #'equal)))
      (setf (beats (eval lexicon-name)) (cons beat-name (beats (eval lexicon-name))))
      (progn (set lexicon-name
                  (make-instance 'lexicon :beats (list beat-name)))
             (pushnew lexicon-name *lexicons*)))))

;;;;;
#| Calling (set-to-zero ((31000 60 1000 4 96) (31000 67 1000 3 96) (31000 72 1000 2 96) (31000 76 1000 1 96))) 
 set-to-zero returned ((0 60 1000 4 96) (0 67 1000 3 96) (0 72 1000 2 96) (0 76 1000 1 96))|#
;;;;;

(defun SET-TO-ZERO (events &optional (subtract (very-first events)))
  "Sets the events to zero."
  (if (null events)()
      (cons (cons (- (very-first events) subtract)
                  (rest (first events)))
            (set-to-zero (rest events) subtract))))

;;;;;
#| Calling (thousandp 5000) 
 thousandp returned t|#
;;;;;

(defun THOUSANDP (number)
  "Returns the number under 1000."
  (if (zerop (mod number 1000)) t))

;;;;;
#| Calling (GET-RULES (57 60 69 76) (59 62 67 79) B206B-1) 
 GET-RULES returned ((3 2 2 B206B-1) (12 2 -2 B206B-1) (7 2 3 B206B-1) (9 2 -2 B206B-1) (4 2 3 B206B-1) (7 -2 3 B206B-1))|#
;;;;;

(defun GET-RULES (start-notes destination-notes name)
  "Gets the intervals between adjacent sets of the two args."
  (setq *rules-storage* ())
  (let ((test (make-lists-equal (list start-notes destination-notes))))
    (get-rules1 (first test)(second test) name)))

;;;;;
#|  Calling (GET-RULES1 (57 60 69 76) (59 62 67 79) B206B-1) 
  GET-RULES1 returned ((3 2 2 B206B-1) (12 2 -2 B206B-1) (7 2 3 B206B-1) (9 2 -2 B206B-1) (4 2 3 B206B-1) (7 -2 3 B206B-1))|#
;;;;;

(defun GET-RULES1 (start-notes destination-notes name)
  "Does the grunt work for get-rules."
  (if (or (null (rest start-notes))(null (rest destination-notes)))
    (reverse *rules-storage*)
    (progn
      (setq *rules-storage* (append (reverse (get-rule (- (first destination-notes) (first start-notes)) 
                                                       (first start-notes) start-notes destination-notes name)) *rules-storage*))
      (get-rules1 (rest start-notes) (rest destination-notes) name))))

;;;;;
#|   Calling (GET-RULE 2 57 (57 60 69 76) (59 62 67 79) B206B-1) 
   GET-RULE returned ((7 -2 3 B206B-1))|#
;;;;;
; λ - The comment seems to be wrong, it only displays the last rule.

(defun GET-RULE (voice start-note start-notes destination-notes name)
  "Gets the rule between first two args."
  (if (or (null (rest start-notes))(null destination-notes))()
      (cons (list (reduce-interval (- (second start-notes) start-note))
                  voice
                  (- (second destination-notes) (second start-notes))
                  name)
            (get-rule voice start-note (rest start-notes)(rest destination-notes) name))))

;;;;;
#|  Calling (MAKE-LISTS-EQUAL ((57 60 69 76) (59 62 67 79))) 
  MAKE-LISTS-EQUAL returned ((57 60 69 76) (59 62 67 79))|#
;;;;;

(defun MAKE-LISTS-EQUAL (lists)
  "Ensures the two lists are equal in length."
  (cond ((> (length (first lists))(length (second lists)))
         (list (firstn (length (second lists)) (first lists))(second lists)))
        ((> (length (second lists))(length (first lists)))
         (list (first lists)(firstn (length (first lists)) (second lists))))
        (t lists)))

;;;;;
#|Calling (REDUCE-INTERVAL 3) 
    REDUCE-INTERVAL returned 3|#
;;;;;

(defun REDUCE-INTERVAL (interval)
  "Reduces the interval mod 12."
  (cond ((<= (abs interval) 12) interval)
        ((minusp interval)(reduce-interval (+ interval 12)))
        (t (reduce-interval (- interval 12)))))

;;;;;
#| Calling (GET-ONSET-NOTES ((0 57 1000 4 96) (0 60 1000 3 96) (0 69 1000 2 96) (0 76 1000 1 96))) 
 GET-ONSET-NOTES returned (57 60 69 76)|#
;;;;;

(defun GET-ONSET-NOTES (events)
  "Gets the onset pitches for its arg."
  (let ((onbeat (very-first events)))
    (loop for event in events
          if (equal (first event) onbeat)
          collect (second event))))


;;;;;;;
#|  Calling (COLLECT-BEATS ((0 57 1000 4 96) (0 60 1000 3 96) . . .
 COLLECT-BEATS returned (((0 57 1000 4 96) (0 60 1000 3 96)  . . .|#
;;;;;;;

(defun COLLECT-BEATS (events)
  "(collect-beats (sortcar #'< b4003))"
  (if (null events)()
      (let* ((test (collect-by-timing (first-place-where-all-together events) events))
             (reduced-test (nthcdr (length test) events)))
        (cons test 
              (collect-beats reduced-test)))))

;;;;;
#| Calling (RETURN-BEAT ((0 45 1000 4 96) (1000 57 500 4 96))) 
 RETURN-BEAT returned 1|#
;;;;;
; λ - This function appears to be unused.

(defun RETURN-BEAT (channel-events &optional (start-time (very-first channel-events)))
  "Returns the beat number of the initiating event."
  (cond ((null channel-events) nil)
        ((and (thousandp (very-first channel-events))
              (not (equal start-time (very-first channel-events))))
         (/ (- (very-first channel-events) start-time) 1000))
        (t (return-beat (rest channel-events)
                        start-time))))


;;;;;
#|  Calling (COLLECT-BY-TIMING 1000 ((0 57 1000 4 96) (0 60 1000 3 96) . . .
  COLLECT-BY-TIMING returned ((0 57 1000 4 96) (0 60 1000 3 96) (0 69 1000 2 96) (0 76 1000 1 96))|#
;;;;;

(defun COLLECT-BY-TIMING (timing events)
  "Collects the events accoring to timing."
  (cond ((null events)())
        ((<= (+ (first (first events))(fourth (first events))) timing)
         (cons (first events)
               (collect-by-timing timing (rest events))))
        (t (collect-by-timing timing (rest events)))))

;;;;;
#|  Calling (FIRST-PLACE-WHERE-ALL-TOGETHER ((0 57 1000 4 96) (0 60 1000 3 96) (0 69 1000 2 96) . . .
FIRST-PLACE-WHERE-ALL-TOGETHER returned 1000|#
;;;;;
; 
; λ -
; It appears that this function looks at the first notes of a piece and
; returns the time on which all channels end simultaneously.
; This time has to be on the beat, i.e. it must be a multiple of 1000.
;

(defun FIRST-PLACE-WHERE-ALL-TOGETHER (events)
  "This looks ahead to get the first time they end together"
  (let* ((test (plot-timings events))
         
         (channels (get-channel-numbers-from-events events))
         
         (ordered-timings-by-channel (loop for channel in channels 
                                           collect (collect-timings-by-channel test channel))))
    
    (all-together (first ordered-timings-by-channel)
                  (rest ordered-timings-by-channel))))

;;;;;
#|   Calling (ALL-TOGETHER ((1 1000) (1 2000) (1 2500) (1 3000) . . .
   ALL-TOGETHER returned 1000|#
;;;;;

(defun ALL-TOGETHER (channel channels)
  "Returns the appropriate channel timing."
  (cond ((null channel) (second (my-last (my-last channels)))) ;;; here is our remaining problem!!!!!
        ((find-alignment-in-all-channels (second (first channel)) channels) )
        (t (all-together (rest channel) channels))))

;;;;;
#|    Calling (FIND-ALIGNMENT-IN-ALL-CHANNELS 1000 (((2 1000) (2 2000) (2 2500) . . .
FIND-ALIGNMENT-IN-ALL-CHANNELS returned 1000|#
;;;;;

(defun FIND-ALIGNMENT-IN-ALL-CHANNELS (point channels)
  "run this on the channels of the channel-point-lists"
  (cond ((null channels) point)
        ((null point) point)
        ((find-alignment point (first channels))
         (find-alignment-in-all-channels point (rest channels)))
        (t ())
        ))

;;;;;
#|     Calling (FIND-ALIGNMENT 1000 ((2 1000) (2 2000) (2 2500) (2 3000) (2 3500) (2 4000) (2 4 . . .
     FIND-ALIGNMENT returned T|#
;;;;;

(defun FIND-ALIGNMENT (point channel)
  "? (find-alignment 1000 '((4 1000) (4 1000) (4 5000)))
   t this finds the timing point in the channel"
  (cond ((null channel)())
        ((and (thousandp point)
              (assoc point (mapcar #'reverse channel) :test #'equal))
         t)
        (t (find-alignment point (rest channel)))))

;;;;;
#|Calling (COLLECT-TIMINGS-BY-CHANNEL ((4 1000) (3 1000) (2 1000)  . . .
   COLLECT-TIMINGS-BY-CHANNEL returned ((1 1000) (1 2000) (1 2500) (1 3000) (1 3500) (1 4000) . . .|#
;;;;;

(defun COLLECT-TIMINGS-BY-CHANNEL (timings channel)
  "collects the timings of the channel indicated in second arg"
  (cond ((null timings)())
        ((equal (very-first timings) channel)
         (cons (first timings)
               (collect-timings-by-channel (rest timings) channel)))
        (t (collect-timings-by-channel (rest timings) channel))))

;;;;;
#| Calling (PLOT-TIMINGS ((0 57 1000 4 96) (0 60 1000 3 96)  . . .
PLOT-TIMINGS returned ((4 1000) (3 1000) (2 1000) (1 1000) . . .|#
;;;;;

(defun PLOT-TIMINGS (events)
  "Plots out the times of each beat."
  (if (null events)()
      (cons (list (fourth (first events))(+ (very-first events)(third (first events))))
            (plot-timings (rest events)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Channel utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;
#|   Calling (GET-CHANNEL-NUMBERS-FROM-EVENTS ((0 57 1000 4 96) (0 60 1000 3 96) . . .
GET-CHANNEL-NUMBERS-FROM-EVENTS returned (1 2 3 4)|#
;;;;;

(defun GET-CHANNEL-NUMBERS-FROM-EVENTS (events &optional (channels ()))
  "simply gets the channel numbers from the music"
  (cond ((null events) channels)
        ((not (member (fourth (first events)) channels :test #'equal))
         (get-channel-numbers-from-events (rest events) (cons (fourth (first events)) channels)))
        (t (get-channel-numbers-from-events (rest events) channels))))

;;;;;
#| Calling (GET-OTHER-CHANNELS 4 ((77000 41 500 4 96) (76000 53 1500 3 96) (76000 60 1500 2 96) (76000 69 1500 1 96) (77500 43 500 4 96) (77500 50 500 3 96) (77500 59 500 2 96) (77500 67 500 1 96))) 
 GET-OTHER-CHANNELS returned ((76000 53 1500 3 96) (76000 60 1500 2 96) (76000 69 1500 1 96) (77500 50 500 3 96) (77500 59 500 2 96) (77500 67 500 1 96))|#
;;;;;

(defun GET-OTHER-CHANNELS (channel-not-to-get events)
  "Returns all but the first arg channeled events."
  (cond ((null events)())
        ((equal (fourth (first events)) channel-not-to-get)
         (get-other-channels channel-not-to-get (rest events)))
        (t (cons (first events)
                 (get-other-channels channel-not-to-get (rest events))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pitch utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;
#| Calling (CREATE-PITCH-CLASS-SET (64 67 71)) 
 CREATE-PITCH-CLASS-SET returned (4 7 11)|#
;;;;;

(defun CREATE-PITCH-CLASS-SET (pitches)
  "Sorts and gets a full pc-set."
  (my-sort #'< (remove-duplicates (create-pc-set pitches))))

;;;;;
#|  Calling (CREATE-PC-SET (64 67 71)) 
  CREATE-PC-SET returned (4 7 11)|#
;;;;;
; λ - A pitch class is a pitch modulo octaves.
; The pitch class 0 corresponds to a C in the MIDI specification.
;
(defun CREATE-PC-SET (pitches)
  "Creates a full PC set."
  (if (null pitches) ()
      (cons (mod (first pitches) 12)
            (create-pc-set (rest pitches)))))

;;;;;
#| Calling (triad? ((111000 40 500 4 96) (111000 55 500 3 96) (111000 64 1000 2 96) (111000 72 1000 1 96))) 
 triad? returned t|#
;;;;;

(defun TRIAD? (events)
  "Checks to see if the events are a triad."
  (let ((pitch-classes (get-smallest-set (create-pitch-class-set (get-pitches events)))))
    (and (equal (length pitch-classes) 3)
         (and (> (- (second pitch-classes)(first pitch-classes)) 2)
              (< (- (second pitch-classes)(first pitch-classes)) 5))
         (and (> (- (third pitch-classes)(second pitch-classes)) 2)
              (< (- (third pitch-classes)(second pitch-classes)) 5)))))

;;;;;
#|  Calling (get-smallest-set (0 4 7)) 
  get-smallest-set returned (0 4 7)|#
;;;;;

(defun GET-SMALLEST-SET (set)
  "Returns the set with the smallest outer boundaries."
  (let* ((projected-sets (project set))
         (set-differentials (get-intervals projected-sets)))
    (nth (position (first (my-sort #'< set-differentials)) set-differentials) projected-sets)))

;;;;;
#|   Calling (project (0 4 7)) 
   project returned ((0 4 7) (4 7 12) (7 12 16))|#
;;;;;

(defun PROJECT (set &optional (length (length set))(times 0))
  "Projects the pc-set through its inversions."
  (if (equal length times)()
      (cons set
            (project (append (rest set)(list (+ 12 (first set)))) length (1+ times)))))

;;;;;
#| Calling (get-intervals ((0 4 7) (4 7 12) (7 12 16))) 
 get-intervals returned (7 8 9)|#
;;;;;

(defun GET-INTERVALS (sets)
  "Returns the intervals in the sets."
  (if  (null sets)()
       (cons (abs (apply #'+ (get-interval (first sets))))
             (get-intervals (rest sets)))))

;;;;;
#|  Calling (get-interval (0 4 7)) 
  get-interval returned (4 3)|#
;;;;;

(defun GET-INTERVAL (set)
  "Returns the intervals between set members."
  (if (null (rest set))
    () (cons (- (second set)(first set))
             (get-interval (rest set)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Composition from a database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;
#| Calling (incf-beat b35300b-42) 
 incf-beat returned b35300b-3
|#
;;;;;
; λ - ?!
;     Apparently, only the last digit is incremented. This is probably a bug.

(defun INCF-BEAT (beat)
  "Increments the beat number."
  (implode (list (get-db-name beat) '- (1+ (my-last (explode beat))))))

;;;;;
#|  Calling (get-db-name b35300b-42) 
  get-db-name returned b35300b|#
;;;;;

(defun GET-DB-NAME (lexicon)
  "Returns the database name."
  (implode (get-db-n (explode lexicon))))

;;;;;
#|   Calling (get-db-n (b 3 5 3 0 0 b - 4 2)) 
   get-db-n returned (b 3 5 3 0 0 b)|#
;;;;;

(defun GET-DB-N (exploded-lexicon)
  "Checks for dashes in the db-name."
  (cond ((equal (first exploded-lexicon) '-)
         ())
        ((null exploded-lexicon)())
        (t (cons (first exploded-lexicon)
                 (get-db-n (rest exploded-lexicon))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Timing utilities for composition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;
#| Calling (REMOVE-FULL-BEAT ((77000 41 500 4 96) (77500 43 500 4 96))) 
 REMOVE-FULL-BEAT returned NIL|#
;;;;;

(defun REMOVE-FULL-BEAT (events &optional (begin-time (very-first events))(duration 0))
  "Removes one full beat from the events arg."
  (cond ((null events)())
        ((>= (+ duration (third (first events))) 1000)
         (rest events))
        (t (remove-full-beat (rest events)
                             (+ begin-time (third (first events)))
                             (+ (third (first events)) duration)))))

;;;;;
#| Calling (REMAINDER (76000 41 1500 4 96)) 
 REMAINDER returned ((77000 41 500 4 96))|#
;;;;;

(defun REMAINDER (event &optional (begin-time (first event))(duration (third event)))
  "Returns the remainder of the beat."
  (cond ((null event)())
        ((= duration 1000)())
        ((< duration 1000) (list (append (list begin-time)
                                         (list (second event))
                                         (list duration)
                                         (nthcdr 3 event))))
      (t (remainder event (+ begin-time 1000)(- duration 1000)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Close splash window
;; λ - no need to translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-menu-items *apple-menu* 
                (make-instance 'menu-item 
                  :menu-item-title "About Chorale..." 
                  :menu-item-action 
                  #'(lambda nil (make-instance 'all-about-window)
                                        ))
                (make-instance 'menu-item 
                  :menu-item-title "-"
                  :menu-item-action 
                  #'(lambda nil)))

(window-close *about-window*)
