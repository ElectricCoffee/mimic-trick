;;; Mimic Trick

(load "util.lisp")

(setf *random-state* (make-random-state t))

(defstruct card suit rank back)

(defun make-deck ()
     (let ((regulars (loop for suit in '(clubs spades hearts diamonds) nconc 
                        (loop for rank from 2 to 14 by 2 
                              collect (make-card :suit suit :rank rank :back suit))))
           (mimic (loop for rank from 1 to 15 by 2 
                        for back in '(clubs spades hearts diamonds diamonds spades hearts clubs)
                        collect (make-card :suit 'mimic :rank rank :back back))))
        (append regulars mimic)))

(defparameter *deck* (make-deck))

(defparameter *rounds* 6)

(defparameter *strategies* '(lowest-first highest-first
                              lowest-suit-first highest-suit-first
                              lowest-mimic-first highest-mimic-first))

(defparameter *game* nil)

;;; Player classes

(defclass player ()
  ((hand   :accessor player-hand   :initform nil)
   (tricks :accessor player-tricks :initform nil)
   (name   :accessor player-name   :initform nil :initarg :name)
   (score  :accessor player-score  :initform 0)))

(defclass human-player (player) ())

(defclass ai-player (player) 
  ((strategy :accessor ai-strategy :initform 'lowest-first)))

;;; Game class

(defclass game ()
  ((deck        :accessor game-deck    :initform nil)
   (trick       :accessor game-trick   :initform nil)
   (players     :accessor game-players :initform nil :initarg :players)
   (lead-player :accessor game-lead    :initform nil)))

;;; Game initialization

(defun deal (game hand-size)
  (loop for player in (game-players game)
        do (setf (player-hand player)
                 (loop repeat hand-size
                       collect (pop (game-deck game))))))

(defun assign-first-lead (game)
  "Randomly decides who the first lead player will be"
  (setf (game-lead game) (util:random-element (game-players game))))

(defun assign-strategies ()
  "Assigns a random strategy to each AI player"
  (loop for player in (game-players *game*)
        when (typep player 'ai-player)
        do (setf (ai-strategy player) (util:random-element *strategies*))))

(defun next-lead ()
  "Advances the lead player to the next player in the list"
  (let* ((players (game-players *game*))
         (pos (position (game-lead *game*) players))
         (next (mod (1+ pos) (length players))))
    (setf (game-lead *game*) (nth next players))))

(defun reset-round ()
  "Resets hands, tricks, and reshuffles the deck for a new round"
  (setf (game-deck *game*) (util:shuffle (make-deck)))
  (setf (game-trick *game*) nil)
  (loop for player in (game-players *game*)
        do (setf (player-hand player) nil)
           (setf (player-tricks player) nil))
  (deal *game* (/ (length *deck*) (length (game-players *game*)))))

(defun init-3p-game ()
  (setf *game* (make-instance 'game
                 :players (list (make-instance 'human-player :name "The Player")
                                (make-instance 'ai-player    :name "Alice")
                                (make-instance 'ai-player    :name "Bob"))))
  (assign-first-lead *game*)
  (assign-strategies)
  (reset-round))

(defun init-4p-game ()
  (setf *game* (make-instance 'game
                 :players (list (make-instance 'human-player :name "The Player")
                                (make-instance 'ai-player    :name "Alice")
                                (make-instance 'ai-player    :name "Bob")
                                (make-instance 'ai-player    :name "Charlie"))))
  (assign-first-lead *game*)
  (assign-strategies)
  (reset-round))

;;; Display

(defun format-suit (suit)
  (case suit
    (clubs "♣")
    (spades "♠")
    (hearts "♥")
    (diamonds "♦")
    (mimic "⊙")
    (otherwise (format nil "~A" suit))))

(defun format-card (card)
  (format nil "~A ~A" (card-rank card) (format-suit (card-suit card))))

(defun show-hand (player)
  (loop for card in (player-hand player)
        for i from 0
        do (format t "~A: ~A~%" (code-char (+ i (char-code #\a))) (format-card card))))

;;; Card Play

(defun mimicp (card)
  (eq (card-suit card) 'mimic))

(defun lead-suit (game) 
  "Finds the led suit in the trick. The lead suit is the first non-mimic card"
  (loop for card in (game-trick game)
        when (not (eq (card-suit card) 'mimic))
          return (card-suit card)))

(defun show-lead-suit (game)
  (let ((suit (lead-suit game)))
    (if (null suit)
      (format t "There is no lead suit. You may play anything.~%")
      (format t "The lead suit is ~A.~%" (format-suit suit)))))

(defun show-other-hands ()
  "Shows the card backs of other players' hands"
  (loop for player in (game-players *game*)
        when (not (typep player 'human-player))
        do (format t "~A's cards: ~{~A~^, ~}~%"
                   (player-name player)
                   (mapcar (lambda (c) (format-suit (card-back c))) (player-hand player)))))

(defun show-card-play-message (player card)
  (format t "~A plays a ~A.~%" (player-name player) (format-card card)))

(defun add-card-to-trick (player card) 
  "Blindly adds a card to the current trick. Make sure to remove it from the player's hand"
  (show-card-play-message player card)
  (setf (player-hand player) (remove card (player-hand player)))
  (util:push-back card (game-trick *game*)))

(defun cards-by-suit (hand suit)
  "Filters a hand to only cards matching the given suit."
  (remove-if-not (lambda (c) (eq (card-suit c) suit)) hand))

(defun lowest-card (cards)
  "Returns the card with the lowest rank."
  (reduce (lambda (a b) (if (<= (card-rank a) (card-rank b)) a b)) cards))

(defun highest-card (cards)
  "Returns the card with the highest rank."
  (reduce (lambda (a b) (if (>= (card-rank a) (card-rank b)) a b)) cards))

(defun trick-has-mimic-rank (rank)
  "Checks if a mimic of the given rank is in the current trick"
  (find-if (lambda (c) (and (mimicp c) (= (card-rank c) rank)))
           (game-trick *game*)))

(defun can-follow-suit (hand lead-suit)
  "Returns suit cards + mimics if any exist, nil otherwise."
  (when lead-suit
    (append (cards-by-suit hand lead-suit)
            (cards-by-suit hand 'mimic))))

(defun can-play-card (player card)
  "Checks if the chosen card can be played to the trick"
  (let ((lead-suit (lead-suit *game*)))
    (or (null lead-suit)
        (mimicp card)
        (eq lead-suit (card-suit card))
        (not (can-follow-suit (player-hand player) lead-suit)))))

(defun pick-card (hand lead-suit prefer-suit-first lowest)
  "Picks a card from playable cards. If prefer-suit-first, tries suit cards before mimics; otherwise mimics first.
   If lowest is t, picks the lowest rank; otherwise the highest."
  (let* ((suit-cards (cards-by-suit hand lead-suit))
         (mimics (cards-by-suit hand 'mimic))
         (selector (if lowest #'lowest-card #'highest-card))
         (first-pool (if prefer-suit-first suit-cards mimics))
         (second-pool (if prefer-suit-first mimics suit-cards)))
    (cond (first-pool  (funcall selector first-pool))
          (second-pool (funcall selector second-pool))
          (t           (funcall selector hand)))))

(defgeneric play-card (player)
  (:documentation "Plays a card to the trick. Humans will be prompted, AI chooses by itself"))

(defun mimic-1-counter-p (hand)
  "Returns the mimic 1 card from hand if the mimic 15 is in the current trick"
  (when (trick-has-mimic-rank 15)
    (find-if (lambda (c) (and (mimicp c) (= (card-rank c) 1))) hand)))

(defun mimic-15-dump-p (hand)
  "Returns the mimic 15 card from hand if the mimic 1 is in the current trick (safe to dump)"
  (when (trick-has-mimic-rank 1)
    (find-if (lambda (c) (and (mimicp c) (= (card-rank c) 15))) hand)))

(defmethod play-card ((player ai-player))
  (let* ((hand (player-hand player))
         (lead (lead-suit *game*))
         (on-suit (can-follow-suit hand lead))
         (counter (mimic-1-counter-p hand))
         (dump (mimic-15-dump-p hand))
         (card (cond
                 ;; High strategies: prefer mimic 1 if 15 is in the trick
                 ((and counter on-suit
                       (member (ai-strategy player)
                               '(highest-first highest-suit-first highest-mimic-first)))
                  counter)
                 ;; Low strategies: dump mimic 15 if 1 is in the trick
                 ((and dump on-suit
                       (member (ai-strategy player)
                               '(lowest-first lowest-suit-first lowest-mimic-first)))
                  dump)
                 ;; Normal on-suit play
                 (on-suit
                  (case (ai-strategy player)
                    (lowest-first        (lowest-card on-suit))
                    (highest-first       (highest-card on-suit))
                    (lowest-suit-first   (pick-card on-suit lead t t))
                    (highest-suit-first  (pick-card on-suit lead t nil))
                    (lowest-mimic-first  (pick-card on-suit lead nil t))
                    (highest-mimic-first (pick-card on-suit lead nil nil))))
                 ;; Off-suit: dump in the opposite direction
                 (t (case (ai-strategy player)
                      ((lowest-first lowest-suit-first lowest-mimic-first)
                       (highest-card hand))
                      ((highest-first highest-suit-first highest-mimic-first)
                       (lowest-card hand)))))))
    (add-card-to-trick player card)))

(defmethod play-card ((player human-player))
  (show-other-hands)
  (show-lead-suit *game*)
  (show-hand player)
  (handler-case
    (let* ((input (util:prompt "Pick a card: "))
           (index (- (char-code (char input 0)) (char-code #\a)))
           (card (nth index (player-hand player))))
      (cond ((or (< index 0) (>= index (length (player-hand player))))
             (format t "Invalid choice.~%")
             (play-card player))
            ((not (can-play-card player card))
             (format t "Cannot play ~A to trick. Doesn't match lead suit.~%" (format-card card))
             (play-card player))
            (t (add-card-to-trick player card))))
    (error ()
      (format t "Invalid input.~%")
      (play-card player))))

(defun resolve-trick ()
  "Returns the index of the card in the trick with the highest lead suit value (mimic or otherwise).
   Special rule: if both mimic 1 and mimic 15 are in the trick, mimic 1 wins."
  (let* ((lead-suit (lead-suit *game*))
         (trick (game-trick *game*))
         (one-beats-fifteen (and (trick-has-mimic-rank 1)
                                 (trick-has-mimic-rank 15)))
         (best-idx 0)
         (best-rank -1))
    (loop for card in trick
          for i from 0
          when (or (eq (card-suit card) lead-suit)
                   (eq (card-suit card) 'mimic))
          do (let ((rank (if (and one-beats-fifteen (mimicp card) (= (card-rank card) 1))
                             16
                             (card-rank card))))
               (when (> rank best-rank)
                 (setf best-rank rank)
                 (setf best-idx i))))
    best-idx))

(defun play-trick ()
  "Runs through the list of players starting with the lead player.
   Sets the trick winner to the new lead player and pushes the trick to the trick winner's trick pool."
  ;; rotate the player list to put the lead player on top
  (let ((players (util:rotate-to (game-lead *game*) (game-players *game*))))
    (loop for player in players do (play-card player)) ; core logic
    (let* ((winner-idx (resolve-trick))
           (trick-winner (nth winner-idx players)))
      (format t "~A wins the trick.~%-----~%" (player-name trick-winner))
      (setf (game-lead *game*) trick-winner)
      (push (game-trick *game*) (player-tricks trick-winner))
      (setf (game-trick *game*) nil))))

(defun count-mimics (trick)
  "Counts the number of mimics in a trick"
  (loop for card in trick count (eq 'mimic (card-suit card))))

(defun eval-player-points (player)
  "Evaluates the tricks taken and gives back the points"
  (let* ((tricks (player-tricks player))
         (trick-count (length tricks))
         (mimic-count (loop for trick in tricks sum (count-mimics trick))))
    (if (= 8 mimic-count)
      (+ trick-count 11)
      (- trick-count mimic-count))))

(defun max-name-length ()
  (loop for player in (game-players *game*)
        maximize (length (player-name player))))

(defun show-round-breakdown (player name-width)
  "Shows how a player's round points were calculated"
  (let* ((tricks (player-tricks player))
         (trick-count (length tricks))
         (mimic-count (loop for trick in tricks sum (count-mimics trick)))
         (points (eval-player-points player)))
    (format t "~vA  ~2D tricks, ~D mimics -> ~3D points~A~%"
            name-width (player-name player)
            trick-count mimic-count points
            (if (= 8 mimic-count) " (all mimics!)" ""))))

(defun show-round-summary ()
  "Shows the round breakdown for all players"
  (let ((width (max-name-length)))
    (format t "~%")
    (loop for player in (game-players *game*)
          do (show-round-breakdown player width))))

(defun add-player-points (player)
  "Adds the trick points to the player's score"
  (let ((points (eval-player-points player)))
    (with-slots (score) player
      (setf score (+ score points)))))

(defun show-scores ()
  "Displays the current scores for all players"
  (format t "~%--- Scores ---~%")
  (loop for player in (game-players *game*)
        do (format t "~A: ~A~%" (player-name player) (player-score player)))
  (format t "--------------~%"))

(defun find-winner ()
  "Returns the player with the highest score"
  (reduce (lambda (a b) (if (>= (player-score a) (player-score b)) a b))
          (game-players *game*)))

(defun play-round ()
  "Plays one hand of the game and evaluates the points"
  (let ((player-count (length (game-players *game*))))
    (loop for _ below (length *deck*) by player-count do
      (play-trick)))
  (show-round-summary)
  (loop for player in (game-players *game*) do
    (add-player-points player)))

(defun play-game ()
  "Main game loop. Plays 6 rounds then declares a winner."
  (loop for round from 1 to *rounds*
        do (format t "~%=== Round ~A ===~%" round)
           (when (> round 1)
             (next-lead)
             (reset-round))
           (play-round)
           (show-scores))
  (let ((winner (find-winner)))
    (format t "~%~A wins with ~A points!~%"
            (player-name winner) (player-score winner))))

(defun start-3p-game () 
  (init-3p-game)
  (play-game))

(defun start-4p-game ()
  (init-4p-game)
  (play-game))
