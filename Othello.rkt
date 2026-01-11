;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Othello) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;; LIBRARY REQUIREMENTS ;;;;;;;;;;;;;;;;;;;;
(require picturing-programs)
(require racket/base)

;;;;;;;;;;;;;;;;;;;; UTILITY FUNCTIONS & DATA STRUCTS ;;;;;;;;;;;;;;;;;;;;
(define (posn-add p q)
  (make-posn (+ (posn-x p) (posn-x q))
             (+ (posn-y p) (posn-y q))))
(define (add-posns p q) (posn-add p q))
(define-struct piece (team loc))
(define-struct game (board player mouse))

;;;;;;;;;;;;;;;;;;;; BOARD & GRID SETUP ;;;;;;;;;;;;;;;;;;;;
(define (vblock n img)
  (if (= n 1) img (above img (vblock (- n 1) img))))
(define (hblock n img)
  (if (= n 1) img (beside img (hblock (- n 1) img))))
(define (grid x y img)
  (if (or (= x 1) (= y 1)) img (vblock y (hblock x img))))
(define board 
  (grid 8 8 (overlay (square 50 "outline" "black")
                     (square 50 "solid" "green"))))
(define (num->grid z)
  (make-posn (- (* 50 (posn-x z)) 25)
             (- (* 50 (posn-y z)) 25)))
(define (grid->num z)
  (make-posn (+ 1 (quotient (posn-x z) 50))
             (+ 1 (quotient (posn-y z) 50))))

;;;;;;;;;;;;;;;;;;;; DRAWING FUNCTIONS ;;;;;;;;;;;;;;;;;;;;
(define (piece->img p)
  (if (string=? (piece-team p) "white")
      (circle 20 "solid" "white")
      (circle 20 "solid" "black")))

(define (draw-all pieces img)
  (if (empty? pieces) img
      (place-image (piece->img (first pieces))
                   (posn-x (num->grid (piece-loc (first pieces))))
                   (posn-y (num->grid (piece-loc (first pieces))))
                   (draw-all (rest pieces) img))))

(define (dh-helper game)
  (draw-all (game-board game) board))

(define (team-placer mouse img player)
  (if (string=? player "white")
      (place-image (circle 20 "solid" (make-color 255 255 255 100))
                   (posn-x (num->grid mouse)) (posn-y (num->grid mouse)) img)
      (place-image (circle 20 "solid" (make-color 0 0 0 100))
                   (posn-x (num->grid mouse)) (posn-y (num->grid mouse)) img)))

(define (dh game)
  (team-placer (game-mouse game) (dh-helper game) (game-player game)))

;;;;;;;;;;;;;;;;;;;; BOARD LOGIC ;;;;;;;;;;;;;;;;;;;;
(define (in-bound? pos)
  (and (>= (posn-x pos) 1)
       (<= (posn-x pos) 8)
       (>= (posn-y pos) 1)
       (<= (posn-y pos) 8)))

(define (occupied? pos pieces)
  (cond
    [(empty? pieces) #false]
    [(and (= (posn-x (piece-loc (first pieces))) (posn-x pos))
          (= (posn-y (piece-loc (first pieces))) (posn-y pos))) #true]
    [else (occupied? pos (rest pieces))]))

(define (find-piece pieces pos)
  (cond
    [(empty? pieces) #false]
    [(and (= (posn-x (piece-loc (first pieces))) (posn-x pos))
          (= (posn-y (piece-loc (first pieces))) (posn-y pos)))
     (first pieces)]
    [else (find-piece (rest pieces) pos)]))

;;;;;;;;;;;;;;;;;;;; FLIPPING LOGIC ;;;;;;;;;;;;;;;;;;;;
(define (flanked-helper board player next dir accum)
  (if (not (in-bound? next)) empty
      (if (not (occupied? next board)) empty
          (if (string=? (piece-team (find-piece board next)) player)
              (if (empty? accum) empty (reverse accum))
              (flanked-helper board player (add-posns next dir) dir (cons next accum))))))

(define (flanked board player pos dir)
  (if (not (in-bound? (add-posns pos dir)))
      empty
      (if (not (occupied? (add-posns pos dir) board))
          empty
          (if (string=? (piece-team (find-piece board (add-posns pos dir))) player)
              empty
              (flanked-helper board player (add-posns pos dir) dir empty)))))

(define (flipper board player pos directions)
  (if (empty? directions)
      empty
      (append (flanked board player pos (first directions))
              (flipper board player pos (rest directions)))))

(define (legal-move? board player pos directions)
  (not (empty? (flipper board player pos directions))))

(define (flip-board board flip-positions new-team)
  (if (empty? board)
      empty
      (if (member (piece-loc (first board)) flip-positions)
          (cons (make-piece new-team (piece-loc (first board)))
                (flip-board (rest board) flip-positions new-team))
          (cons (first board)
                (flip-board (rest board) flip-positions new-team)))))

;;;;;;;;;;;;;;;;;;;; DIRECTIONS & CELLS ;;;;;;;;;;;;;;;;;;;;
(define directions (list (make-posn 0 1)
                         (make-posn -1 1)
                         (make-posn 0 -1)
                         (make-posn -1 -1)
                         (make-posn -1 0)
                         (make-posn 1 -1)
                         (make-posn 1 1)
                         (make-posn 1 0)))
(define blocks
  (for*/list ([i (in-range 1 9)]
              [j (in-range 1 9)])
    (make-posn i j)))

;;;;;;;;;;;;;;;;;;;; GAMEPLAY LOGIC ;;;;;;;;;;;;;;;;;;;;
(define (cont-play? board team cells)
  (cond
    [(empty? cells) #false]
    [else 
     (if (and (not (occupied? (first cells) board))
              (legal-move? board team (first cells) directions))
         #true
         (cont-play? board team (rest cells)))]))

(define (any-legal-move? board team)
  (cont-play? board team blocks))

(define (placer-helper game new-board new-pos)
  (define flips (flipper new-board (game-player game) new-pos directions))
  (if (empty? flips)
      game
      (make-game (flip-board new-board flips (game-player game))
                 (if (string=? (game-player game) "white") "black" "white")
                 new-pos)))

(define (placer game new-pos)
  (if (or (occupied? new-pos (game-board game))
          (not (legal-move? (game-board game) (game-player game) new-pos directions)))
      game
      (placer-helper game (cons (make-piece (game-player game) new-pos)
                                (game-board game))
                     new-pos)))

;;;;;;;;;;;;;;;;;;;; EVENT HANDLERS ;;;;;;;;;;;;;;;;;;;;
(define (mh game x y event)
  (if (string=? "button-down" event)
      (placer game (grid->num (make-posn x y)))
      game))

(define (tick game)
  (if (not (any-legal-move? (game-board game) (game-player game)))
      (make-game (game-board game)
                 (if (string=? (game-player game) "white") "black" "white")
                 (game-mouse game))
      game))

(define (stop? game)
  (or (>= (length (game-board game)) 64)
      (and (not (any-legal-move? (game-board game) "white"))
           (not (any-legal-move? (game-board game) "black")))))

(define (count-team pieces team)
  (if (empty? pieces)
      0
      (if (string=? (piece-team (first pieces)) team)
          (+ 1 (count-team (rest pieces) team))
          (count-team (rest pieces) team))))

(define (win-screen game)
  (define white-count (count-team (game-board game) "white"))
  (define black-count (count-team (game-board game) "black"))
  (cond
    [(> white-count black-count)
     (text (string-append "White wins! " (number->string white-count)
                          " to " (number->string black-count)) 40 "black")]
    [(> black-count white-count)
     (text (string-append "Black wins! " (number->string black-count)
                          " to " (number->string white-count)) 40 "black")]
    [else (text "Tie!" 40 "black")]))

;;;;;;;;;;;;;;;;;;;; BIG-BANG ;;;;;;;;;;;;;;;;;;;;
(big-bang 
 (make-game (list (make-piece "white" (make-posn 4 4))
                  (make-piece "white" (make-posn 5 5))
                  (make-piece "black" (make-posn 5 4))
                  (make-piece "black" (make-posn 4 5))) 
            "black" 
            (make-posn 1 1))
 (on-draw dh)
 (on-mouse mh)
 (on-tick tick)
 (stop-when stop? win-screen)
 (name "Othello"))

