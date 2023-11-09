(require 2htdp/universe)
(require 2htdp/image)

; A Owner is one of:
; - (make-none)
; - (make-player Number)
; - (make-draw)

(define-struct none [])
(define-struct player (id))
(define-struct draw [])

; A Block is (make-block Number Number Owner)
(define-struct block (x y owner))

; A cursor-block is (make-cursor-block Number Number)
(define-struct cursor-block (x y))

; A World is (make-world block block block
;                        block block block
;                        block block block
;                        block Owner)

(define-struct world (tl tm tr
                         ml mm mr
                         bl bm br
                         cursor-block
                         winner))

(define world1 (make-world (make-block 50 50 (make-none))
                           (make-block 150 50 (make-none))
                           (make-block 250 50 (make-none))
                           (make-block 50 150 (make-none))
                           (make-block 150 150 (make-none))
                           (make-block 250 150 (make-none))
                           (make-block 50 250 (make-none))
                           (make-block 150 250 (make-none))
                           (make-block 250 250 (make-none))
                           (make-cursor-block 50 50)
                           (make-none)))

; A Background is an Image
(define background
  (above
   (beside (square 100 "outline" "gray")
           (square 100 "outline" "gray")
           (square 100 "outline" "gray")
           )
   (beside (square 100 "outline" "gray")
           (square 100 "outline" "gray")
           (square 100 "outline" "gray"))
   (beside (square 100 "outline" "gray")
           (square 100 "outline" "gray")
           (square 100 "outline" "gray")
           )))

; A p1 is an Image
(define p1 (circle 40 "outline"  "green"))

; A p2 is an Image
(define p2 (overlay (line 80 80 "red")
                    (line -80 80 "red")))

(define cursor (square 80 "outline" "red"))

#| DRAW FUNCTIONS |# 

; draw-block : Block Image -> Image
; takes a block and draws the owner on top of the given image
; if the owner doesn't exist, image stays the same.

(define (draw-block b i)
  (cond [(none? (block-owner b)) i]
        [(player? (block-owner b))
         (cond [(= 0 (player-id (block-owner b)))
                (place-image p1
                             (block-x b)
                             (block-y b) i)]
               [else (place-image p2
                                  (block-x b)
                                  (block-y b) i)])]))

; winner-background
(define winner-background
  (overlay (rectangle 100 50 "solid" "maroon")
           (square 210 "solid" "purple")
           (square 230 "solid" "pink")
           (square 250 "solid" "green")
           (square 270 "solid" "red")
           (square 300 "solid" "black")))

; draw-winner : World Image -> Image
; takes a World and an Image, if there is a winner
; will return a winner background with winner name or return
; given image as is.
  
(define (draw-winner w i)
  (cond [(none? (world-winner w)) i]
        [(draw? (world-winner w))
         (overlay (above (text
                          "TIE!" 14 "white")
                         (text "Wins!" 14 "white")) 
                  winner-background)]
        [else 
         (overlay (above (text
                          (string-append
                           (number->string
                            (player-id
                             (world-winner w)))) 14 "white")
                         (text "Wins!" 14 "white")) 
                  winner-background)])) 

; draw-world : World -> Image
; takes in a World and draws it as an image
; draw-winner gets called incase there's a winner in the World.
  
(define (draw-world w)
  (cond [(player? (world-winner w))
         (overlay (above
                   (text
                    (string-append "Player "
                                   (number->string
                                    (player-id
                                     (world-winner w)))) 14 "white")
                   (text "Wins!" 14 "white")) 
                  winner-background)]
        [(draw? (world-winner w))
         (overlay (text
                   "IT'S A TIE!" 14 "white") 
                  winner-background)]
        [(none? (world-winner w))
         (red-box w
                  (draw-block
                   (world-tl w)
                   (draw-block
                    (world-tm w)
                    (draw-block
                     (world-tr w)
                     (draw-block
                      (world-ml w) 
                      (draw-block
                       (world-mm w)
                       (draw-block
                        (world-mr w)
                        (draw-block
                         (world-bl w)
                         (draw-block
                          (world-bm w)
                          (draw-block
                           (world-br w) background))))))))))]))
 
; red-box: Block Image -> Image
; takes a block and a background, places the cursor on the
; given block correspended to the background.

(define (red-box w i)
  (place-image cursor (cursor-block-x (world-cursor-block w))
               (cursor-block-y (world-cursor-block w)) i))

#| GAME FUNCTIONS |#

; world-cursor : World Cursor-Block -> World
; takes a World and a Cursor-Block, replaces the cursor-block of the
; given world.

(define (world-cursor cb w)
  (make-world (world-tl w)(world-tm w)(world-tr w)
              (world-ml w)(world-mm w)(world-mr w)
              (world-bl w)(world-bm w)(world-br w)
              cb (world-winner w)))

; blocks-winner : ListOfBlocks -> Boolean
; gets a ListOfBlocks and returns a boolean
; whether the blocks are the same players

(define (blocks-winner lob)
  (cond [(or (none? (first lob))
             (none? (second lob))
             (none? (third lob))) #f]
        [else (= (player-id (first lob)) 
                 (player-id (second lob))
                 (player-id (third lob)))])) 

; owners : World -> [ListOf Owners]
; takes a World and returns a list of Owners.

(define (owners w)
  (list (block-owner (world-tl w))
        (block-owner (world-tm w))
        (block-owner (world-tr w))
        (block-owner (world-ml w))
        (block-owner (world-mm w))
        (block-owner (world-mr w))
        (block-owner (world-bl w))
        (block-owner (world-bm w))
        (block-owner (world-br w))))

; count-players : [ListOf Owners] -> Number
; takes a list of owners and returns how many players are in that list

(define (count-players lop)
  (cond [(empty? lop) 0]
        [else (if (none? (first lop))
                  (count-players (rest lop))
                  (add1 (count-players (rest lop))))])) 

; check-winner : World -> Player
; takes a World and returns a Player indicating who won
; none for nobody, Player for a specific player.
  
(define (check-winner w)
  (let* ([player-tl (block-owner (world-tl w))]
         [player-tm (block-owner (world-tm w))]
         [player-tr (block-owner (world-tr w))]
         [player-ml (block-owner (world-ml w))]
         [player-mm (block-owner (world-mm w))]
         [player-mr (block-owner (world-mr w))]
         [player-bl (block-owner (world-bl w))]
         [player-bm (block-owner (world-bm w))]
         [player-br (block-owner (world-br w))])
     
    (begin
      (cond
        [(blocks-winner (list player-tl
                              player-tm
                              player-tr))
         player-tl]
        [(blocks-winner (list player-tl
                              player-ml
                              player-bl))
         player-tl]
        [(blocks-winner (list player-tm
                              player-mm
                              player-bm))
         player-tm]
        [(blocks-winner (list player-tr
                              player-mr
                              player-br))
         player-tr] 
        [(blocks-winner (list player-tl
                              player-mm
                              player-br)) 
         player-tl]
        [(blocks-winner (list player-ml
                              player-mm
                              player-mr))
         player-ml]
        [(blocks-winner (list player-bl
                              player-bm
                              player-br))
         player-bl]
        [(blocks-winner (list player-bl
                              player-mm
                              player-tr))
         player-bl]
        [(blocks-winner (list player-br
                              player-mm
                              player-tl))
         player-br]
        [(blocks-winner (list player-tl
                              player-tm
                              player-tr))
         player-tl]
        [else (if (>= (count-players (owners w)) 9)
                  (make-draw)
                  (make-none))]))))

; replace-winner : World Player -> World
; takes a World and a Player, replaces the given's worlds
; winner with the given player

(define (replace-winner w p)
  (make-world (world-tl w)(world-tm w)(world-tr w)
              (world-ml w)(world-mm w)(world-mr w)
              (world-bl w)(world-bm w)(world-br w)
              (world-cursor-block w)
              p)) 

; owner-box : World Player -> World 
; takes a World and a Player, whatever the curser-box of the
; given World is, correlates the curser-box's x and y value to the
; actual box, changes its owner to given Player

(define (owner-box w p)
  (let* ([curs (world-cursor-block w)]
         [x (cursor-block-x curs)]
         [y (cursor-block-y curs)])
    (if (or (player? (check-winner w))
            (draw? (check-winner w)))
        w
        (cond [(= y 50)
               (cond [(= x 50)
                      (let* ([block (world-tl w)]
                             [x (block-x block)]
                             [y (block-y block)])
                        (cond [(none? (block-owner block))
                               (make-world
                                (make-block x y p)
                                (world-tm w)
                                (world-tr w)
                                (world-ml w)(world-mm w)(world-mr w)
                                (world-bl w)(world-bm w)(world-br w)
                                (world-cursor-block w)
                                (world-winner w))]
                              [else (begin
                                      (if (even? turn)
                                          (set! turn 1)
                                          (set! turn 0))
                                      w)]))]
                     [(= x 150)
                      (let* ([block (world-tm w)]
                             [x (block-x block)]
                             [y (block-y block)])
                        (cond [(none? (block-owner block))
                               (make-world
                                (world-tl w)
                                (make-block x y p)
                                (world-tr w)
                                (world-ml w)(world-mm w)(world-mr w)
                                (world-bl w)(world-bm w)(world-br w)
                                (world-cursor-block w)
                                (world-winner w))]
                              [else (begin
                                      (if (even? turn)
                                          (set! turn 1)
                                          (set! turn 0))
                                      w)]))]
                     [(= x 250)
                      (let* ([block (world-tr w)]
                             [x (block-x block)]
                             [y (block-y block)])
                        (cond [(none? (block-owner block))
                               (make-world
                                (world-tl w)
                                (world-tm w)
                                (make-block x y p)
                                (world-ml w)(world-mm w)(world-mr w)
                                (world-bl w)(world-bm w)(world-br w)
                                (world-cursor-block w)
                                (world-winner w))]
                              [else (begin
                                      (if (even? turn)
                                          (set! turn 1)
                                          (set! turn 0))
                                      w)]))])] 
              [(= y 150)
               (cond [(= x 50)
                      (let* ([block (world-ml w)]
                             [x (block-x block)]
                             [y (block-y block)])
                        (cond [(none? (block-owner block))
                               (make-world
                                (world-tl w)
                                (world-tm w)(world-tr w)
                                (make-block x y p)
                                (world-mm w)(world-mr w)
                                (world-bl w)(world-bm w)(world-br w)
                                (world-cursor-block w)
                                (world-winner w))]
                              [else (begin
                                      (if (even? turn)
                                          (set! turn 1)
                                          (set! turn 0))
                                      w)]))]
                     [(= x 150)
                      (let* ([block (world-mm w)]
                             [x (block-x block)]
                             [y (block-y block)])
                        (cond [(none? (block-owner block))
                               (make-world
                                (world-tl w)
                                (world-tm w)(world-tr w)
                                (world-ml w)
                                (make-block x y p)(world-mr w)
                                (world-bl w)(world-bm w)(world-br w)
                                (world-cursor-block w)
                                (world-winner w))]
                              [else (begin
                                      (if (even? turn)
                                          (set! turn 1)
                                          (set! turn 0))
                                      w)]))]
                     [(= x 250)
                      (let* ([block (world-mr w)]
                             [x (block-x block)]
                             [y (block-y block)])
                        (cond [(none? (block-owner block))
                               (make-world
                                (world-tl w)(world-tm w)(world-tr w)
                                (world-ml w)(world-mm w)
                                (make-block x y p)
                                (world-bl w)(world-bm w)(world-br w)
                                (world-cursor-block w)
                                (world-winner w))]
                              [else (begin
                                      (if (even? turn)
                                          (set! turn 1)
                                          (set! turn 0))
                                      w)]))])]
              [(= y 250)
               (cond [(= x 50) 
                      (let* ([block (world-bl w)]
                             [x (block-x block)]
                             [y (block-y block)])
                        (cond [(none? (block-owner block))
                               (make-world
                                (world-tl w)(world-tm w)(world-tr w)
                                (world-ml w)(world-mm w)(world-mr w)
                                (make-block x y p)
                                (world-bm w)(world-br w)
                                (world-cursor-block w)
                                (world-winner w))]
                              [else (begin
                                      (if (even? turn)
                                          (set! turn 1)
                                          (set! turn 0))
                                      w)]))]
                     [(= x 150) 
                      (let* ([block (world-bm w)]
                             [x (block-x block)]
                             [y (block-y block)])
                        (cond [(none? (block-owner block))
                               (make-world
                                (world-tl w)(world-tm w)(world-tr w)
                                (world-ml w)(world-mm w)(world-mr w)
                                (world-bl w)(make-block x y p)
                                (world-br w)
                                (world-cursor-block w)
                                (world-winner w))]
                              [else (begin
                                      (if (even? turn)
                                          (set! turn 1)
                                          (set! turn 0))
                                      w)]))]
                     [(= x 250)
                      (let* ([block (world-br w)]
                             [x (block-x block)]
                             [y (block-y block)])
                        (cond [(none? (block-owner block))
                               (make-world
                                (world-tl w)(world-tm w)(world-tr w)
                                (world-ml w)(world-mm w)(world-mr w)
                                (world-bl w)(world-bm w)
                                (make-block x y p)
                                (world-cursor-block w)
                                (world-winner w))]
                              [else (begin
                                      (if (even? turn)
                                          (set! turn 1)
                                          (set! turn 0))
                                      w)]))])]))))  

; Turn is an Integer indicating player's turn

(define turn 0)

; generate-player: Integer -> Player
; takes an Integer from generate-turn and returns the player
; correspondent to the given Integer

(define (generate-player t)
  (cond [(even? turn)
         (begin
           (set! turn 1)
           (make-player 0))]
        [else
         (begin
           (set! turn 0)
           (make-player 1))]))
 
#| KEY FUNCTIONS |#

; check-key : KeyEvent World -> World
; takes a KeyEvent and a World, updates the given World
; depending on the key pressed.
  
(define (check-key w ke)
  (cond [(key=? ke "up")
             (if (= (cursor-block-y (world-cursor-block w)) 50)
                 w
                 (world-cursor
                  (make-cursor-block
                   (cursor-block-x (world-cursor-block w))
                   (- (cursor-block-y
                       (world-cursor-block w)) 100))
                  w))]
            [(key=? ke "down") 
             (if (= (cursor-block-y (world-cursor-block w)) 250)
                 w
                 (world-cursor
                  (make-cursor-block
                   (cursor-block-x (world-cursor-block w))
                   (+ (cursor-block-y
                       (world-cursor-block w)) 100))
                  w))]
            [(key=? ke "left")
             (if (= (cursor-block-x (world-cursor-block w)) 50)
                 w
                 (world-cursor
                  (make-cursor-block
                   (- (cursor-block-x
                       (world-cursor-block w)) 100)
                   (cursor-block-y (world-cursor-block w))) 
                  w))]
            [(key=? ke "right")
             (if (= (cursor-block-x (world-cursor-block w)) 250)
                 w
                 (world-cursor
                  (make-cursor-block
                   (+ (cursor-block-x
                       (world-cursor-block w)) 100)
                   (cursor-block-y (world-cursor-block w)))
                  w))] 
            [(key=? ke " ")
             (owner-box w (generate-player turn))]
            [else w])) 
 
; mouse : World Integer Integer MouseEvent -> World
; takes a world, x and y coordinate, and a MouseEvent and edits
; the world depending on the mouse event.
   
(define (mouse w x y me)
  (if (or (player? (check-winner w))
          (draw? (check-winner w)))
      (replace-winner w (check-winner w))
      (cond [(<= 0 x 90)
             (cond [(<= 0 y 90)
                    (cond
                      [(mouse=? me "button-down")
                       (if (player? (check-winner w))
                           (replace-winner w (check-winner w))
                           (owner-box w
                                      (generate-player turn)))]
                      [else
                       (world-cursor
                        (make-cursor-block 50 50) w)])]
                   [(<= 110 y 190)
                    (cond
                      [(mouse=? me "button-down")
                       (if (player? (check-winner w))
                           (replace-winner w (check-winner w))
                           (owner-box w
                                      (generate-player turn)))]
                      [else (world-cursor
                             (make-cursor-block 50 150) w)])]
                   [(<= 210 y 290)
                    (cond
                      [(mouse=? me "button-down")
                       (if (player? (check-winner w))
                           (replace-winner w (check-winner w))
                           (owner-box w
                                      (generate-player turn)))]
                      [else (world-cursor
                             (make-cursor-block 50 250) w)])]
                   [else w])]
            [(<= 110 x 190)
             (cond [(<= 0 y 90)
                    (cond
                      [(mouse=? me "button-down")
                       (if (player? (check-winner w))
                           (replace-winner w
                                           (check-winner w))
                           (owner-box w
                                      (generate-player turn)))]
                      [else (world-cursor
                             (make-cursor-block 150 50) w)])]
                   [(<= 110 y 190)
                    (cond
                      [(mouse=? me "button-down")
                       (if (player? (check-winner w))
                           (replace-winner w
                                           (check-winner w))
                           (owner-box w
                                      (generate-player turn)))]
                      [else (world-cursor
                             (make-cursor-block 150 150) w)])]
                   [(<= 210 y 290)
                    (cond
                      [(mouse=? me "button-down")
                       (if (player? (check-winner w))
                           (replace-winner w (check-winner w))
                           (owner-box w
                                      (generate-player turn)))]
                      [else (world-cursor
                             (make-cursor-block 150 250) w)])]
                   [else w])]
            [(<= 210 x 290)
             (cond
               [(<= 0 y 90)
                (cond
                  [(mouse=? me "button-down")
                   (if (player? (check-winner w))
                       (replace-winner w (check-winner w))
                       (owner-box w
                                  (generate-player turn)))]
                  [else (world-cursor
                         (make-cursor-block 250 50) w)])]
               [(<= 110 y 190)
                (cond
                  [(mouse=? me "button-down")
                   (if (player? (check-winner w))
                       (replace-winner
                        w
                        (check-winner w))
                       (owner-box w
                                  (generate-player turn)))]
                  [else (world-cursor
                         (make-cursor-block 250 150) w)])]
               [(<= 210 y 290)
                (cond
                  [(mouse=? me "button-down")
                   (if (player? (check-winner w))
                       (replace-winner w (check-winner w))
                       (owner-box w (generate-player turn)))]
                  [else (world-cursor
                         (make-cursor-block 250 250) w)])]
               [else w])]
            [(mouse=? me "button-down")
             (owner-box w (generate-player turn))] 
            [else w])))

(define (who-won? w)
  (make-world
   (world-tl w)(world-tm w)(world-tr w)
   (world-ml w)(world-mm w)(world-mr w)
   (world-bl w)(world-bm w)(world-br w)
   (world-cursor-block w)
   (check-winner w))) 
        
(big-bang world1 
  [to-draw draw-world]
  [on-key check-key]
  [on-mouse mouse]
  [on-tick who-won?]) 

















