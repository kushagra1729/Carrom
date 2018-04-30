#lang racket

(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)
(define states 'main-menu)
(define red (circle 10 "solid" "crimson"))
(define black (circle 10 "solid" "black"))
(define white (circle 10 "solid" "navajowhite"))
(define blue (circle 10 "solid" "blue"))
(define coin-m 23) ;decide later
(define striker-m 32) ;decide later
(define coin-r 10) 
(define striker-r 12.5) 
(define coin-coeff 5) ;decide later
(define striker-coeff 5) ;decide later
(define pocket-r 16) 
(define max-power 1000)

(define x0 20)
(define y0 20)
(define x1 520)
(define y1 520)
(define midx (/ (+ x0 x1) 2))
(define midy (/ (+ y0 y1) 2))
(define edges (* 0.1 (- x1 x0))) 
(define e-wall 0.7) ;decide later
(define e-discs 0.9) ;decide later
(define pockets (list (cons (+ x0 pocket-r) (+ y0 pocket-r)) (cons (- x1 pocket-r) (+ y0 pocket-r))
                      (cons (+ x0 pocket-r) (- y1 pocket-r)) (cons (- x1 pocket-r) (- y1 pocket-r))))                                      


(define mode 0)
(define turn 0)
(define time 0)
(define static 0.5) ; changed by Kushagra
(define del-t (/ 1 50))
(define g 9.81)


(define-syntax myfor
  (syntax-rules (:)
    [(myfor init : condition : step : statements)
     (begin init (define (iter) (cond [condition (begin statements step (iter))])) (iter))]))



(struct state (str coi str-o coi-o qt nqt coi-t scr turn-end) #:transparent)

(struct disc (color mass radius coeff x y vx vy) #:transparent #:mutable)

(define striker (disc blue striker-m striker-r striker-coeff midx (+ y0 edges) 0 0))

(define coins (build-vector 19 (lambda (z) (disc (if (= z 0) red (if (= 0 (remainder z 2)) black white))
                                                 coin-m coin-r coin-coeff
                                                 (+ midx (if (= z 0) 0 (if (< z 7) (* 2 coin-r (cos (* z (/ pi 3))))
                                                                           (* 2 (if (= 0 (remainder z 2)) (sqrt 3) 2) coin-r
                                                                              (cos (* (- z 7) (/ pi 6)))))))
                                                 (+ midy (if (= z 0) 0 (if (< z 7) (* 2 coin-r (sin (* z (/ pi 3))))
                                                                           (* 2 (if (= 0 (remainder z 2)) (sqrt 3) 2) coin-r
                                                                              (sin (* (- z 7) (/ pi 6)))))))
                                                 0 0))))

(define (magn a b)
  (sqrt (+ (* a a) (* b b))))



(define (wall-collision disc1) 
  (cond[(> (disc-x disc1) (- x1 (disc-radius disc1))) (begin  (set-disc-x! disc1 (- (* (+ 1 e-wall) (- x1 (disc-radius disc1))) (* e-wall (disc-x disc1))))
                                                              (set-disc-vx! disc1 (* -1 (disc-vx disc1) e-wall)))]
       [(> (disc-y disc1) (- y1 (disc-radius disc1))) (begin  (set-disc-y! disc1 (- (* (+ 1 e-wall) (- y1 (disc-radius disc1))) (* e-wall (disc-y disc1))))
                                                              (set-disc-vy! disc1 (* -1 (disc-vy disc1) e-wall)))]
       [(< (disc-x disc1) (+ x0 (disc-radius disc1))) (begin  (set-disc-x! disc1 (- (* (+ 1 e-wall) (+ x0 (disc-radius disc1))) (* e-wall (disc-x disc1))))
                                                              (set-disc-vx! disc1 (* -1 (disc-vx disc1) e-wall)))]
       [(< (disc-y disc1) (+ y0 (disc-radius disc1))) (begin  (set-disc-y! disc1 (- (* (+ 1 e-wall) (+ y0 (disc-radius disc1))) (* e-wall (disc-y disc1))))
                                                              (set-disc-vy! disc1 (* -1 (disc-vy disc1) e-wall)))]
       [else '()]))

(define (collision disc1 disc2)
  (begin
    (set-disc-x! disc1 (- (disc-x disc1) (* del-t (disc-vx disc1))))
    (set-disc-x! disc2 (- (disc-x disc2) (* del-t (disc-vx disc2))))
    (set-disc-y! disc1 (- (disc-y disc1) (* del-t (disc-vy disc1))))
    (set-disc-y! disc2 (- (disc-y disc2) (* del-t (disc-vy disc2))))
    

    (define d0 (magn (- (disc-x disc2) (disc-x disc1)) (- (disc-y disc2) (disc-y disc1))))
    (define cos0 (/ (- (disc-x disc2) (disc-x disc1)) d0))
    (define sin0 (/ (- (disc-y disc2) (disc-y disc1)) d0))
    (define h10 (+ (* (disc-vx disc1) cos0) (* (disc-vy disc1) sin0)))
    (define h20 (+ (* (disc-vx disc2) cos0) (* (disc-vy disc2) sin0)))
    (define hp10 (- (* (disc-vy disc1) cos0) (* (disc-vx disc1) sin0)))
    (define hp20 (- (* (disc-vy disc2) cos0) (* (disc-vx disc2) sin0)))
    (define relvel (magn (- h10 h20) (- hp10 hp20))) 
    (define sin2t (with-handlers [(exn:fail:contract:divide-by-zero? (lambda (exn) 0))]
                    (/ (* (- hp10 hp20) (- hp10 hp20)) (* relvel relvel))))
    
    (define cost (with-handlers [(exn:fail:contract:divide-by-zero? (lambda (exn) 0))]
                   (/ (abs (- h10 h20)) relvel)))
    
    (define d1 (+ (disc-radius disc1) (disc-radius disc2)))
    (define dc (sqrt (- (+ (* d1 d1) (* d0 d0)) (+ (* 2 d0 d0 sin2t) (* 2 d0 cost (sqrt (- (* d1 d1) (* d0 d0 sin2t))))))))
    (define tc (with-handlers [(exn:fail:contract:divide-by-zero? (lambda (exn) 0))]
                 (/ dc relvel)))

    (set-disc-x! disc1 (+ (disc-x disc1) (* tc (disc-vx disc1))))
    (set-disc-x! disc2 (+ (disc-x disc2) (* tc (disc-vx disc2))))
    (set-disc-y! disc1 (+ (disc-y disc1) (* tc (disc-vy disc1))))
    (set-disc-y! disc2 (+ (disc-y disc2) (* tc (disc-vy disc2))))
    
    (define cos1 (/ (- (disc-x disc2) (disc-x disc1)) d1))
    (define sin1 (/ (- (disc-y disc2) (disc-y disc1)) d1))
    (define h11 (+ (* (disc-vx disc1) cos1) (* (disc-vy disc1) sin1)))
    (define h21 (+ (* (disc-vx disc2) cos1) (* (disc-vy disc2) sin1)))
    (define hp11 (- (* (disc-vy disc1) cos1) (* (disc-vx disc1) sin1)))
    (define hp21 (- (* (disc-vy disc2) cos1) (* (disc-vx disc2) sin1)))
    

    (define u1 (/ (- (+ (* (disc-mass disc1) h11) (* (disc-mass disc2) h21)) (* (disc-mass disc2) e-discs (- h11 h21))) (+ (disc-mass disc1) (disc-mass disc2))))
    (define u2 (/ (+ (* (disc-mass disc1) h11) (* (disc-mass disc2) h21) (* (disc-mass disc1) e-discs (- h11 h21))) (+ (disc-mass disc1) (disc-mass disc2))))
  
   
    (set-disc-vx! disc1 (- (* u1 cos1) (* sin1 hp11)))
    (set-disc-vy! disc1 (+ (* u1 sin1) (* hp11 cos1)))
    (set-disc-vx! disc2 (- (* u2 cos1) (* sin1 hp21)))
    (set-disc-vy! disc2 (+ (* u2 sin1) (* hp21 cos1)))

    (set-disc-x! disc1 (+ (disc-x disc1) (* (- del-t tc) (disc-vx disc1))))
    (set-disc-x! disc2 (+ (disc-x disc2) (* (- del-t tc) (disc-vx disc2))))
    (set-disc-y! disc1 (+ (disc-y disc1) (* (- del-t tc) (disc-vy disc1))))
    (set-disc-y! disc2 (+ (disc-y disc2) (* (- del-t tc) (disc-vy disc2))))
    
    ))



(define (put-back coin-out coins) ;if striker is out -remember to add to queen putback if its queen turn and foul together
  (define dummy 0)
  (define poslist (map cons (map (lambda (z) (+ midx (if (= z 0) 0 (if (< z 7) (* 2 coin-r (cos (* z (/ pi 3))))
                                                                       (* 2 (if (= 0 (remainder z 2)) (sqrt 3) 2) coin-r
                                                                          (cos (* (- z 7) (/ pi 6)))))))) (range 19))
                       (map (lambda (z) (+ midy (if (= z 0) 0 (if (< z 7) (* 2 coin-r (sin (* z (/ pi 3))))
                                                                  (* 2 (if (= 0 (remainder z 2)) (sqrt 3) 2) coin-r
                                                                     (sin (* (- z 7) (/ pi 6)))))))) (range 19))))
  (define (in-region x y vec)
    (ormap (lambda (z) (let ([vec-z-x (disc-x (vector-ref vec z))]
                             [vec-z-y (disc-y (vector-ref vec z))])
                         (< (magn (- x vec-z-x) (- y vec-z-y)) (* 2 coin-r)))) (range 19)))


  
  (define (bestfit lst vec)
    (cond [(null? (cdr lst)) (car lst)]
          [(in-region (caar lst) (cdar lst) vec) (bestfit (cdr lst) vec)]
          [else (car lst)]))
  
  (define pos (bestfit poslist coins))
  
                                                 
                                                 
  
  (begin 
    (myfor (define z 2) : (< z 19) : (set! z (+ z 2)) : (if (and (equal? (vector-ref coin-out z) turn) (= dummy 0))
                                                            (begin (vector-set! coin-out z #f)
                                                                   (set-disc-x! (vector-ref coins z) (car pos))
                                                                   (set-disc-y! (vector-ref coins z) (cdr pos))
                                                                   (set-disc-vx! (vector-ref coins z) 0)
                                                                   (set-disc-vy! (vector-ref coins z) 0)
                                                                   (set! dummy 10)) '()))
    (myfor (set! z 1) : (< z 19) : (set! z (+ z 2)) : (if (and (equal? (vector-ref coin-out z) turn) (= dummy 0))
                                                          (begin (vector-set! coin-out z #f)
                                                                 (set-disc-x! (vector-ref coins z) (car pos))
                                                                 (set-disc-y! (vector-ref coins z) (cdr pos))
                                                                 (set-disc-vx! (vector-ref coins z) 0)
                                                                 (set-disc-vy! (vector-ref coins z) 0)
                                                                 (set! dummy 20)) '()))
    (if (and (equal? (vector-ref coin-out 0) turn) (= dummy 0)) (begin (vector-set! coin-out 0 #f)
                                                                       (set-disc-x! (vector-ref coins 0) midx)
                                                                       (set-disc-y! (vector-ref coins 0) midy)
                                                                       (set-disc-vx! (vector-ref coins 0) 0)
                                                                       (set-disc-vy! (vector-ref coins 0) 0)
                                                                       (set! dummy 50)) '())
    dummy))
     


; turn is a global variable where 0 means up and 1 means below
;per unit time interval(0.01s ), the number of pixels travelled
(define speed (+ (quotient max-power 3) (random (quotient max-power 3))))
(define y-val 470)
(define x-val-l 90)
(define x-val-r 450)
(define coin-radius 10)
(define striker-radius 12.5); change it if necessary
(define corners (list->vector (list (cons 36 504) (cons 504 504) (cons 504 36) (cons 36 36))))
(define delta-x 10)

;later move it inside the function
(define curstate #f)
(define striker-x midx)
(define striker-vx 0)
(define striker-vy (- speed))


;(define x-list (map (lambda(x) (* x delta-x)) (range 0 (+ 1 (quotient (- x-val-r x-val-l) delta-x)))))
(define striker-poss (build-vector (+ (quotient (- x-val-r x-val-l) delta-x) 1)
                                   (lambda (x) (cons (+ x-val-l (* x delta-x)) y-val))))
  
  
(define (dist a b)
  (sqrt (+ (expt (- (car a) (car b)) 2) (expt (- (cdr a) (cdr b)) 2))))

;find area of triangle formed by three points
(define (area p q r)
  (let* ([x1 (car p)]
         [y1 (cdr p)]
         [x2 (car q)]
         [y2 (cdr q)]
         [x3 (car r)]
         [y3 (cdr r)])
    (/ (abs (- (+ (* x2 y3) (* x3 y1) (* x1 y2)) (+ (* x3 y2) (* x1 y3) (* x2 y1)))) 2)))

;vectorizes two points 
(define (vectorize i f)
  (cons (- (car f) (car i)) (- (cdr f) (cdr i))))

(define (dot-product v1 v2)
  (+ (* (car v1) (car v2)) (* (cdr v1) (cdr v2))))

;p and q are a pair of coordinates of the form x and y
(define (min-dist p q x)
  (define perp (/ (* 2 (area p q x)) (dist p q)))
  (if (and (> (dot-product (vectorize q x) (vectorize q p)) 0) (> (dot-product (vectorize p x) (vectorize p q)) 0))
      perp (min (dist p x) (dist q x))))

; q-p vector 
(define (cosine p q)
  (/ (- (car q) (car p)) (sqrt (+ (expt (- (car q) (car p)) 2) (expt (- (cdr q) (cdr p)) 2)))))

(define (sine p q)
  (/ (- (cdr q) (cdr p)) (sqrt (+ (expt (- (car q) (car p)) 2) (expt (- (cdr q) (cdr p)) 2)))))

(define (extend-vec p q)
  (cons (+ (car q) (* (+ striker-radius coin-radius) (cosine p q))) (+ (cdr q) (* (+ striker-radius coin-radius) (sine p q)))))

;coordinates of the hole must be given
(define (possible-coin hole coin-num)
  (define table (state-coi curstate))
  (define poss #t)
  (myfor (begin
           (define i 0)
           (define fixed (vector-ref table coin-num))
           (define pos1 (cons (disc-x fixed) (disc-y fixed)))):
    (< i 19):(set! i (+ i 1)):
    (let* ([var (vector-ref table i)]
           [pos2 (cons (disc-x var) (disc-y var))]
           [p (or (equal? coin-num i) (> (min-dist hole pos1 pos2) (* 1.999 coin-radius)))])
      (set! poss (and poss p))))
  (begin
    ;(displayln poss)
  poss))

; pos1 is the position of the striker and point is the destination
(define (possible-striker point pos1 coin-num)
  (define table (state-coi curstate))
  (define poss #t)
  (myfor (define i 0):
    (< i 19):(set! i (+ i 1)):
    (let* ([var (vector-ref table i)]
           [pos2 (cons (disc-x var) (disc-y var))]
           [p (or (= coin-num i) (> (min-dist point pos1 pos2) (* 0.999 (+ striker-radius coin-radius))))]) ; 0.999
      (begin
        ;(displayln poss)
        (set! poss (and poss p)))))
  (begin
    ;(displayln poss)
    poss))

(define (possible-overall hole coin-num striker-pos)
  (define table (state-coi curstate))
  (define fixed (vector-ref table coin-num))
  (define pos1 (cons (disc-x fixed) (disc-y fixed)))
  (define newpos (extend-vec hole pos1))
  (let* ([a (and (possible-striker newpos striker-pos coin-num) (possible-coin hole coin-num) 
       (< (dot-product (vectorize newpos hole) (vectorize newpos striker-pos)) 0))])
    (begin
      ;(displayln a)
      a)))

(struct striker-state (x vx vy angle) #:mutable #:transparent) 


;assume that Akshat has a function called end-state which gives the last state given a particular state
(define (find-best curstate1)
  (begin
    (define vec (make-vector 3 (striker-state #f #f #f 2)))
    (set! striker-x midx)
    (set! striker-vx 0)
    (set! striker-vy (- speed))
    (set! curstate curstate1)
    (define vector-coins (state-coi curstate))
    (myfor (define hole 2):
       (< hole 4):
       (set! hole (+ hole 1)):
       (myfor (define striker 0):
              (< striker (vector-length striker-poss)):
              (set! striker (+ striker 1)):
              (myfor (define coin 0):
                     (< coin 19):
                     (set! coin (+ coin 1)):
                     (begin
                       (define p (possible-overall (vector-ref corners hole) coin (vector-ref striker-poss striker)))
                       ;(if p (displayln "YES") 0)
                       (if (and p (not (vector-ref (state-coi-o curstate) coin)))
                           (begin
                             ;(displayln "YES")
                             ;(set! striker-x (car (vector-ref striker-poss striker)))
                             (let()
                             (define index (cond [(= coin 0) 0]
                                                 [(= (remainder coin 2) 1) 1]
                                                 [else 2]))
                             (define act-coin (vector-ref vector-coins coin))
                             (define pos-coin (cons (disc-x act-coin) (disc-y act-coin)))
                             (define dest (extend-vec (vector-ref corners hole) pos-coin))
                             (define cos-angle (/ (dot-product (vectorize pos-coin (vector-ref corners hole))
                                                               (vectorize pos-coin (vector-ref striker-poss striker)))
                                                  (* (dist pos-coin (vector-ref corners hole))
                                                     (dist pos-coin (vector-ref striker-poss striker)))))
                             ; cos-angle should be minimum  
                             (define origin (vector-ref striker-poss striker))
                             (if (and (< (sine origin dest) 0) (< cos-angle (striker-state-angle (vector-ref vec index))))
                                 (begin
                                   (set-striker-state-angle! (vector-ref vec index) cos-angle)
                                   (set-striker-state-x! (vector-ref vec index) (car (vector-ref striker-poss striker)))
                                   (set-striker-state-vx! (vector-ref vec index) (* speed (cosine origin dest)))
                                   (set-striker-state-vy! (vector-ref vec index) (* speed (sine origin dest))))
                                 0))
                              )
                               ;(displayln (cosine dest origin))
                               ;(displayln (sine dest origin))
                             ;(set! striker-vx (* speed (cosine origin dest)))
                             ;(set! striker-vy (* speed (sine origin dest)))
                             0)))))
                           
    ; set everything
    (define best-id #f)
    (cond [(not (equal? (striker-state-x (vector-ref vec 0)) #f))  (set! best-id 0)]
          [(not (equal? (striker-state-x (vector-ref vec 1)) #f))  (set! best-id 1)]
          [(not (equal? (striker-state-x (vector-ref vec 2)) #f))  (set! best-id 2)])
    (if (equal? best-id #f)
    (begin
      (set-disc-x! (state-str curstate1) striker-x)
      (set-disc-vx! (state-str curstate1) striker-vx)
      (set-disc-vy! (state-str curstate1) striker-vy))
    (begin
      (set-disc-x! (state-str curstate1) (striker-state-x (vector-ref vec best-id)))
      (set-disc-vx! (state-str curstate1) (striker-state-vx (vector-ref vec best-id)))
      (set-disc-vy! (state-str curstate1) (striker-state-vy (vector-ref vec best-id)))))
    ))

(define (pass-time ws)
  (define striker (state-str ws))
  (define coins (state-coi ws))
  (define striker-out (state-str-o ws))
  (define coin-out (state-coi-o ws))
  (define score (state-scr ws))
  (define queen-turn (state-qt ws))
  (define next-queen-turn (state-nqt ws))
  (define coin-taken (state-coi-t ws))
  (define turn-end (state-turn-end ws))
  
  
   
  (begin
    (if (and (= mode 1) turn-end (= turn 1)) (begin (find-best ws) (set! turn-end #f)) '())
    (if striker-out '()
        (let ([vel-magn (magn (disc-vx striker) (disc-vy striker))])
          (begin (set-disc-x! striker (+ (disc-x striker) (* del-t (disc-vx striker))))
                 (set-disc-y! striker (+ (disc-y striker) (* del-t (disc-vy striker))))
                 (set-disc-vx! striker (- (disc-vx striker) (with-handlers [(exn:fail:contract:divide-by-zero? (lambda (exn) 0))]
                                                              (/ (* del-t (disc-coeff striker) g (disc-vx striker)) vel-magn))))
                 (set-disc-vy! striker (- (disc-vy striker) (with-handlers [(exn:fail:contract:divide-by-zero? (lambda (exn) 0))]
                                                              (/ (* del-t (disc-coeff striker) g (disc-vy striker)) vel-magn))))
                 (if (and (< (abs (disc-vx striker)) static) (< (abs (disc-vy striker)) static)) (begin (set-disc-vx! striker 0) (set-disc-vy! striker 0)) '()))))

    (myfor (define z 0) : (< z 19) : (set! z (+ z 1)) : (let ([coins-z (vector-ref coins z)])
                                                          (if (vector-ref coin-out z) '()
                                                              (let ([vel-magn (magn (disc-vx coins-z) (disc-vy coins-z))])
                                                                (begin (set-disc-x! coins-z (+ (disc-x coins-z) (* del-t (disc-vx coins-z))))
                                                                       (set-disc-y! coins-z (+ (disc-y coins-z) (* del-t (disc-vy coins-z))))
                                                                       (set-disc-vx! coins-z (- (disc-vx coins-z)
                                                                                                (with-handlers [(exn:fail:contract:divide-by-zero? (lambda (exn) 0))]
                                                                                                  (/ (* del-t (disc-coeff coins-z) g (disc-vx coins-z)) vel-magn))))
                                                                       (set-disc-vy! coins-z (- (disc-vy coins-z)
                                                                                                (with-handlers [(exn:fail:contract:divide-by-zero? (lambda (exn) 0))]
                                                                                                  (/ (* del-t (disc-coeff coins-z) g (disc-vy coins-z)) vel-magn))))
                                                                       (if (and (< (abs (disc-vx coins-z)) static) (< (abs (disc-vy coins-z)) static))
                                                                           (begin (set-disc-vx! coins-z 0) (set-disc-vy! coins-z 0)) '()))))))


    (myfor (set! z 0) : (< z 19) : (set! z (+ z 1)) : (let ([coins-z (vector-ref coins z)])
                                                        (if (vector-ref coin-out z) '()
                                                            (map (lambda (w) (if (< (magn (- (car w) (disc-x coins-z)) (- (cdr w) (disc-y coins-z)))
                                                                                    (sqrt (- (* pocket-r pocket-r) (* coin-r coin-r))))
                                                                                 (begin (vector-set! coin-out z turn)
                                                                                        (set-disc-vx! coins-z 0) (set-disc-vy! coins-z 0)
                                                                                        (set-disc-x! coins-z 1000)
                                                                                        (set! coin-taken #t) 
                                                                                        (if (equal? (disc-color coins-z) black)
                                                                                            (vector-set! score turn (+ 10 (vector-ref score turn)))
                                                                                            (if (equal? (disc-color coins-z) white)
                                                                                                (vector-set! score turn (+ 20 (vector-ref score turn)))
                                                                                                (set! next-queen-turn #t))))
                                                                                 '())) pockets))))
    
    (if striker-out '() (map (lambda (w) (if (< (magn (- (car w) (disc-x striker)) (- (cdr w) (disc-y striker)))
                                                (sqrt (- (* pocket-r pocket-r) (* striker-r striker-r))))
                                             (begin (set-disc-vx! striker 0) (set-disc-vy! striker 0)
                                                    (set-disc-x! striker 1000) (set! striker-out #t)) '())) pockets))
                          
    
    (if striker-out '() (wall-collision striker))
  
    (myfor (set! z 0) : (< z 19) : (set! z (+ z 1)) : (let ([coins-z (vector-ref coins z)])
                                                        (if (vector-ref coin-out z) '()
                                                            (begin (wall-collision coins-z);collision between coin and wall
                                                                   (if (and (not striker-out)
                                                                            (< (magn (- (disc-x striker) (disc-x coins-z)) (- (disc-y striker) (disc-y coins-z)))
                                                                               (+ striker-r coin-r)))
                                                                       (collision striker coins-z) '()); collision between coin and striker
                                                                   (let() (myfor (define w 0) :
                                                                                 (< w 19) :
                                                                                 (set! w (+ w 1)):
                                                                                 (let ([coins-w (vector-ref coins w)]);check collision between coins
                                                                                   (if (and (not (= z w))
                                                                                            (not (vector-ref coin-out z))
                                                                                            (< (magn (- (disc-x coins-w) (disc-x coins-z))
                                                                                                     (- (disc-y coins-w) (disc-y coins-z)))
                                                                                               (* 2 coin-r)))
                                                                                       (collision coins-w coins-z) '()))))))))

    (cond [(and (= (disc-vx striker) 0) (= 0 (disc-vy striker)) (andmap (lambda (z)
                                                                          (and (= 0 (disc-vx (vector-ref coins z)))
                                                                               (= 0 (disc-vy (vector-ref coins z)))))
                                                                        (range 19)) (not turn-end))
           (begin
             
             (if queen-turn (if coin-taken (vector-set! score turn (+ (vector-ref score turn) 50))
                                (begin (vector-set! coin-out 0 #f) (set-disc-vx! (vector-ref coins 0) 0)
                                       (set-disc-vy! (vector-ref coins 0) 0)
                                       (set-disc-x! (vector-ref coins 0) midx)
                                       (set-disc-y! (vector-ref coins 0) midy))) '())
             (if striker-out (vector-set! score turn (- (vector-ref score turn) (put-back coin-out coins))) '())
             (if coin-taken '() (set! turn (if (= turn 0) 1 0)))

             ;take inputs 
             (set-disc-x! striker midx)
             (set-disc-y! striker (if (= turn 0) (+ y0 edges) (- y1 edges)))
             (set-disc-vx! striker 0)
             (set-disc-vy! striker 0)
             (set! queen-turn next-queen-turn)
             (set! next-queen-turn #f)
             (set! striker-out #f)
             (set! coin-taken #f)
             (set! turn-end #t))])
    
    (state striker coins striker-out coin-out queen-turn next-queen-turn coin-taken score turn-end)
    
    ))



(define empty_frame
  (empty-scene  740 540))
(define s1 (square 500 "outline" "white"))
(define s2 (square 540 "solid" "LightSkyBlue"))
(define rec1 (rectangle 200 540 "solid" "Dim Gray"))
(define hole (circle 16 "solid" "DimGray"))
(define coin (circle 10 "outline" "white"))
(define strikers (circle 12.5 "solid" "Dark Green"))
(define hor-line (line 360 0 "white"))
(define ver-line (line 0 360 "white"))
(define line-circle (circle 10 "outline" "white"))
(define centre-circle (circle 50 "outline" "white"))
(define centre-circle2 (circle 49 "outline" "white"))
(define design (radial-star 8 8 12 "solid" "white"))
(define a (text "Player 1" 24 "white"))
(define c (text "Carrom King" 30 "white"))
(define b (text (if (= mode 0) "Player 2" "AI") 24 "white"))
(define (carrom_board argument)
  (define cons-of-all (helper-func striker coins))
  (define car-cons (car cons-of-all))
  (define cdr-cons (car (cdr cons-of-all)))
  (define a-score (text (number->string (vector-ref (state-scr argument) 0)) 24 "white"))
  (define b-score (text (number->string (vector-ref (state-scr argument) 1)) 24 "white"))
  (place-images (list a b a-score b-score c design strikers black white black white black white black white black white black white black white black white black white red
                      coin centre-circle centre-circle2 rec1 hole hole hole hole hor-line hor-line coin coin hor-line hor-line coin coin 
                      ver-line ver-line coin coin ver-line ver-line coin coin s1 s2 
                      )
                (list
                 (make-posn 640 140)
                 (make-posn 640 350)
                 (make-posn 640 170)
                 (make-posn 640 380)
                 (make-posn 640 50)
                 (make-posn (car car-cons) (cdr car-cons))
                 (make-posn (car car-cons) (cdr car-cons))
                 (let* ((x (caar cdr-cons))
                        (y (cdar cdr-cons)))
                   (set! cdr-cons (cdr cdr-cons))
                   (make-posn x y))
                 (let* ((x (caar cdr-cons))
                        (y (cdar cdr-cons)))
                   (set! cdr-cons (cdr cdr-cons))
                   (make-posn x y))
                 (let* ((x (caar cdr-cons))
                        (y (cdar cdr-cons)))
                   (set! cdr-cons (cdr cdr-cons))
                   (make-posn x y))
                 (let* ((x (caar cdr-cons))
                        (y (cdar cdr-cons)))
                   (set! cdr-cons (cdr cdr-cons))
                   (make-posn x y))
                 (let* ((x (caar cdr-cons))
                        (y (cdar cdr-cons)))
                   (set! cdr-cons (cdr cdr-cons))
                   (make-posn x y))
                 (let* ((x (caar cdr-cons))
                        (y (cdar cdr-cons)))
                   (set! cdr-cons (cdr cdr-cons))
                   (make-posn x y))
                 (let* ((x (caar cdr-cons))
                        (y (cdar cdr-cons)))
                   (set! cdr-cons (cdr cdr-cons))
                   (make-posn x y))
                 (let* ((x (caar cdr-cons))
                        (y (cdar cdr-cons)))
                   (set! cdr-cons (cdr cdr-cons))
                   (make-posn x y))
                 (let* ((x (caar cdr-cons))
                        (y (cdar cdr-cons)))
                   (set! cdr-cons (cdr cdr-cons))
                   (make-posn x y))
                 (let* ((x (caar cdr-cons))
                        (y (cdar cdr-cons)))
                   (set! cdr-cons (cdr cdr-cons))
                   (make-posn x y))
                 (let* ((x (caar cdr-cons))
                        (y (cdar cdr-cons)))
                   (set! cdr-cons (cdr cdr-cons))
                   (make-posn x y))
                 (let* ((x (caar cdr-cons))
                        (y (cdar cdr-cons)))
                   (set! cdr-cons (cdr cdr-cons))
                   (make-posn x y))
                 (let* ((x (caar cdr-cons))
                        (y (cdar cdr-cons)))
                   (set! cdr-cons (cdr cdr-cons))
                   (make-posn x y))
                 (let* ((x (caar cdr-cons))
                        (y (cdar cdr-cons)))
                   (set! cdr-cons (cdr cdr-cons))
                   (make-posn x y))
                 (let* ((x (caar cdr-cons))
                        (y (cdar cdr-cons)))
                   (set! cdr-cons (cdr cdr-cons))
                   (make-posn x y))
                 (let* ((x (caar cdr-cons))
                        (y (cdar cdr-cons)))
                   (set! cdr-cons (cdr cdr-cons))
                   (make-posn x y))
                 (let* ((x (caar cdr-cons))
                        (y (cdar cdr-cons)))
                   (set! cdr-cons (cdr cdr-cons))
                   (make-posn x y))
                 (let* ((x (caar cdr-cons))
                        (y (cdar cdr-cons)))
                   (set! cdr-cons (cdr cdr-cons))
                   (make-posn x y))
                 (let* ((x (caar cdr-cons))
                        (y (cdar cdr-cons)))
                   (set! cdr-cons (cdr cdr-cons))
                   (make-posn x y))
                 (make-posn 270 270)
                 (make-posn 270 270)
                 (make-posn 270 270)
                 (make-posn 640 270)
                 (make-posn 36 36)
                 (make-posn 504 36)
                 (make-posn 36 504)
                 (make-posn 504 504)
                 (make-posn 270 60)
                 (make-posn 270 80)
                 (make-posn 90 70)
                 (make-posn 450 70)
                 (make-posn 270 480)
                 (make-posn 270 460)
                 (make-posn 90 470)
                 (make-posn 450 470)
                 (make-posn 60 270)
                 (make-posn 80 270)
                 (make-posn 70 90)
                 (make-posn 70 450)
                 (make-posn 480 270)
                 (make-posn 460 270)
                 (make-posn 470 90)
                 (make-posn 470 450)
                 (make-posn 270 270)
                 (make-posn 270 270)
                 )
                empty_frame))
  
               
(define (helper-func struct vector)
  (define striker (cons (disc-x struct) (disc-y struct)))
  (define (helper l ans n)
    (cond [(equal? n 19) ans]
          [else (helper l (cons (cons (disc-x (vector-ref l n)) (disc-y (vector-ref l n))) ans) (+ n 1))]))
  (cons striker (list (helper vector '() 0))))



(define (cond1 x y a b)
  (if (and (< y 70) (<= (+ (* (- x a) (- x a)) (* (- y b) (- y b))) 2500)) #t
      #f))
(define (cond2 x y a b)
  (if (and (> y 470) (<= (+ (* (- x a) (- x a)) (* (- y b) (- y b))) 2500)) #t
      #f))

(define (mouse-expr ws x y type)
  (define power-event 0)
  (define striker (state-str ws))
  (define coins (state-coi ws))
  (define striker-out (state-str-o ws))
  (define coin-out (state-coi-o ws))
  (define score (state-scr ws))
  (define queen-turn (state-qt ws))
  (define next-queen-turn (state-nqt ws))
  (define coin-taken (state-coi-t ws))
  (define turn-end (state-turn-end ws))
  (begin (if (and turn-end (or (= turn 0) (= mode 0))) (begin
                                                         (cond
                          
                                                           ; move the striker
                                                           ;[(mouse=? type "enter")
                                                           ;(cond [(and (>= y 20) (<= y 520) (>= x 20) (<= x 520)) play-the-game])]
                                                           ;; pause the game
                                                           ;[(mouse=? type "leave")
                                                           ;;; play the game again
                                                           ;(cond [(or (<= y 20) (>= y 520) (<= x 20) (>= x 520)) pause-the-game])] 
                                                           [(mouse=? type "button-down")
                                                            (cond [(and (>= y 60) (<= y 80) (>= x 90) (<= x 450) (= turn 0))
                                                                   (set-disc-x! striker x)
                                                                   (set-disc-y! striker 70)]
                                                                  [(and (>= y 460) (<= y 480) (>= x 90) (<= x 450)(= turn 1))
                                                                   (set-disc-x! striker x)
                                                                   (set-disc-y! striker 470)])]
                                                           ; put the striker down. store it somewhere maybe. make an arrow pointing the direction.
                                                           [(mouse=? type "drag")
                                                            (cond [(and 
                                                                    (cond1 x y (disc-x striker) (disc-y striker))
                                                                    (= turn 0))
                                                                   (let* ((a (disc-x striker))
                                                                          (b (disc-y striker))
                                                                          (length (sqrt (+ (* (- x a) (- x a)) (* (- y b) (- y b))))))
                                                                     (set! power-event (* max-power (/ length 40))))]
                                                                  [(and 
                                                                    (cond2 x y (disc-x striker) (disc-y striker))
                                                                    (= turn 1))
                                                                   (let* ((a (disc-x striker))
                                                                          (b (disc-y striker))
                                                                          (length (sqrt (+ (* (- x a) (- x a)) (* (- y b) (- y b))))))
                                                                     (set! power-event (* max-power (/ length 40))))])]
                                                           [(mouse=? type "move")
                                                            (cond [(and (>= y 60) (<= y 80) (>= x 90) (<= x 450) (= turn 0))
                                                                   (set-disc-x! striker x)
                                                                   (set-disc-y! striker 70)]
                                                                  [(and (>= y 460) (<= y 480) (>= x 90) (<= x 450)(= turn 1))
                                                                   (set-disc-x! striker x)
                                                                   (set-disc-y! striker 470)])]
                                                           [(mouse=? type "button-up")
                                                            (cond [(and 
                                                                    (cond1 x y (disc-x striker) (disc-y striker))
                                                                    (= turn 0))
                                                                   (let* ((a (disc-x striker))
                                                                          (b (disc-y striker))
                                                                          (length (sqrt (+ (* (- x a) (- x a)) (* (- y b) (- y b)))))
                                                                          (sin-theta (/ (- b y) length))
                                                                          (cos-theta (/ (- a x) length)))
                                                                     (set! power-event (* max-power (/ length 50)))
                                                                     (set-disc-vx! striker (* power-event cos-theta))
                                                                     (set-disc-vy! striker (* power-event sin-theta))
                                                                     (set! turn-end #f))]
                                                                  [(and 
                                                                    (cond2 x y (disc-x striker) (disc-y striker))
                                                                    (= turn 1))
                                                                   (let* ((a (disc-x striker))
                                                                          (b (disc-y striker))
                                                                          (length (sqrt (+ (* (- x a) (- x a)) (* (- y b) (- y b)))))
                                                                          (sin-theta (/ (- b y) length))
                                                                          (cos-theta (/ (- a x) length)))
                                    
                                                                     (set! power-event (* max-power (/ length 50)))
                                                                     (set-disc-vx! striker (* power-event cos-theta))
                                                                     (set-disc-vy! striker (* power-event sin-theta))
                                                                     (set! turn-end #f))])])) '())
         (state striker coins striker-out coin-out queen-turn next-queen-turn coin-taken score turn-end)
         ))

    

(define (to-draw-helper t)
  (cond [(equal? states 'main-menu) (main_menu "welcome")]
        [(equal? states 'rules) (rules_ "welcome")]
        [(equal? states 'how-to-play) (how_play "welcome")]
        [(equal? states 'carrom-board) (carrom_board t)]))
  
(define (game-end ws)
  (if (andmap (lambda (x) (vector-ref (state-coi-o ws) x)) (range 19)) #t #f))

(define (main ws)
  (set! states 'main-menu)
  (big-bang ws
    [on-tick on-tick-helper (/ 1 50)]
    [to-draw to-draw-helper]
    [on-mouse on-mouse-helper]
    [stop-when game-end]))

(define (on-tick-helper t)
  (if (equal? states 'carrom-board) (pass-time t) t))
(define (on-mouse-helper ws x y type)
  (cond [(equal? states 'carrom-board) (mouse-expr ws x y type)]
        [(and (equal? type "button-down") (equal? states 'main-menu))
         (cond [(and (> x 25) (< x 345) (> y 307) (< y 367)) (begin (set! mode 1) (set! states 'carrom-board) ws)]
               [(and (< x 715) (> x 395) (> y 307) (< y 367)) (begin (set! mode 0) (set! states 'carrom-board) ws)]
               [(and (< x 345) (> x 25) (> y 443) (< y 503)) (begin (set! states 'how-to-play) ws)]
               [(and (< x 715) (> x 395) (< y 503) (> y 443)) (begin (set! states 'rules) ws)])]
        [(and (equal? type "button-down") (equal? states 'how-to-play))
         (cond [(and (> x 570) (< x 660) (> y 460) (< y 550)) (begin (set! states 'main-menu) ws)])]
        [(and (equal? type "button-down") (equal? states 'rules))
         (cond [(and (> x 570) (< x 660) (> y 460) (< y 550)) (begin (set! states 'main-menu) ws)])]
        [else ws]
        ))


(define empty_frame3
  (empty-scene 740 540))
;(define a (text "How to Play" 120 "blue"))
(define base (bitmap/file "image/howtoplay.PNG"))
;(define b (text "Use mouse to play." 50 "blue"))
;(define c (text "For each strike ,position the striker on the base line." 50 "blue"))
;(define d (text "Before striking ,click and drag drag the hand to select
;the power and then release it to pocket the piece" 50 "blue"))

(define (how_play argument)
  (place-images (list base)
                (list (make-posn 370 270)) empty_frame3))


(define empty_frame4
  (empty-scene 740 540))

(define bases (bitmap/file "image/rules.PNG"))
;(define b (text "A turn consists of one or more strikes. A player having higher score wins at the end of game
;
;Sinking the striker will cost you one piece.
;After pocketing the red colour piece, you must sink
; any of your carrom men,
; covering it in the next shot,
;or else red colour piece will be returned back to
;the center point of the board." 50 "blue"))
(define (rules_ argument)
  (place-images (list  bases)
                (list 
                      
                 (make-posn 370 270)) empty_frame4))

 
(define empty_frame2
  (empty-scene 740 540))
  
(define (main_menu argument)
  (place-images (list basess aa bb cc dd)
                (list 
                 (make-posn 370 135)
                 (make-posn 185 337)
                 (make-posn 555 337)
                 (make-posn 185 473)
                 (make-posn 555 473)) empty_frame2))
(define basess (bitmap/file "image/project.png"))
(define aa (bitmap/file "image/button1.png"))
(define bb (bitmap/file "image/button2.PNG"))
(define cc (bitmap/file "image/button3.PNG"))
(define dd (bitmap/file "image/button4.PNG"))


(main (state striker coins #f (make-vector 19 #f) #f #f #f (make-vector 2 0) #t))


