;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
(define BG-WDTH 100)
(define BG-HGHT 100)
(define BG (empty-scene BG-WDTH BG-HGHT))

(define TANK-WDTH 20)
(define TANK-HGHT 5)
(define TANK-Y (- BG-HGHT TANK-HGHT))
(define TANK-DX 2)
(define TANK (rectangle TANK-WDTH TANK-HGHT "solid" "blue"))

(define UFO-WDTH 20)
(define UFO-RADIUS 3)
(define UFO-HGHT (* 2 UFO-RADIUS))
(define UFO-DY 1)
(define UFO-COLOR "green")
(define UFO (overlay
             (circle UFO-RADIUS "solid" UFO-COLOR)
             (rectangle UFO-WDTH UFO-RADIUS "solid" UFO-COLOR)))

(define MISSILE-RADIUS 3)
(define MISSILE-DY -2)
(define MISSILE-COLOR "red")
(define MISSILE (circle MISSILE-RADIUS "solid" MISSILE-COLOR))

(define-struct sigs (ufo-pos tank-x missile-pos))

(define SIGS0 (make-sigs
               (make-posn (random (- BG-WDTH UFO-WDTH)) UFO-RADIUS)  ; ufo-xy
               (- (/ BG-WDTH 2) (/ TANK-WDTH 2))  ; tank-x
               false  ; missile-xy
               ))

(define (draw-missile? missile-pos)
  (cond
    [(false? missile-pos) false]
    [(<= (posn-y missile-pos) 0) false]
    [else true]))

(define (maybe-draw-missile sigs scene)
  (if (draw-missile? (sigs-missile-pos sigs))
      (underlay/xy
       scene
       (posn-x (sigs-missile-pos sigs))
       (posn-y (sigs-missile-pos sigs))
       MISSILE)
      scene))

(define (render sigs)
  (maybe-draw-missile
   sigs
   (underlay/xy
    (underlay/xy BG (sigs-tank-x sigs) TANK-Y TANK)
    (posn-x (sigs-ufo-pos sigs)) (posn-y (sigs-ufo-pos sigs))
    UFO
    )))

(define (mv-tank dx sigs)
  (if (< 0 (+ (sigs-tank-x sigs) dx) (- BG-WDTH TANK-WDTH))
      (make-sigs
       (sigs-ufo-pos sigs)
       (+ (sigs-tank-x sigs) dx)
       (sigs-missile-pos sigs))
      sigs))

(define (fire sigs)
  (if (false? (sigs-missile-pos sigs))
      (make-sigs
       (sigs-ufo-pos sigs)
       (sigs-tank-x sigs)
       (make-posn
        (+ (sigs-tank-x sigs) (/ TANK-WDTH 2))
        (- TANK-Y TANK-HGHT)))
      sigs))

(define (handle-key sigs k)
  (cond
    [(key=? k "j") (mv-tank (- TANK-DX) sigs)]
    [(key=? k "l") (mv-tank TANK-DX sigs)]
    [(key=? k " ") (fire sigs)]
    [else sigs]))

(define (next-missile-pos curr)
  (if (false? curr)
      curr
      (make-posn (posn-x curr) (+ (posn-y curr) MISSILE-DY))))

(define (handle-tick sigs)
  (make-sigs
   (make-posn
    (+ (posn-x (sigs-ufo-pos sigs)) (if (= 0 (random 5)) (- (random 3) 1) 0))
    (+ (posn-y (sigs-ufo-pos sigs)) UFO-DY))
   (sigs-tank-x sigs)
   (next-missile-pos (sigs-missile-pos sigs))))

(define (collision? point-x point-y box-x1 box-y1 box-w box-h)
  (and
   (<= box-x1 point-x (+ box-x1 box-w))
   (<= box-y1 point-y (+ box-y1 box-h))))

(define (missile-hit? missile-pos ufo-pos)
  (if (false? missile-pos)
      false
      (collision?
       (posn-x missile-pos)
       (posn-y missile-pos)
       (posn-x ufo-pos)
       (posn-y ufo-pos)
       UFO-WDTH
       UFO-HGHT)))

(define (gameover? sigs)
  (or
   (> (posn-y (sigs-ufo-pos sigs)) (- BG-HGHT UFO-RADIUS))  ; ufo landed
   (< (posn-x (sigs-ufo-pos sigs)) (- (/ UFO-WDTH 2)))  ; ufo outran tank left
   (> (posn-x (sigs-ufo-pos sigs)) (- BG-WDTH UFO-RADIUS))  ; ufo outran tank right
   (missile-hit? (sigs-missile-pos sigs) (sigs-ufo-pos sigs))))    

(big-bang SIGS0
          [to-draw render]
          [on-key handle-key]
          [on-tick handle-tick]
          [stop-when gameover?])