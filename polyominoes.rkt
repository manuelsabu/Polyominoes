;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname polyominoes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "a10.rkt")

;; Uncomment the following line if you want to use
;; the examples in kanoodle.rkt
;; (require "kanoodle.rkt")

;; A Grid is a (ne-listof (ne-listof Character))

(define-struct pos (x y))
;; A Pos is a (make-pos Nat Nat)

(define-struct state (puzzle pieces))
;; A State is a (make-state Grid (listof Grid))

;; A temporary neighbours function that always fails.  
;; Provide only the contract, purpose and function definition.
(define (neighbours s)
  empty)

;; solve-puzzle: Grid (listof Grid) Symbol -> (union (listof String) false)
;; Solve a polyomino puzzle, given the initial empty (or partially filled 
;; in) grid, a set of pieces that must be placed, and a Symbol indicating
;; what visualization style to use.  Legal viz styles are 'interactive
;; (draw every step of the search), 'at-end (just draw the solution, if one
;; is found), or 'offline (don't draw anything).  Produce either the solved
;; Grid (converted to a list of Strings, just for convenience) or false if
;; no solution exists.
;;
(define (solve-puzzle grid polys viz-style)
  (local
    [(define result
       (search 
        (lambda (S) (empty? (state-pieces S)))
        neighbours
        (cond
          [(symbol=? viz-style 'interactive)
           (lambda (S) (draw-grid (state-puzzle S)))]
          [else false])
        (make-state grid polys)))
     
     (define maybe-last-draw
       (cond
         [(and (state? result)
               (symbol=? viz-style 'at-end))
          (draw-grid (state-puzzle result))]
         [else false]))]
    (cond
      [(boolean? result) result]
      [else (map list->string (state-puzzle result))])))

;; Examples:
;; (The examples are not provided in check-expect form.  They're meant to
;; demonstrate typical uses of the function, but we don't want to them to
;; open interactive visualizations every time you start the program.)

;; Solve offline (i.e. work like a normal Scheme function).
;(solve-puzzle
;  (strlist->grid '("...." "...." "...." "...." "...." "...."))
;  (cons '((#\L #\L) (#\. #\L)) (cons '((#\O)) tetrominoes-uc))
;  'offline)

;; Display the result graphically, if a solution is found.
;(solve-puzzle
;  (strlist->grid '("...." "...." "...." "...." "...." "...."))
;  (cons '((#\L #\L) (#\. #\L)) (cons '((#\O)) tetrominoes-uc))
;  'at-end)

;; Display every step of the search as it progresses.
;(solve-puzzle
;  (strlist->grid '("...." "...." "...." "...." "...." "...."))
;  (cons '((#\L #\L) (#\. #\L)) (cons '((#\O)) tetrominoes-uc))
;  'interactive)

;;**************************************************************************************
;; Definitions:

(define Grid1 
  (list (list #\. #\d #\.)
        (list #\d #\d #\d)
        (list #\. #\d #\.)))

(define Grid2
  (list (list #\a #\a)
        (list #\a #\a)
        (list #\a #\.)))

(define Grid3 
  (list (list #\b #\b #\b)
        (list #\. #\b #\.)
        (list #\. #\b #\.)))

(define Grid4
  (list (list #\b #\b)
        (list #\b #\b)
        (list #\b #\b)))

(define Grid5 
  (list (list #\c #\c #\c)
        (list #\. #\c #\c)
        (list #\. #\c #\.)))

(define Grid6 
  (list (list #\b #\. #\. #\. #\.)
        (list #\b #\b #\b #\b #\b)
        (list #\. #\b #\. #\. #\.)))

(define Grid7 
  (list (list #\b #\. #\. #\a #\a)
        (list #\b #\b #\b #\a #\a)
        (list #\. #\b #\. #\a #\.)))

(define Grid8 
  (list (list #\. #\d #\.)
        (list #\c #\c #\c)
        (list #\. #\c #\c)))

(define Grid9
  (list (list #\. #\d #\.)
        (list #\d #\d #\d)
        (list #\. #\d #\.)))

(define GridList3
  (list 
   (list (list #\b #\b)
         (list #\b #\b)
         (list #\b #\b))
   (list (list #\b #\b #\b)
         (list #\b #\b #\b))))

(define GridList2
  (list 
   (list (list #\b #\b #\b)
         (list #\. #\b #\.)
         (list #\. #\b #\.))
   (list (list #\. #\b #\.)
         (list #\. #\b #\.)
         (list #\b #\b #\b))
   (list (list #\b #\. #\.)
         (list #\b #\b #\b)
         (list #\b #\. #\.))
   (list (list #\. #\. #\b)
         (list #\b #\b #\b)
         (list #\. #\. #\b))))

(define GridList1 
  (list 
   (list (list #\a #\a)
         (list #\a #\a)
         (list #\a #\.))
   (list (list #\a #\a)
         (list #\a #\a)
         (list #\. #\a))
   (list (list #\a #\.)
         (list #\a #\a)
         (list #\a #\a))
   (list (list #\. #\a)
         (list #\a #\a)
         (list #\a #\a))
   (list (list #\a #\a #\a)
         (list #\a #\a #\.))
   (list (list #\a #\a #\.)
         (list #\a #\a #\a))
   (list (list #\a #\a #\a)
         (list #\. #\a #\a))
   (list (list #\. #\a #\a)
         (list #\a #\a #\a))))
;;**************************************************************************************

;; build-2dlist: Nat Nat (Nat Nat -> Any) -> (listof (listof Any))
;; Purpose: Consumes two natural numbers "Width", "Height" and a function
;;          "Func" that is applied to all (x,y) positions corresponding to 
;;          positions in the grid, in that order. The function  produces a 
;;          2D list of the values obtained from the function substitution.

;; Examples:

(check-expect (build-2dlist 2 4 +)
              (list (list 0 1)
                    (list 1 2)
                    (list 2 3)
                    (list 3 4)))

(check-expect (build-2dlist 2 4 -)
              (list (list 0 1)
                    (list -1 0)
                    (list -2 -1)
                    (list -3 -2)))

;; Definitions:
(define (build-2dlist Width Height Func)
  (build-list Height (lambda (x) 
                       (build-list Width (lambda (y) (Func y x))))))

;; Tests:
(check-expect (build-2dlist 2 4 +)
              (list (list 0 1)
                    (list 1 2)
                    (list 2 3)
                    (list 3 4)))

(check-expect (build-2dlist 2 4 -)
              (list (list 0 1)
                    (list -1 0)
                    (list -2 -1)
                    (list -3 -2)))

(check-expect (build-2dlist 2 4 *)
              (list (list 0 0)
                    (list 0 1)
                    (list 0 2)
                    (list 0 3)))

(check-expect (build-2dlist 3 3 +) 
              (list (list 0 1 2)
                    (list 1 2 3)
                    (list 2 3 4)))

(check-expect (build-2dlist 1 1 make-pos)
              (list (list (make-pos 0 0))))

(check-expect (build-2dlist 1 4 make-pos)
              (list (list (make-pos 0 0)) 
                    (list (make-pos 0 1)) 
                    (list (make-pos 0 2)) 
                    (list (make-pos 0 3))))

(check-expect (build-2dlist 4 1 make-pos)
              (list (list (make-pos 0 0)
                          (make-pos 1 0)
                          (make-pos 2 0)
                          (make-pos 3 0))))

(check-expect (build-2dlist 0 0 make-pos)
              (list))

(check-expect (build-2dlist 0 4 +) 
              (list empty empty empty empty))

(check-expect (build-2dlist 2 0 +) empty)

;;**************************************************************************************

;; all-positions: Nat Nat -> (listof Pos)
;; Purpose: Consumes two natural numbers "Width", "Height" and produces 
;;          a list of all the positions (x,y) coordinates of the grid formed.

;; Examples:

(check-expect (lists-equiv? (all-positions 0 0) empty) true)

(check-expect (lists-equiv? (all-positions 1 1) (list (make-pos 0 0))) true)

;; Defintions:
(define (all-positions Width Height)
  (foldr (lambda (x y)
           (append y x)) empty (build-2dlist Width Height make-pos)))

;; Tests:
(check-expect (lists-equiv? (all-positions 2 3)
                            (list (make-pos 0 2)
                                  (make-pos 1 2)
                                  (make-pos 0 1)
                                  (make-pos 1 1)
                                  (make-pos 0 0)
                                  (make-pos 1 0))) true)

(check-expect (lists-equiv? (all-positions 0 0) empty) true)

(check-expect (lists-equiv? (all-positions 1 1) (list (make-pos 0 0))) true)

(check-expect (lists-equiv? (all-positions 1 3) (list
                                                 (make-pos 0 0)
                                                 (make-pos 0 1)
                                                 (make-pos 0 2))) true)

(check-expect (lists-equiv? (all-positions 3 1) (list 
                                                 (make-pos 0 0)
                                                 (make-pos 1 0)
                                                 (make-pos 2 0))) true)



;;**************************************************************************************

;; all-orientations: Grid -> (listof Grid)
;; Purpose: Consumes a "Grid" and produces all the possible orientations of
;;          the Grid excluding the repititions.

;; Examples:
(check-expect (lists-equiv? (all-orientations Grid3) GridList2) true)

(check-expect (lists-equiv? (all-orientations Grid4) GridList3) true)

;; Defintions:
(define (all-orientations Grid)
  (local[
         ;; main: Grid -> (listof Grid)
         ;; Purpose: Consumes a "Grid" and produces all the possible orientations of
         ;;          the Grid excluding the repititions.
         (define (main Grid)
           (foldr (lambda (x y)
                    (cons x (filter (lambda (Rest-list)
                                      (not (equal? x Rest-list))) y)))
                  empty 
                  
                  (list Grid 
                        (rotate Grid)
                        (reverse Grid) 
                        (rotate (reverse Grid)) 
                        (y-axis-flip Grid) 
                        (rotate (y-axis-flip Grid)) 
                        (y-axis-flip (reverse Grid)) 
                        (rotate (y-axis-flip (reverse Grid))))))
         
         ;; y-axis-flip: Grid -> Grid
         ;; Purpose: Consumes a "Grid" and produces a new Grid which
         ;;          is flipped along its horizontal axis.
         (define (y-axis-flip Grid)
           (cond
             [(empty? Grid) empty]
             [else (cons (reverse (first Grid)) 
                         (y-axis-flip (rest Grid)))]))
         
         ;; rotate: Grid -> Grid
         ;; Purpose: Consumes a "Grid" and produces a new Grid which
         ;;          is rotated counter-clockwise.
         (define (rotate Grid)
           (local [
                   ;; rotate-main: Grid -> Grid
                   ;; Purpose: Consumes a "Grid" and produces a new Grid which
                   ;;          is rotated counter-clockwise.
                   (define (rotate-main Grid)
                     (cond
                       [(empty? (first Grid)) empty]
                       [else (cons (last-column Grid) 
                                   (rotate (Cut-Grid Grid)))]))
                   
                   ;; last-column: Grid -> (listof Any)
                   ;; Purpose: Consumes a "Grid" and produces a list of all the last
                   ;;          elements in each list of the grid.
                   (define (last-column Grid)
                     (cond
                       [(empty? Grid) empty]
                       [else  (foldl (lambda (x y)
                                       (cons x (last-column (rest Grid)))) empty (first Grid))]))
                   
                   ;; Cut-Grid: Grid -> Grid
                   ;; Purpose: Consumes a "Grid" and produces a new grid with
                   ;;          the last element in each list of the grid excluded.                   
                   (define (Cut-Grid Grid)
                     (cond
                       [(empty? Grid) empty]
                       [else (cons (last-term-eliminator (first Grid)) 
                                   (Cut-Grid (rest Grid)))]))
                   
                   ;; last-term-eliminator: (listof Any) -> (listof Any)
                   ;; Purpose: Consumes a "lst" and produces a new list with all
                   ;;          elements excluding the last element.
                   (define (last-term-eliminator lst)
                     (cond
                       [(empty? lst) empty]
                       [(empty? (rest lst)) empty]
                       [else (cons (first lst) 
                                   (last-term-eliminator (rest lst)))]))]
             (rotate-main Grid)))]  
    (main Grid)))


;; Tests:
(check-expect (lists-equiv? (all-orientations Grid3) GridList2) true)

(check-expect (lists-equiv? (all-orientations Grid4) GridList3) true)

(check-expect (lists-equiv? (all-orientations Grid2) GridList1) true)

(check-expect (lists-equiv? (all-orientations Grid1) (list Grid1)) true)


;;**************************************************************************************

;; first-empty-pos: Grid -> Pos
;; Purpose: Consumes a "Grid" and produces the Pos (position)
;;          of the first occurance of "#\." in the grid.
(define (first-empty-pos Grid)
  (local [
          ;; first-empty-pos: Grid -> Pos
          ;; Purpose: Consumes a "Grid" and produces the Pos (position)
          ;;          of the first occurance of "#\." in the grid.
          (define (first-empty-pos Grid)
            (start Grid 0))
          
          ;; start: Grid Int -> Pos
          ;; Purpose: Consumes a "Grid" and an accumulator "y-acc" to produce a Pos (postion)
          ;;          of the first occurance of "#\." in the Grid.
          (define (start Grid y-acc)
            (cond
              [(empty? Grid) false]
              [else 
               (cond
                 [(number? (helper1 (first Grid) 0)) (make-pos (helper1 (first Grid) 0) y-acc)]
                 [else (start (rest Grid) (add1 y-acc))])]))
          
          ;; helper1: (listof Char) Int -> (union Int False)
          ;; Purpose: Consumes a list "lst" and an accumulator "x-axis" to produce the x-axis
          ;;          position of "#\." or false if the character isn't found.
          (define (helper1 lst x-acc)
            (cond
              [(empty? lst) false]
              [else 
               (cond
                 [(equal? (first lst) #\.) x-acc]
                 [else (helper1 (rest lst) (add1 x-acc))])]))]
    (first-empty-pos Grid)))



(check-expect (first-empty-pos Grid5) (make-pos 0 1))
(check-expect (first-empty-pos Grid1) (make-pos 0 0))
(check-expect (first-empty-pos Grid4) false)

;;**************************************************************************************
;; Question 4
(define testgrid2
  (list (list #\b #\b #\b)
        (list #\b #\b #\b)
        (list #\b #\b #\b)))

(define testgrid3
  (list (list #\t #\t #\t)
        (list #\. #\t #\.)
        (list #\. #\t #\.)))

(define testgrid1  
  (list (list #\b #\b #\b)
        (list #\b #\b #\b)
        (list #\t #\t #\t)))

;; superimpose: Grid Grid Pos -> Grid
;; Purpose: Concumes "Grid1", "Grid2" and "coordinate" to produce
;;          a new Grid where Grid2 is laid over Grid1. Any occurances of
;;          #\. in Grid2  do not overwrite the contents of Grid1.

(define (superimpose Grid1 Grid2 coordinate)
  (local [
          ;; starter: Grid Grid Pos -> Grid
          ;; Purpose: Concumes "Grid1", "Grid2" and "coordinate" to produce
          ;;          a new Grid where Grid2 is laid over Grid1. Any occurances of
          ;;          #\. in Grid2  do not overwrite the contents of Grid1.
          
          (define (starter Grid1 Grid2 coordinate)
            (grid-cutter (row-elem-count (first Grid1) 0) 
                         (row-cons Grid1 Grid2 (pos-x coordinate) (pos-y coordinate) 0)))
          
          ;; grid-cutter: Int Grid -> Grid
          ;; Purpose: Consumes an int "row-length" and "Grid" to produce
          ;;          a new Grid with each list in the grid containing 
          ;;          "row-length" elements.
          
          (define (grid-cutter row-length Grid)
            (local 
              [
               ;; grid-start: Int Grid -> Grid
               ;; Purpose: Consumes an int "row-length" and "Grid" to produce
               ;;          a new Grid with each list in the grid containing 
               ;;          "row-length" elements.
               
               (define (grid-start row-length Grid)
                 (cond
                   [(empty? Grid) empty]
                   [else (cons (grid-cut-help (first Grid) row-length 0)
                               (grid-cutter row-length (rest Grid)))]))
               
               ;; grid-cut-help: (listof Any) Nat Nat -> (listof Any)
               ;; Purpose: Consumes a list "lst", "length" and "acc"
               ;;          to produce a new list of only acc elements.
               
               (define (grid-cut-help lst length acc)
                 (cond
                   [(= acc length) empty]
                   [else (cons (first lst) (grid-cut-help (rest lst) 
                                                          length 
                                                          (add1 acc)))]))]
              (grid-start row-length Grid)))
          
          ;; row-elem-count: (listof Any) Nat -> Nat
          ;; Purpose: Consumes "lst" and "acc" to produce the total
          ;;          number of elements in lst.
          
          (define (row-elem-count lst acc)
            (cond
              [(empty? lst) acc]
              [else (row-elem-count (rest lst) (add1 acc))]))
          
          ;; row-elem-count: Grid Grid Nat Nat Nat -> Grid
          ;; Purpose: Consumes "Grid1", "Grid2", "x-axis", "y-axis" and "acc" to 
          ;;          produce a new Grid with Grid2 overlaid on Grid1.
          
          (define (row-cons Grid1 Grid2 x-axis y-axis acc)
            (cond
              [(empty? Grid1) empty]
              [(= acc y-axis)
               (cond
                 [(empty? Grid2) Grid1]
                 [else (cons (list-cons (first Grid1) (first Grid2) x-axis 0)
                             (row-cons (rest Grid1) (rest Grid2) x-axis y-axis acc))])]
              [else (cons (first Grid1) (row-cons (rest Grid1) Grid2 x-axis y-axis (add1 acc)))]))
          
          ;; list-cons: (listof Any) (listof Any) Nat Nat -> (listof Any)
          ;; Purpose: Consumes "lst1", "lst2", "x-axis", and "acc" to produce
          ;;          a new list with "lst2" overlaid on "lst1". If #\. is encountered
          ;;          in lst2, then it does not overwrite lst1.
          
          (define (list-cons lst1 lst2 x-axis acc)
            (cond
              [(and (empty? lst1) (empty? lst2)) empty]
              [(empty? lst1) lst2]
              [(= acc x-axis) 
               (cond 
                 [(empty? lst2) lst1]
                 [(equal? (first lst2) #\.)
                  (cons (first lst1) (list-cons (rest lst1) (rest lst2) x-axis acc))]
                 [else (cons (first lst2) (list-cons (rest lst1) (rest lst2) x-axis acc))])]
              [else (cons (first lst1) (list-cons (rest lst1) lst2 x-axis (add1 acc)))]))] 
    
    (starter Grid1 Grid2 coordinate)))

(check-expect (superimpose testgrid2 testgrid3  (make-pos 0 2)) testgrid1)
(check-expect (superimpose Grid6 Grid2 (make-pos 3 0)) Grid7)
(check-expect (superimpose Grid1 Grid5 (make-pos 0 1)) Grid8)
(check-expect (superimpose Grid1 empty (make-pos 0 1)) Grid9)
(check-expect (superimpose Grid2 Grid4 (make-pos 0 0)) Grid4)

;;**************************************************************************************
