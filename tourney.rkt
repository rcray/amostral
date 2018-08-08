#lang racket/base

;; Player A matches B.  If A wins, it matches C.  If B wins, it
;; matches C.  In general, if X plays Y and wins, it matches Z, the
;; other player waiting for this turn to play.  If a player wins twice
;; in a row, the torney ends.  If this doesn't happen, but 4 matches
;; have been played, the tourney ends.  Enumerate all possible
;; sequences of games.

;; (*) The tree
;; racket@torneio.rkt> (f-sample 'A 'B 0 0 0 0)
;; (tree
;;  '(game A B)
;;  (tree '(game A C) '() (tree '(game C B) '() (tree '(game B A) '() '())))
;;  (tree '(game B C) '() (tree '(game C A) '() (tree '(game A B) '() '()))))

;; (*) The list of games
;; racket@torneio.rkt> (print-tree "" (f-sample 'A 'B 0 0 0 0))
;; (game A B) --> (game A C) -->  end
;; (game A B) --> (game A C) --> (game C B) -->  end
;; (game A B) --> (game A C) --> (game C B) --> (game B A) --> end
;; (game A B) --> (game B C) -->  end
;; (game A B) --> (game B C) --> (game C A) -->  end
;; (game A B) --> (game B C) --> (game C A) --> (game A B) --> end
;; racket@torneio.rkt> 

;; (local-set-key (kbd "RET") 'newline-and-indent)

(require racket/bool)
(require racket/list)

(define (other p1 p2)
  (cond [(and (symbol=? p1 'A) (symbol=? p2 'B)) 'C]
        [(and (symbol=? p1 'A) (symbol=? p2 'C)) 'B]
        [(and (symbol=? p1 'B) (symbol=? p2 'A)) 'C]
        [(and (symbol=? p1 'B) (symbol=? p2 'C)) 'A]
        [(and (symbol=? p1 'C) (symbol=? p2 'A)) 'B]
        [(and (symbol=? p1 'C) (symbol=? p2 'B)) 'A]
        [else 'impossible]))

;; A Tree is either
;;  - empty OR
;; - (make-tree '(game X Y) Tree Tree)
(define-struct tree (root left right) #:transparent)

(define (print-tree acc-str tree)
  (cond [(empty? tree) (displayln (string-append acc-str "end"))]
        ;; avoid going down the tree if both branches are empty
        [(and (empty? (tree-left tree))
              (empty? (tree-right tree)))
         (displayln (string-append acc-str (format "~a " (tree-root tree)) "--> end"))]
        [else
         (print-tree
               (string-append acc-str (format "~a " (tree-root tree)) "--> ")
               (tree-left tree))
         (print-tree
          (string-append acc-str (format "~a " (tree-root tree)) "--> ")
          (tree-right tree))]))

(define (f-sample p1 p2 total a b c)
  (cond [(= total 4) empty]
        [(or (= a 2) (= b 2) (= c 2)) empty]
        [else (make-tree
               (list p1 p2)
               ;; left
               (f-sample p1 (other p1 p2) (add1 total)
                           (if (symbol=? p1 'A) (add1 a) a)
                           (if (symbol=? p1 'B) (add1 b) b)
                           (if (symbol=? p1 'C) (add1 c) c))
               ;; right
               (f-sample p2 (other p1 p2) (add1 total)
                           (if (symbol=? p2 'A) (add1 a) a)
                           (if (symbol=? p2 'B) (add1 b) b)
                           (if (symbol=? p2 'C) (add1 c) c)))]))

;; This function produces a sample space where the torney only ends if
;; 4 matches take place.  It doesn't end if a player wins twice in a
;; row.  I wrote it because it's easier than the above.
(define (f p1 p2 total)
  (cond [(= total 4) empty]
        [else (make-tree (list 'game p1 p2)
                    ;; left
                    (f p1 (other p1 p2) (add1 total))
                    ;; right
                    (f p2 (other p1 p2) (add1 total)))]))

;; --8<---------------cut here---------------start------------->8---
;; From Matthias.  How to print a tree beautifully.
;; Amazing code.  How does it work?

(require racket/local)

(define (printer t0)
  (local (;; from-t0 : the path from t0 to t 
          (define (printer/acc t from-t0)
            (cond
              [(empty? t)
               (map display (reverse (cons " end" from-t0)))
               (newline)]
              [else
               (define from-t (list* " --> " (tree-root t) from-t0))
               (printer/acc (tree-left t) from-t)
               (unless (empty? (tree-right t))
                 (printer/acc (tree-right t) from-t))])))
    (printer/acc t0 '())))

;; (printer tree1)
;; --8<---------------cut here---------------end--------------->8---
