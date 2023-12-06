#lang at-exp racket

(define file (open-input-file "input.txt"))
(define blocks (string-split (port->string file) "\n\n"))

(define seeds (map string->number (cdr (string-split (car blocks) " "))))

(define maps
  (for/list ([block (cdr blocks)])
    (define vals (cdr (string-split block "\n")))
    (map (λ (str) (map string->number (string-split str " "))) vals)))


(define maps*
  `(mappings
    ([seed soil] ,@(first maps))
    ([soil fertilizer] ,@(second maps))
    ([fertilizer water] ,@(third maps))
    ([water light] ,@(fourth maps))
    ([light temperature] ,@(fifth maps))
    ([temperature humidity] ,@(sixth maps))
    ([humidity location] ,@(seventh maps))))

(displayln `(numbers ,@seeds))
(displayln maps*)
