#lang racket

(define file (open-input-file "test.txt"))

(define games 
  (string-join
    (map 
      (λ (gamestr) 
        (format "{~a}"
          (string-join 
            (map 
              (λ (roundstr) 
                (format "[~a]"
                  (string-join (map (λ (l) (format "(~a)" (string-trim l))) (string-split roundstr ",")) " ")))
              (string-split gamestr ";"))
            "\n")))
      (string-split (port->string file) "\n"))
    "\n"))

(displayln games)
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
