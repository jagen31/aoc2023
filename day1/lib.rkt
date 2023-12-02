#lang racket

(require art (for-syntax racket/list racket/match racket/string racket/format syntax/parse))

(provide (all-defined-out))

(module+ test (require rackunit))

;; done for part 1 (left unchanged)

;; text
(define-art-object (text [str]))

;; a number
(define-art-object (number [n]))

;; only keep the first and last from each line
(define-mapping-rewriter (first-and-last [(: t text)])
  (λ (stx t)
    (syntax-parse t
      [(_ str:string)
       (define str* (syntax-e #'str))
       (define li (string->list str*))
       (define li* (map (λ (l) (findf char-numeric? l)) (list li (reverse li))))
       (qq-art stx (number #,(string->number (list->string li*))))])))
   
;; shorthand for many lines
(define-art-rewriter lines
  (λ (stx)
    (syntax-parse stx
      [(_ expr ...)
       #:with (expr* ...) (map (compose ~s syntax-e) (syntax->list #'(expr ...)))
       (qq-art stx (ix-- (text expr*) ...))])))

(define-art-realizer sum-performer
  (λ (stx)
    (syntax-parse stx
      [(_ expr ...)
       (define sum
         (for/sum ([e (syntax->list #'(expr ...))])
           (syntax-parse e
             [({~datum number} n:number) (syntax-e #'n)]
             [_ 0])))
       #`#,sum])))

(module+ test
  
  (check-equal?
   (perform sum-performer
     (lines 1abc2 pqr3stu8vwx a1b2c3d4e5f treb7uchet)
     (debug-perform quote-performer)
     (first-and-last)
     (debug-perform quote-performer))
   142))

;; done for part 2

(define-for-syntax (string-reverse str)
  (list->string (reverse (string->list str))))

;; lets add a step where we normalize the outermost text numbers first
(define-mapping-rewriter (normalize-text-nums [(: t text)])
  (λ (stx t)
    (syntax-parse t
      [(_ str:string)
       (define str* (syntax-e #'str))
       (define nums (map ~s '(one two three four five six seven eight nine)))
       (define indexed-matches
         (filter (compose cons? cdr)
          (for/list ([num nums] [i (in-naturals)])
           (cons i (regexp-match-positions* num str*)))))
       (define replaced
         (cond [(null? indexed-matches) str*]
               [else
                (define first (argmin cadr (map (match-lambda [(cons i matches) (cons i (argmin car matches))]) indexed-matches)))
                (define last (argmax cddr (map (match-lambda [(cons i matches) (cons i (argmax cdr matches))]) indexed-matches)))
                (string-append (substring str* 0 (cadr first)) (~s (add1 (car first)))
                               (if (< (cddr first) (cadr last)) (substring str* (cddr first) (cadr last)) "") (~s (add1 (car last))) (substring str* (cddr last)))]))
       (qq-art t (text #,replaced))])))

(module+ test
  
  (check-equal?
   (perform sum-performer
     (lines two1nine eightwothree abcone2threexyz xtwone3four 4nineeightseven2 zoneight234 4fourztnthreeone8mqmdfour 7pqrstsixteen twone)
     (normalize-text-nums)
     (debug-perform quote-performer)
     (first-and-last))
   346))