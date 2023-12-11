#lang racket

(require art art/sequence/lib art/sequence/ravel 2htdp/image (for-syntax syntax/parse syntax/id-table racket/list))

(provide (all-defined-out))

(define-art-embedding (cube-game [rounds])
  (λ (stx ctxt)
    (syntax-parse stx
      [(head:id expr ...)
       (compile-rewrite-exprs (list (quasisyntax/loc stx (@ () expr ...))) '())])))

(define-art-embedding (cube-round [blocks])
  (λ (stx ctxt)
    (syntax-parse stx
      [(head:id expr ...)
       (compile-rewrite-exprs (list (quasisyntax/loc stx (@ () expr ...))) '())])))

(define-coordinate (color [c]))
(define-hom-merge-rule color (λ (l r _ __ ___) (or r l)))
(define-hom-within?-rule color (λ (l r _ __ ___) #t))
(define-for-syntax (expr-color stx)
  (syntax-parse (context-ref (get-id-ctxt stx) #'color)
    [(_ c:id) #'c]
    [_ #f]))

(define-art-object (blocks [num]))

(define-art-rewriter make-cube-games
  (λ (stx)
    (syntax-parse stx
      [(_ {[(num:number color*:id) ...] ...} ...)
       (qq-art stx
        (ix-- (cube-game (ix-- (cube-round (@ [(color color*)] (blocks num)) ...) ...)) ...))])))

(define-mapping-rewriter (translate-cube-game [(: cg cube-game)])
  (λ (stx cg)
    (syntax-parse #`(#,stx #,cg)
      [((_ the-color:id ...) (_ cg-expr ...))
       (define colors (syntax->list #'(the-color ...)))
       (define rounds (context-ref* (syntax->list #'(cg-expr ...)) #'cube-round))
       (define results (for/list ([round rounds])
         (syntax-parse round
           [(_ expr ...)
            (define expr-by-color (make-free-id-table))
            (for ([e (syntax->list #'(expr ...))])
              (free-id-table-set! expr-by-color (expr-color e) (syntax-parse e [({~datum blocks} n) (qq-art e (number n))])))
            (qq-art round (seq #,@(for/list ([co colors] [i (in-naturals)])
              (define e (free-id-table-ref expr-by-color co (λ () #f)))
              (if e (qq-art e (ix@ #,i #,e)) (qq-art stx (@ [(index #,i) (color #,co)] (number 0)))))))])))
       (qq-art cg (seq #,@results))])))

(define-interpretation day2)

(interpretation+ day2
  [test-data (make-cube-games
         {[(3 blue) (4 red)]
          [(1 red) (2 green) (6 blue)]
          [(2 green)]}
         {[(1 blue) (2 green)]
          [(3 green) (4 blue) (1 red)]
          [(1 green) (1 blue)]}
         {[(8 green) (6 blue) (20 red)]
          [(5 blue) (4 red) (13 green)]
          [(5 green) (1 red)]}
         {[(1 green) (3 red) (6 blue)]
          [(3 green) (6 red)]
          [(3 green) (15 blue) (14 red)]}
         {[(6 red) (1 blue) (3 green)]
          [(2 blue) (1 red) (2 green)]})])

(define-syntax cube-game-drawer
  (drawer/s
  (λ (stx width height)
    (syntax-parse stx
      [({~datum cube-game} expr ...)
       (define rounds (context-ref* (syntax->list #'(expr ...)) #'cube-round))
       (define round-height (floor (/ (- height (* 15 (length rounds))) (length rounds))))
       (define round-images
         (for/list ([round rounds])
           (define cube-images
             (syntax-parse round
               ;; FIXME jagen not modular
               [({~datum cube-round} inner-e* ...)
                (apply append
                       (for/list ([inner-e (syntax->list #'(inner-e* ...))])
                         (syntax-parse inner-e
                           [({~datum blocks} n:number)
                            ;; FIXME jagen broken
                            (define color (syntax->datum (expr-color inner-e)))
                            (build-list (syntax-e #'n) (λ (_) (list (random (- width 15)) (random (- round-height 15)) #`(rectangle 15 15 'solid '#,color))))])))]))
           (for/fold ([acc #`(rectangle #,width #,round-height 'solid 'transparent)])
                     ([im cube-images])
             #`(overlay/xy #,(caddr im) #,(- (car im)) #,(- (cadr im)) #,acc))))
       #`(above empty-image #,@(cons (car round-images) (flatten (for/list ([im (cdr round-images)]) #`(above (rectangle #,width 15 'solid 'black) #,im)))))]))))

#;(println "the cube games")
   
#;(perform (draw-seq-performer [800 400] [cube-game-drawer])
  (ix@ 0 (seq (test-data) (interpret day2))))

#;(println "translated to numbers")

#;(perform (draw-seq-performer [800 200] [number-drawer])
  (ix@ 0 (seq (test-data) (interpret day2)))
  (rewrite-in-seq
   (translate-cube-game red green blue)
   (run-apl ((each mix) *ctxt*))))

(interpretation+ day2
  [solve-part1
   (run-apl
     (reduce apl:+ 
      (apl:* 
        (apl:+ (lit 1) (iota (rho *ctxt*)))
        ((each (monad-dfn (reduce apl:and (ravel (apl:>= (mix (replicate (apl:first (rho ω)) (enclose (lit 12 13 14)))) ω))))) ((each mix) *ctxt*)))))])

#;(println "solved")

#;(perform (draw-seq-performer [100 100] [number-drawer])
  (ix@ 0 (seq (test-data) (interpret day2)))
  (rewrite-in-seq
   (translate-cube-game red green blue)
   (solve-part1) (interpret day2)))

#;(perform (draw-seq-performer [800 400] [number-drawer])
  (test-data)
  (interpret day2)
  (translate-cube-game red green blue)
  (run-apl (reduce apl:+ ((each (monad-dfn (reduce apl:* (reduce apl:max ω #:axis 0)))) ((each mix) *ctxt*)))))


(interpretation+ day2
  [input-data
   (make-cube-games
    {[(1 blue) (8 green)]
     [(14 green) (15 blue)]
     [(3 green) (9 blue)]
     [(8 green) (8 blue) (1 red)]
     [(1 red) (9 green) (10 blue)]}
    {[(3 blue) (1 green) (2 red)]
     [(2 red) (2 green) (5 blue)]
     [(3 green) (10 blue)]
     [(8 red) (1 blue)]
     [(3 red) (1 green) (5 blue)]
     [(1 blue) (5 red) (3 green)]}
    {[(4 green) (1 blue)]
     [(6 blue) (5 green) (1 red)]
     [(11 green) (10 blue)]}
    {[(12 blue) (12 green) (3 red)]
     [(15 blue) (1 green) (10 red)]
     [(8 blue) (3 red) (2 green)]
     [(14 red) (8 blue)]}
    {[(7 blue) (8 red) (5 green)]
     [(15 blue) (16 red) (14 green)]
     [(3 blue) (14 red) (10 green)]}
    {[(4 blue) (13 red)]
     [(1 green) (13 blue) (11 red)]
     [(4 red) (19 blue)]
     [(18 blue) (10 red) (1 green)]}
    {[(8 green) (3 blue) (3 red)]
     [(2 red) (7 green) (10 blue)]
     [(6 green) (11 red) (3 blue)]}
    {[(10 red) (6 green) (1 blue)]
     [(15 green) (10 red) (3 blue)]
     [(8 red) (10 green) (5 blue)]}
    {[(2 green) (8 blue) (1 red)]
     [(6 blue) (10 red)]
     [(13 blue) (12 red) (7 green)]}
    {[(2 blue) (8 red) (10 green)]
     [(1 green) (2 blue)]
     [(1 red) (1 green)]
     [(7 red) (2 blue) (1 green)]}
    {[(8 green) (1 blue)]
     [(6 green)]
     [(2 green) (1 blue)]
     [(2 blue) (11 green)]
     [(1 red) (12 green)]}
    {[(3 red) (2 green) (15 blue)]
     [(1 blue) (1 green) (4 red)]
     [(1 green) (12 blue) (3 red)]
     [(1 red) (10 blue)]
     [(3 red) (2 green) (14 blue)]
     [(3 red) (13 blue)]}
    {[(7 blue) (5 red)]
     [(7 red) (3 green) (9 blue)]
     [(9 green) (7 blue) (7 red)]
     [(6 blue) (8 red)]
     [(11 red)]
     [(3 green) (7 blue) (8 red)]}
    {[(4 blue) (6 green) (7 red)]
     [(8 red) (4 green) (11 blue)]
     [(3 green) (9 red) (13 blue)]}
    {[(3 green) (1 blue) (5 red)]
     [(2 red)]
     [(1 red) (4 green)]}
    {[(1 green) (7 blue)]
     [(3 red) (5 blue)]
     [(1 green) (5 blue)]
     [(5 blue) (1 green)]
     [(1 green) (1 red) (13 blue)]}
    {[(4 blue) (2 red) (4 green)]
     [(1 blue) (7 red) (4 green)]
     [(4 red) (4 green) (10 blue)]
     [(1 blue) (4 red) (14 green)]}
    {[(7 blue) (5 green)]
     [(4 blue) (3 green)]
     [(1 red) (6 green) (7 blue)]}
    {[(10 blue) (3 red) (6 green)]
     [(3 blue) (4 red) (17 green)]
     [(19 green) (3 red) (3 blue)]
     [(19 green) (3 blue)]
     [(4 red) (7 green) (7 blue)]
     [(10 blue) (13 green) (1 red)]}
    {[(3 blue) (6 red) (1 green)]
     [(6 green) (7 red) (18 blue)]
     [(1 green) (5 red) (14 blue)]
     [(1 green) (12 blue) (8 red)]}
    {[(16 blue) (7 green) (13 red)]
     [(11 red) (7 blue) (5 green)]
     [(4 green) (3 blue)]}
    {[(14 blue) (6 red) (1 green)]
     [(9 red) (1 green) (11 blue)]
     [(3 red) (13 blue)]
     [(6 red) (10 blue)]
     [(13 red) (1 green) (2 blue)]}
    {[(17 red) (1 blue) (13 green)]
     [(19 green) (1 blue) (3 red)]
     [(7 red) (19 green)]
     [(16 red) (10 green)]
     [(16 red) (12 green) (1 blue)]}
    {[(1 green) (2 blue)]
     [(10 green) (4 blue)]
     [(8 blue) (11 green) (14 red)]}
    {[(9 blue) (10 red)]
     [(2 red) (7 green) (5 blue)]
     [(4 green) (10 red) (5 blue)]
     [(6 red) (6 blue)]
     [(12 blue) (4 green)]}
    {[(9 red) (2 blue) (5 green)]
     [(3 red) (4 green) (1 blue)]
     [(5 red) (2 blue) (13 green)]}
    {[(1 green) (14 blue) (2 red)]
     [(9 red) (7 blue) (7 green)]
     [(9 blue) (10 red) (7 green)]
     [(1 blue) (5 red) (3 green)]
     [(1 blue) (4 red)]
     [(9 red) (1 green)]}
    {[(11 red) (13 blue) (12 green)]
     [(8 blue) (4 green) (6 red)]
     [(2 blue) (9 green)]}
    {[(5 green) (16 red) (1 blue)]
     [(6 blue) (3 green) (2 red)]
     [(1 green) (7 blue) (9 red)]}
    {[(1 green) (2 blue) (2 red)]
     [(7 blue) (5 red)]
     [(2 red) (1 blue)]
     [(6 red) (1 green) (6 blue)]}
    {[(5 green) (7 blue) (14 red)]
     [(19 red) (9 green) (9 blue)]
     [(4 green) (8 red) (12 blue)]
     [(20 red) (12 green)]
     [(10 red) (3 green) (6 blue)]
     [(5 blue) (17 red) (8 green)]}
    {[(6 blue) (6 red) (8 green)]
     [(6 blue) (3 green) (7 red)]
     [(4 red) (6 green) (4 blue)]
     [(5 green) (3 blue) (5 red)]
     [(8 blue) (6 red) (5 green)]}
    {[(5 red) (15 green) (3 blue)]
     [(4 green) (8 red)]
     [(6 blue) (17 green) (2 red)]}
    {[(12 blue) (1 green)]
     [(3 red) (14 blue) (1 green)]
     [(1 green) (16 blue) (3 red)]}
    {[(2 red) (16 blue)]
     [(17 blue) (5 green)]
     [(10 blue) (3 green)]
     [(1 blue) (2 green)]
     [(4 blue) (4 green)]}
    {[(7 blue) (8 red) (4 green)]
     [(3 red) (13 green) (14 blue)]
     [(17 green) (2 blue) (8 red)]
     [(2 red) (13 blue) (2 green)]
     [(12 blue) (1 red) (9 green)]
     [(12 green) (10 blue)]}
    {[(5 green)]
     [(3 green) (14 red)]
     [(2 red) (1 blue)]
     [(11 green) (1 blue)]
     [(8 green) (18 red) (1 blue)]
     [(1 blue) (16 red)]}
    {[(5 red)]
     [(9 green) (11 blue) (7 red)]
     [(2 blue) (2 green) (1 red)]
     [(3 blue) (7 red)]
     [(5 red) (8 blue)]}
    {[(1 blue) (7 green) (6 red)]
     [(18 green) (2 red)]
     [(1 blue) (19 green) (6 red)]
     [(2 green) (3 blue) (9 red)]
     [(14 green) (4 red) (3 blue)]
     [(16 green) (4 red) (1 blue)]}
    {[(14 red) (2 green) (2 blue)]
     [(2 blue) (2 red) (4 green)]
     [(8 red) (2 blue)]
     [(1 green) (17 red)]
     [(10 red)]
     [(2 green) (3 red)]}
    {[(11 green) (1 red) (1 blue)]
     [(2 blue) (18 green) (3 red)]
     [(2 blue) (2 green) (8 red)]
     [(7 red) (1 blue) (17 green)]
     [(14 green) (2 blue)]
     [(2 blue) (14 red) (18 green)]}
    {[(2 red) (3 green) (15 blue)]
     [(3 green) (18 blue)]
     [(9 blue) (2 red)]}
    {[(4 green) (2 blue)]
     [(10 blue) (2 red) (14 green)]
     [(13 green) (1 red) (11 blue)]
     [(9 green) (10 blue)]
     [(14 green) (8 red) (8 blue)]}
    {[(10 red)]
     [(3 red) (8 blue) (1 green)]
     [(10 red) (2 blue) (1 green)]
     [(5 green) (7 blue) (17 red)]
     [(3 green) (18 red) (6 blue)]
     [(3 green) (11 blue) (3 red)]}
    {[(10 blue) (5 green)]
     [(14 blue) (5 green)]
     [(10 green) (14 blue) (1 red)]
     [(1 red) (5 green) (18 blue)]
     [(5 green) (5 blue) (1 red)]
     [(17 blue) (12 green) (1 red)]}
    {[(2 blue) (2 green)]
     [(1 green) (1 blue)]
     [(2 blue) (3 green)]
     [(1 red)]
     [(2 green)]}
    {[(6 blue) (1 red) (12 green)]
     [(2 red) (3 green) (4 blue)]
     [(1 blue) (13 green) (6 red)]
     [(12 green) (4 blue) (5 red)]}
    {[(4 green) (5 red)]
     [(19 green) (1 blue) (11 red)]
     [(4 red) (8 green)]
     [(10 red) (1 blue) (16 green)]}
    {[(12 red) (2 blue)]
     [(6 green)]
     [(1 green) (9 red)]}
    {[(1 green) (1 blue) (17 red)]
     [(1 blue) (14 red) (1 green)]
     [(2 blue) (6 red)]}
    {[(12 green) (9 blue) (1 red)]
     [(6 green) (2 blue) (1 red)]
     [(14 green) (5 blue)]
     [(1 green) (2 red) (12 blue)]
     [(4 green) (2 red) (8 blue)]
     [(1 green) (5 blue)]}
    {[(1 green) (7 blue) (3 red)]
     [(6 blue) (5 red) (5 green)]
     [(7 green)]
     [(5 blue) (1 green) (6 red)]}
    {[(3 blue) (1 red)]
     [(1 blue) (3 green)]
     [(2 green) (5 blue)]
     [(4 blue) (3 green)]
     [(1 green)]
     [(2 green) (5 blue) (1 red)]}
    {[(13 blue) (3 red)]
     [(17 blue) (1 green) (8 red)]
     [(2 green) (2 red) (11 blue)]
     [(2 green) (1 red) (4 blue)]
     [(8 red)]}
    {[(1 green)]
     [(3 blue) (5 red)]
     [(1 blue) (1 green) (4 red)]
     [(1 red) (10 blue)]
     [(4 red) (17 blue)]}
    {[(4 blue) (12 green) (12 red)]
     [(6 green) (10 blue) (2 red)]
     [(8 green) (11 red) (3 blue)]
     [(6 green) (10 red) (10 blue)]
     [(7 red) (5 green)]}
    {[(2 green) (8 blue) (3 red)]
     [(17 blue) (1 green) (9 red)]
     [(4 red) (7 blue)]}
    {[(4 blue) (1 red)]
     [(2 blue) (1 green)]
     [(2 red) (5 green) (4 blue)]
     [(7 green) (5 blue) (2 red)]}
    {[(4 green) (5 red)]
     [(4 green) (7 red) (1 blue)]
     [(15 red) (8 green)]}
    {[(6 green) (3 red)]
     [(4 red) (6 green) (4 blue)]
     [(5 green) (4 blue) (1 red)]
     [(3 red) (4 blue) (2 green)]
     [(7 red) (3 blue)]
     [(1 green) (1 blue)]}
    {[(7 red) (10 blue) (7 green)]
     [(11 blue) (9 green) (1 red)]
     [(11 red) (1 green) (3 blue)]
     [(3 green) (13 red) (2 blue)]
     [(7 green) (2 blue) (1 red)]}
    {[(10 green) (9 red)]
     [(6 blue) (10 red) (11 green)]
     [(7 red) (2 blue) (2 green)]}
    {[(10 green) (2 blue)]
     [(1 red)]
     [(9 green)]
     [(2 blue) (8 green)]
     [(1 blue) (1 green) (1 red)]}
    {[(2 green)]
     [(8 red)]
     [(4 green) (5 red) (2 blue)]}
    {[(2 red) (16 blue)]
     [(5 green) (11 blue) (3 red)]
     [(15 blue) (4 green) (3 red)]}
    {[(2 green) (3 blue)]
     [(1 red) (3 blue)]
     [(1 green) (4 blue)]
     [(1 blue) (2 green) (1 red)]
     [(2 blue)]
     [(1 red) (3 green) (5 blue)]}
    {[(2 blue) (11 red) (8 green)]
     [(13 blue) (19 red) (13 green)]
     [(3 red) (3 green) (3 blue)]
     [(2 red) (5 blue) (16 green)]
     [(13 red) (4 blue) (5 green)]}
    {[(1 blue) (9 red) (7 green)]
     [(3 green) (9 red) (4 blue)]
     [(7 green) (5 blue)]
     [(3 green) (9 red) (3 blue)]
     [(5 red) (1 blue) (8 green)]
     [(8 green) (1 blue) (4 red)]}
    {[(10 green) (1 red) (2 blue)]
     [(1 red) (11 green)]
     [(1 blue) (1 green)]
     [(8 green)]
     [(11 green) (1 red)]
     [(1 green) (3 blue)]}
    {[(8 red) (15 blue) (18 green)]
     [(14 blue) (9 red) (8 green)]
     [(6 blue) (2 red) (2 green)]
     [(1 green) (4 blue) (9 red)]
     [(3 blue) (15 green) (10 red)]}
    {[(10 red) (10 green) (6 blue)]
     [(3 green) (6 blue) (13 red)]
     [(7 green) (6 red) (12 blue)]}
    {[(12 red) (5 blue)]
     [(13 red) (6 green) (11 blue)]
     [(11 green) (7 red) (11 blue)]}
    {[(8 blue) (1 red) (3 green)]
     [(1 blue)]
     [(6 blue) (4 green)]}
    {[(1 red) (3 blue) (8 green)]
     [(6 green) (3 blue) (1 red)]
     [(18 green) (1 red)]
     [(3 blue) (1 red) (14 green)]
     [(5 green) (1 blue)]
     [(3 blue) (1 green)]}
    {[(4 blue) (1 red) (3 green)]
     [(6 blue)]
     [(11 green) (6 blue) (3 red)]
     [(10 green) (4 red)]
     [(2 blue) (2 red) (6 green)]}
    {[(7 blue)]
     [(15 blue) (2 red) (1 green)]
     [(2 blue) (5 red)]
     [(2 red) (15 blue)]
     [(4 red) (15 blue)]
     [(9 blue) (5 red)]}
    {[(12 blue) (8 green) (15 red)]
     [(12 blue) (19 red) (16 green)]
     [(6 blue) (5 green) (16 red)]}
    {[(2 red) (7 blue) (14 green)]
     [(1 red) (3 green) (1 blue)]
     [(4 blue) (8 green) (10 red)]}
    {[(3 red) (5 green)]
     [(2 blue) (1 red) (18 green)]
     [(4 red) (15 green) (2 blue)]
     [(18 green) (2 blue) (7 red)]
     [(7 green) (6 red)]}
    {[(8 green) (5 red) (9 blue)]
     [(14 blue) (13 red) (6 green)]
     [(14 blue) (7 red) (4 green)]
     [(3 blue) (16 red) (4 green)]
     [(5 green) (13 blue) (2 red)]
     [(16 blue) (2 green) (5 red)]}
    {[(10 green) (10 red) (4 blue)]
     [(13 red) (2 blue)]
     [(8 green) (10 red) (7 blue)]}
    {[(1 blue) (4 red) (7 green)]
     [(3 green) (2 blue) (3 red)]
     [(1 blue) (3 red) (3 green)]
     [(6 red) (4 green) (5 blue)]
     [(3 blue) (6 green)]}
    {[(3 red) (1 blue) (2 green)]
     [(6 blue) (2 green) (2 red)]
     [(6 green) (1 blue)]
     [(11 blue) (4 red) (10 green)]}
    {[(9 red) (12 green) (1 blue)]
     [(7 red) (5 green) (3 blue)]
     [(3 green) (8 blue) (11 red)]
     [(1 red) (3 blue) (10 green)]}
    {[(2 blue) (2 red)]
     [(3 blue) (3 red)]
     [(2 green) (2 red)]
     [(2 red) (4 green)]}
    {[(2 green) (5 blue) (9 red)]
     [(4 red) (2 green) (5 blue)]
     [(6 red) (2 green) (6 blue)]
     [(4 red) (4 blue) (1 green)]}
    {[(17 red) (3 blue) (2 green)]
     [(5 red) (1 green) (5 blue)]
     [(4 blue) (3 green) (7 red)]}
    {[(7 green) (1 blue) (1 red)]
     [(1 blue) (1 red) (10 green)]
     [(1 blue) (2 green)]
     [(5 green)]
     [(7 green) (1 blue)]
     [(1 red) (3 green)]}
    {[(6 red) (11 blue) (3 green)]
     [(3 blue) (4 green) (3 red)]
     [(4 blue) (17 red) (4 green)]
     [(17 blue) (3 green) (4 red)]
     [(3 red) (9 blue) (4 green)]
     [(6 red) (9 blue) (9 green)]}
    {[(9 red) (4 green) (1 blue)]
     [(6 green) (5 red) (11 blue)]
     [(3 blue) (8 red)]}
    {[(3 red) (7 blue) (1 green)]
     [(11 blue) (3 green) (4 red)]
     [(1 red) (8 blue) (3 green)]
     [(10 red) (4 blue) (3 green)]}
    {[(2 blue) (1 red)]
     [(7 red) (1 blue)]
     [(4 green) (11 red) (5 blue)]}
    {[(13 red) (11 green) (10 blue)]
     [(3 blue) (10 green)]
     [(3 blue) (7 red) (3 green)]}
    {[(9 green) (8 blue) (4 red)]
     [(7 blue) (9 green) (5 red)]
     [(16 green) (5 red)]
     [(4 blue)]}
    {[(9 green)]
     [(1 blue) (16 green)]
     [(2 green) (2 red)]
     [(1 green) (1 blue)]
     [(6 green)]
     [(7 green) (2 red)]}
    {[(6 green) (8 red) (5 blue)]
     [(2 blue) (14 green) (17 red)]
     [(2 blue) (15 green) (16 red)]
     [(16 green) (1 red)]
     [(16 red) (2 green) (1 blue)]
     [(18 red) (13 green) (6 blue)]}
    {[(9 green) (8 red) (11 blue)]
     [(11 green) (13 red) (4 blue)]
     [(7 blue) (1 green) (6 red)]
     [(1 blue) (12 red) (7 green)]}
    {[(1 green) (12 blue) (4 red)]
     [(8 blue) (5 red) (1 green)]
     [(1 green) (8 blue) (7 red)]
     [(1 green) (5 red) (6 blue)]
     [(3 blue) (1 green) (1 red)]
     [(4 blue) (1 green) (4 red)]}
    {[(1 blue) (2 green) (2 red)]
     [(2 red) (8 green)]
     [(14 green) (1 blue)]
     [(1 red) (2 green)]
     [(1 blue) (1 green) (2 red)]
     [(6 green) (2 red)]}
    {[(6 green) (15 red) (12 blue)]
     [(9 red)]
     [(16 red)]
     [(17 red) (3 blue) (7 green)]})])