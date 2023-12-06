#lang racket

(require art 2htdp/image (for-syntax syntax/parse syntax/id-table racket/list))

(define-art-embedding (cube-game [rounds])
  (λ (stx ctxt)
    (syntax-parse stx
      [(head:id expr ...)
       (compile-rewrite-exprs (list (qq-art/no-context stx (@ () expr ...))) '())])))

(define-art-embedding (cube-round [blocks])
  (λ (stx ctxt)
    (syntax-parse stx
      [(head:id expr ...)
       (compile-rewrite-exprs (list (qq-art/no-context stx (@ () expr ...))) '())])))

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
            (for ([e (syntax->list #'(expr ...))]) (free-id-table-set! expr-by-color (expr-color e) (syntax-parse e [({~datum blocks} n) (qq-art e (number n))])))
            (qq-art round (seq #,@(for/list ([co colors] [i (in-naturals)])
              (define e (free-id-table-ref expr-by-color co (λ () #f)))
              (if e (qq-art e (ix@ #,i #,e)) (qq-art stx (@ [(index #,i) (color #,co)] (number 0)))))))])))
       (qq-art cg (seq #,@results))])))

(define-interpretation test1)

(interpretation+ test1
  [data (make-cube-games
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

(println "the cube games")
   
(perform (draw-seq-performer [1000 600] [cube-game-drawer])
  (ix@ 0 (seq (data) (interpret test1))))

(println "translated to numbers")

(perform (draw-seq-performer [800 200] [number-drawer])
  (ix@ 0 (seq (data) (interpret test1)))
  (rewrite-in-seq (translate-cube-game red green blue) (run-apl ((each mix) *ctxt*))))
