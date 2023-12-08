#lang racket

(require art)
;; in←2 4⍴46 85 75 82 208 1412 1257 1410
;; ×/{+/((⍳⍺)×⍺-⍳⍺)>⍵}/[1]in

(perform (draw-seq-performer [400 200] [number-drawer])
  (ix-- (numbers 46 85 75 82) (numbers 208 1412 1257 1410))
  (run-apl (reduce apl:*
    (reduce (dyad-dfn (reduce apl:+ (apl:>= (apl:* (iota α #:start 1) (apl:- α (iota α #:start 1))) ω)))
            *ctxt* #:axis 0))))
