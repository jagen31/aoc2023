#lang racket

(require art "lib.rkt")

(perform (quote-performer)
  (input-converters)
  (@ [(type seed)] (input-seeds))
  (convert-all)
  (interpret day5)
  (run-apl (reduce apl:min *ctxt*)))