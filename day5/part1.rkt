#lang racket

(require art art/sequence/lib art/sequence/ravel "lib.rkt")

(perform (quote-performer)
  (input-converters)
  (@ [(type seed)] (input-seeds))
  (convert-all)
  (interpret day5)
  (delete interval-map)
  (run-apl (reduce apl:min *ctxt*)))
