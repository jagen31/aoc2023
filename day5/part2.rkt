#lang racket

(require art art/sequence/lib art/sequence/ravel "lib.rkt")

(perform (quote-performer)
  (input-seed-ranges)
  (input-converters)
  (interpret day5)
  (totalize-maps seed soil fertilizer water light temperature humidity location)
  (compose-maps* seed soil fertilizer water light temperature humidity location)
  (seed-ranges->seeds)
  (convert seed location)
  (delete interval-map seed-range)
  (run-apl (reduce apl:min *ctxt*)))