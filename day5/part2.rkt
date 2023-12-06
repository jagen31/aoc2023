#lang racket

(require art "lib.rkt")

(perform (quote-performer)
  (input-seed-ranges)
  (input-converters)
  (interpret day5)
  (totalize-maps seed soil fertilizer water light temperature humidity location)
  (compose-maps* seed soil fertilizer water light temperature humidity location)
  (seed-ranges->seeds)
  (convert seed location)
  (run-apl (reduce apl:min *ctxt*)))