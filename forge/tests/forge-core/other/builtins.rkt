#lang forge/core

; Test that built-in macros, such as `reachable`, work in core
(set-option! 'verbosity 0)
(set-option! 'run_sterling 'off)

(sig Node)
(relation edges (Node Node))
(relation nodeList (Int Node))

;(pred Reach (reachable Node Node edges))
(pred SeqTest (&&
               (isSeqOf nodeList Node)))


;(test Reach-run #:preds [Reach] #:expect 'sat)