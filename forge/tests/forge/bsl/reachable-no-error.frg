#lang forge --/bsl
-- Is *reachable* itself being created improperly? (reachable (join n next) n next) seems like the right expansion.
-- Yes: evaluating `reachable` produces ... itself.
option run_sterling off


sig Node {
    next: lone Node
}

sig A {
    field: one Node
}

pred cycle {
    all n: Node | reachable[n.next, n, next]
}

test expect {
    canRun: {cycle} is sat
}