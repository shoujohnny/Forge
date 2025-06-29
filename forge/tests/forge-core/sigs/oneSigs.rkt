#lang forge/core

(set-option! 'verbose 0)

(sig UniqueObject #:one)

(sig Stuff)

(test oneSigEnforced
      #:preds [(= (card UniqueObject) (int 1))]
      #:expect checked)
(test oneSigIsntPersistent
      #:preds [(= (card Stuff) (int 2))]
      #:expect sat)


(sig Thing)
(sig SpecialThing #:one #:extends Thing)
(sig UnspecialThing #:extends Thing)

(test oneExtendActuallyExtends
      #:preds [(in SpecialThing Thing)]
      #:expect checked)
(test oneExtendEnforced
      #:preds [(= (card SpecialThing) (int 1))]
      #:expect checked)
(test oneExtendDoesntSpread
      #:preds [(= (card UnspecialThing) (int 2))] 
      #:expect sat)
