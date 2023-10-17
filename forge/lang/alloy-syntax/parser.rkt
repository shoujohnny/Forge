#lang brag

; Alloy reference: http://alloytools.org/download/alloy-language-reference.pdf
; Forge's grammar is somewhat simpler, and introduces some new constructs,
; but is heavily inspired by Alloy.

; Grammar was cleaned up circa Feb 12 2022, so if you're looking for
; module names, old XML-style instances, state/transition sublanguage,
; etc. they would need to be re-added from prior history.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Remaining notes re: parser cleanup

; TODO: custom errors for common Alloy keywords
;   e.g., fact, after, ...
; TODO: odd associativity for => interplay with quantification
; TODO: Does brag allow heuristics for error location?


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Starting production for the parser
; Two options: a model, or an evaluation request
AlloyModule :  
            Import* Paragraph*
            | EvalDecl*

; Import other Forge files by name
Import : /OPEN-TOK QualName (LEFT-SQUARE-TOK QualNameList RIGHT-SQUARE-TOK)? (AS-TOK Name)?
       | /OPEN-TOK FILE-PATH-TOK (AS-TOK Name)?

; Basic top-level constructs of a model: sigs, preds, commands, etc.
@Paragraph : 
            SigDecl 
          | PredDecl 
          | FunDecl 
          | AssertDecl 
          | CmdDecl
          | TestExpectDecl
          | SexprDecl
          | QueryDecl
          | EvalRelDecl
          | OptionDecl
          | InstDecl
          | ExampleDecl 
          | PropertyDecl
          | TestSuiteDecl

; NOTE: When extending sigs with "in" (subset sigs) is implemented,
;  if "sig A in B extends C" is allowed, update this to allow multiple SigExt
SigDecl : VAR-TOK? ABSTRACT-TOK? Mult? /SIG-TOK NameList SigExt? /LEFT-CURLY-TOK ArrowDeclList? /RIGHT-CURLY-TOK Block?
SigExt : EXTENDS-TOK QualName 
       | IN-TOK QualName (PLUS-TOK QualName)*

Mult : LONE-TOK | SOME-TOK | ONE-TOK | TWO-TOK
; for field etc. declaration
ArrowMult : LONE-TOK | SET-TOK | ONE-TOK | TWO-TOK | FUNC-TOK | PFUNC-TOK
; for helper fun/pred declaration
HelperMult : LONE-TOK | SET-TOK | ONE-TOK | FUNC-TOK | PFUNC-TOK
ParaDecl  : DISJ-TOK? NameList /COLON-TOK HelperMult? Expr8 ; rel-expr
QuantDecl : DISJ-TOK? NameList /COLON-TOK SET-TOK? Expr8 ; rel-expr

; ArrowDecl should only be used by sig field declaration right now; 
; note the optional VAR for Electrum. Remember that a preceding / means
;  to cut the token; it won't get included in the AST.
ArrowDecl : VAR-TOK? NameList /COLON-TOK ArrowMult ArrowExpr

PredType : WHEAT-TOK

; A predicate declaration can contain any number of formulas in its body
PredDecl : /PRED-TOK PredType? (QualName DOT-TOK)? Name ParaDecls? Block
; A function declaration should only ever contain a single expression in its body
;   the expression must be a rel-expr.
FunDecl : /FUN-TOK (QualName DOT-TOK)? Name ParaDecls? /COLON-TOK HelperMult? Expr8 /LEFT-CURLY-TOK Expr8 /RIGHT-CURLY-TOK
; A ParaDecls is a special declaration form for pred/fun definition, where every identifier
; is paired with an expr and (optional) multiplicity
ParaDecls : /LEFT-PAREN-TOK @ParaDeclList? /RIGHT-PAREN-TOK 
          | /LEFT-SQUARE-TOK @ParaDeclList? /RIGHT-SQUARE-TOK

AssertDecl : /ASSERT-TOK Name? Block
CmdDecl :  (Name /COLON-TOK)? (RUN-TOK | CHECK-TOK) (QualName | Block)? Scope? (/FOR-TOK Bounds)?

TestDecl : (Name /COLON-TOK)? (QualName | Block)? Scope? (/FOR-TOK Bounds)? /IS-TOK (SAT-TOK | UNSAT-TOK | THEOREM-TOK)
TestExpectDecl : TEST-TOK? EXPECT-TOK Name? TestBlock
TestBlock : /LEFT-CURLY-TOK TestDecl* /RIGHT-CURLY-TOK
Scope : /FOR-TOK Number (/BUT-TOK @TypescopeList)?
      | /FOR-TOK @TypescopeList
Typescope : EXACTLY-TOK? Number QualName
Const : NONE-TOK | UNIV-TOK | IDEN-TOK
      | MINUS-TOK? Number 


PropertyDecl : /ASSERT-TOK Name /IS-TOK (SUFFICIENT-TOK | NECESSARY-TOK) /FOR-TOK Name Scope? (/FOR-TOK Bounds)? 

TestSuiteDecl : /TEST-TOK /SUITE-TOK /FOR-TOK Name /LEFT-CURLY-TOK TestConstruct* /RIGHT-CURLY-TOK

@TestConstruct : ExampleDecl | TestExpectDecl | PropertyDecl


# UnOp : Mult
#      | NEG-TOK | NO-TOK | SET-TOK | HASH-TOK | TILDE-TOK | STAR-TOK | EXP-TOK
# BinOp : OR-TOK | AND-TOK | IFF-TOK | IMP-TOK | AMP-TOK
#       | PLUS-TOK | MINUS-TOK | PPLUS-TOK | DOT-TOK ;SUBT-TOK | SUPT-TOK
ArrowOp : (@Mult | SET-TOK)? ARROW-TOK (@Mult | SET-TOK)?
CompareOp : IN-TOK | EQ-TOK | LT-TOK | GT-TOK | LEQ-TOK | GEQ-TOK | IS-TOK | NI-TOK
Block : /LEFT-CURLY-TOK Expr* /RIGHT-CURLY-TOK
BlockOrBar : Block | BAR-TOK Expr 
Quant : ALL-TOK | NO-TOK | SUM-TOK | @Mult
QualName : (THIS-TOK /SLASH-TOK)? (@Name /SLASH-TOK)* @Name | INT-TOK | SUM-TOK

OptionDecl : /OPTION-TOK QualName (QualName | FILE-PATH-TOK | MINUS-TOK? Number)

Name : IDENTIFIER-TOK
NameList : @Name
         | @Name /COMMA-TOK @NameList
QualNameList : @QualName
             | @QualName /COMMA-TOK @QualNameList

; Used only in fun/pred definition
ParaDeclList : ParaDecl
             | ParaDecl /COMMA-TOK @ParaDeclList
; Used only in quantifiers / set comprehension
QuantDeclList : QuantDecl
              | QuantDecl /COMMA-TOK @QuantDeclList
; Used in field declaration
ArrowDeclList : ArrowDecl
              | ArrowDecl /COMMA-TOK @ArrowDeclList

TypescopeList : Typescope
              | Typescope /COMMA-TOK @TypescopeList

; Comma-separated list of rel-expr expressions
ExprList : Expr8
         | Expr8 /COMMA-TOK @ExprList


; Let declarations appear in formula-position, but may bind either
LetDecl : @Name /EQ-TOK (Expr | Expr8)
LetDeclList : LetDecl
            | LetDecl /COMMA-TOK @LetDeclList

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This chain of productions enforces operator precedence in brag.
; The list goes from weakest to strongest binding operators.

; HT: https://beautifulracket.com/basic-2/expressions.html
; "[W]hen the parser descends into a new rule, it creates a subnode in the parse tree inside the current node."

; This machinery is needed because, for better or worse, brag has no
; yacc-style "assoc" annotations, and so we need to enforce associativity
; and precedence ourselves. 

; Example: 
;   e := e /\ e | e => e | num
; as a BNF this is OK, but ambiguity must be resolved to build a parser. 
; Without "assoc" lists, we have to deny the parser undesireable derivations 
; ourselves, by controlling the flow of non-terminals. E.g.:
;   e  := e1 => e | @e1
;   e1 := e2 /\ e1 | @e2
;   e2 := num
;
; Note that, once we've progressed to e1, we no longer have the ability to 
; parse implication! Likewise, progressing into e2 removes the ability to 
; parse conjunction. "e1 => e" simply cannot have another implication on
; the LHS of the implication; others must group to the right. Neither side
; of "e2 /\ e1" can be an implication. Etc.

; Note on brag syntax:
; @: splice, merges elements of a node into the surrounding node
;   LHS splice: *always* merged into surrounding node
;   RHS splice: @'d pattern element is spliced if it appears

; Following Alloy's lead, we don't try to make the parser disambiguate 
; "formulas" and "expressions" --- just produce a parse tree that the 
; expander can handle. Alloy 6 spec:
;    https://alloytools.org/spec.html
; Addendum Oct 2023: we now have a rough separation, around Expr8.

; "All binary operators associate to the left, with the exception of 
;  implication and sequence, which associate to the right, and of 
;  binary temporal connectives which are not associative."

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Boolean-valued expressions ("formulas")
; and top of instance-declaration expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Expr    : @Expr1
        | LET-TOK LetDeclList BlockOrBar
        | BIND-TOK LetDeclList BlockOrBar
        | Quant DISJ-TOK? QuantDeclList BlockOrBar
Expr1   : @Expr2  | Expr1 OR-TOK Expr2
Expr2   : @Expr3  | Expr2 IFF-TOK Expr3
;; right assoc ITE (formula case)
Expr3   : @Expr4  | Expr4 IMP-TOK Expr3 (ELSE-TOK Expr3)?
;;Expr3   : Expr4 IMP-TOK Expr3 (ELSE-TOK Expr3)? | Expr4       
Expr4   : @Expr4.5  | Expr4 AND-TOK Expr4.5
; Electrum binary operators (not associative)
Expr4.5 : @Expr5  | Expr5 UNTIL-TOK Expr5
                  | Expr5 RELEASE-TOK Expr5
                  | Expr5 SINCE-TOK Expr5
                  | Expr5 TRIGGERED-TOK Expr5
Expr5   : @Expr6  | NEG-TOK Expr5
                  | ALWAYS-TOK Expr5
                  | EVENTUALLY-TOK Expr5
                  | AFTER-TOK Expr5
                  | BEFORE-TOK Expr5
                  | ONCE-TOK Expr5
                  | HISTORICALLY-TOK Expr5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Transitioning from formulas to rel-expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The parser should not accept a rel-expr being used as a formula.
; When a rel-expr is expected, advance to that non-terminal (Expr8)
; Formula context also has its own set of base/escape cases. These may appear to duplicate 
; the rel-expr context's, but they do not.

;Expr6   : @Expr7  | Expr6 NEG-TOK? CompareOp Expr7
; Expr7   : @Expr8 | (NO-TOK | SOME-TOK | LONE-TOK | ONE-TOK | TWO-TOK | SET-TOK) Expr8

Expr6   : @Expr7  | Expr8 NEG-TOK? CompareOp Expr8
Expr7   :           (NO-TOK | SOME-TOK | LONE-TOK | ONE-TOK | TWO-TOK | SET-TOK) Expr8
                  | /LEFT-PAREN-TOK @Expr /RIGHT-PAREN-TOK ; paren grouping (formula)
                  | QualName ; name (formula context, could be e.g. a 0-ary pred)
                  | Block ; non-quantifier grouping outside predicate-declaration bodies
                  | Name LEFT-SQUARE-TOK ExprList RIGHT-SQUARE-TOK ; predicate invocation (arity > 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Relation-valued expressions ("rel-expressions")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; DO NOT CHANGE THIS: Expr8 IS THE START OF rel-expressions.

Expr8   : @Expr9  | Expr8 (PLUS-TOK | MINUS-TOK | SEMICOLON-TOK) Expr10
; Support trailing semicolon in `inst` and `example` syntax. 
                 ; | Expr10 /SEMICOLON-TOK

Expr9   : @Expr10 | CARD-TOK Expr9
Expr10  : @Expr11 | Expr10 PPLUS-TOK Expr11
Expr11  : @Expr12 | Expr11 AMP-TOK Expr12
Expr12  : @Expr13 | Expr12 ArrowOp Expr13
Expr13  : @Expr14 | Expr13 (SUBT-TOK | SUPT-TOK) Expr14
; The parser cannot distinguish helper-function invocation from a box join;
; x[y] might be either and both may be used in the same (rel-expr) context.
Expr14  : @Expr15 | Expr14 LEFT-SQUARE-TOK ExprList RIGHT-SQUARE-TOK
Expr15  : @Expr16 | Expr15 DOT-TOK Expr16
                  | Name LEFT-SQUARE-TOK ExprList RIGHT-SQUARE-TOK
Expr16  : @Expr17 | Expr16 PRIME-TOK
Expr17  : @Expr18 | (TILDE-TOK | EXP-TOK | STAR-TOK) Expr17
Expr18  :          
          Const ; Relational or integer constant
        | QualName ; name (rel-expr context, could be e.g. a 0-ary helper function or quant variable)        
        | BACKQUOTE-TOK Name ; atom name
        | LEFT-CURLY-TOK QuantDeclList BlockOrBar RIGHT-CURLY-TOK ; set comprehension
        | /LEFT-PAREN-TOK @Expr8 /RIGHT-PAREN-TOK ; paren grouping (rel-expr context)        
        | /LEFT-CURLY-TOK @Expr8 /RIGHT-CURLY-TOK ; paren grouping (single rel-expr)
        | Sexpr
        ;; right assoc ITE (rel-expr case, which requires an else branch)
        | Expr4 IMP-TOK Expr8 ELSE-TOK Expr8

        ; Alloy-language stubs, unsupported
        ;| THIS-TOK
        ;| AT-TOK Name
        ; | Block


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Escape to forge/core 

SexprDecl : Sexpr
Sexpr : SEXPR-TOK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Partial Instances

; In older versions, instances were pasted in as raw XML.
; This isn't used nor tested, so disabled
;InstanceDecl : INSTANCE-TOK

; Partial `inst` declaration
InstDecl : /INST-TOK Name Bounds Scope?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Productions for the evaluation-request case
;   i.e., this isn't a Forge model, but rather a query 
;   from the evaluator:
EvalRelDecl : ArrowDecl
EvalDecl : EVAL-TOK Expr | EVAL-TOK Expr8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Bounds : EXACTLY-TOK? @ExprList
       | EXACTLY-TOK? @Block

ExampleDecl : /EXAMPLE-TOK Name /IS-TOK Expr /FOR-TOK Bounds

; ??? used where?
QueryDecl : @Name /COLON-TOK ArrowExpr /EQ-TOK Expr

ArrowExpr : QualName
          | QualName /ARROW-TOK @ArrowExpr


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Ints
NumberList : Number
           | Number /COMMA-TOK @NumberList

Number : NUM-CONST-TOK
