#lang forge

option backend smtlibtor
option verbose 0

/*
 * Alloy model of paragraph numbering
 *
 * This model addresses some issues that arose in the design of a text tagging tool. The
 * tool receives as input a text stream in which paragraphs are tagged with style names,
 * along with a style sheet that indicates how paragraphs of a given style are to be numbered.
 * In practice, the style sheet includes such details as what symbols to use for numbering (eg,
 * roman numericals, letters of the alphabet, etc), but these details are uninteresting.
 *
 * In the simplest case, the styles are organized into chains. For example, there may be a
 * single chain, chapter-section-subsection, so that chapters are numbered 1, 2, 3, etc,
 * sections are numbered 1.1, 1.2, 1.3, etc, and subsections are numbered 1.1.1, 1.1.2,
 * etc, each paragraph being numbered according to a number associated with its own
 * style, and a number for each ancestor.
 *
 * Some styles, however, should be numbered independently of one another, but still
 * according to the same ancestors. For example, we might also have a figure style
 * that is numbered, like section, according to its chapter, with figures and sections in
 * some arbitrary interleaving, the numbering of one not affecting the other.
 *
 * So in our style hierarchy, a style can have more than one "child". A more tricky complication
 * allows multiple parents. We might want to have an appendix style, for example, with a
 * different numbering from the chapter style, but would want section and subsection to work
 * within appendix exactly as they would work within chapter. So the first section in an
 * appendix numbered A might be numbered A.1, but if placed in a chapter numbered 1,
 * it would be numbered 1.1 instead.
 *
 * To account for this, styles are organized into replacement classes. Chapter and appendix,
 * for example, are replacements of one another. When a chapter style is encountered, it
 * is as if the style hierarchy contains only chapter, with children section, figure and so on;
 * when appendix is encountered subsequently, chapter is replaced, and figure and section
 * become children of appendix. We'll call the set of styles active in the tree at a given time
 * the "context".
 *
 * The first part focuses on the replacement mechanism. It characterizes a well-formed
 * style sheet (with the fact StyleSheet), and a well-formed state (with the fact Forest). An
 * operation addStyleToContext describes how the context is altered, and includes a
 * precondition requiring that, for example, a child is not encountered before its parents
 * (a document can't start with subsection, eg). The assertion PreservesForest checks that the
 * operation preserves the well-formedness of the state; it was analyzing this that helped
 * determine an appropriate precondition and the appropriate constraints in the invariant.
 *
 * The second part adds the numbering of styles. Note the idiom of declaring a subsignature
 * and then equating it to the supersignature, thus essentially retrofitting the new fields to the
 * old signature. A second operation describes how numbers are assigned; the conjunction of the
 * two operations is what happens when a style is encountered. The assertion AddNeverReduces
 * checks that when a style is encountered the number associated with each style in the context is
 * not decreased. The first assertion is valid; the second isn't.
 *
 * author: Daniel Jackson, 11/15/01
 */


sig Style {
    replaces, parents: set Style
}


pred StyleSheet {
    // equivalence[replaces, Style]
    // acyclic[parents, Style]
    all x: Style {
        // equivalence
        x.replaces in x.replaces.replaces
        all y: Style | y in x.replaces => y.replaces in x.replaces
        all y, z : Style | (x in y.replaces and y in z.replaces) => x in z.replaces
        // acyclic
        x not in x.^parents
        // rest
        x.replaces.parents.replaces = x.parents
        all y,z: x.parents | y in z.replaces
        }
    }

sig State {
    context: set Style,
    ancestors: pfunc Style -> Style
    }

pred DefineAncestors {
    all s: State, x: Style | s.ancestors [x] = x.*parents & s.context
    }

pred Forest [s: State] {
    all x: s.context |
        some root: s.ancestors[x] {
            no root.parents
            all y: s.ancestors[x] - root | one y.parents & s.context
            }
    all x: Style | lone x.replaces & s.context
    }

pred AddStyleToContext [s, s1: State, style: Style] {
    all x: style.^parents | some x.replaces & s.context
    s1.context = s.context - style.replaces + style
    }


sig Value {next: one Value}
sig NumberedStyle extends Style {
    initial: one Value
    }
sig NumberedState extends State {
    value: pfunc Style -> Value
    }
pred fact1 {Style = NumberedStyle}
pred fact2 {State = NumberedState}
pred fact3 { all x : NumberedState | all y : Style | one y.(x.value) }

pred AddStyleToNumbering [s, s1: State, style: Style] {
    s1.value[style] = (style in s.context => (s.value[style]).next else style.initial)
    s1.context = s.context - style.replaces + style
    all x: Style - style |
        s1.value[x] = (style in x.^parents => x.initial else s.value[x])
    }

pred AddStyle [s, s1: State, style: Style] {
    AddStyleToContext [s,s1,style]
    AddStyleToNumbering [s,s1,style]
    }

pred model_facts {
    StyleSheet and DefineAncestors and fact1 and fact2
}

pred AddNeverReduces {
    all s,s1: State, z: Style |
        Forest[s] && AddStyle [s,s1,z] =>
            (all y: s1.context | s1.value[y] in (s.value[y]).*next)
    }
test expect {
 number_1 : {model_facts => AddNeverReduces} for 5 is sat
}

