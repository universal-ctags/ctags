package comments

/* Outer /* nested */ comment */
after_nested :: proc() {}

// Line comment
after_line :: proc() {}

/* Multi
   line
   comment */
after_multi :: proc() {}

/* Outer /* mid /* deep */ mid */ outer */
after_deep :: proc() {}

/* has a / slash and a * star inside */
after_slash :: proc() {}

/* /* nested
   with newline and a * star and a / slash
*/ done */
after_complex :: proc() {}
