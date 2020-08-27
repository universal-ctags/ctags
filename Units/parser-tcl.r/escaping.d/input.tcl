# Taken from a comment in #2627 submitted by @surmish.
proc proc1 {} {
  expr {[string first "\\" $varName]==0}
}

proc proc2  {} {}
