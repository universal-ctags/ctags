# Taken from #2627 submitted by @surmish
proc proc1 {} {
  if {[regexp {[0-9]$} ""]} {
    echo "matched"
  }
}

proc proc2 {} {}

proc proc3 {} {
    set abc "hello"
    puts ${abc}
}

proc proc4 {} {}

proc3
