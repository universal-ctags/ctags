# Taken from https://github.com/universal-ctags/ctags/issues/3638 submitted by @kenifanying
proc test1 {a} {
	set i 1
	set PORT1 Gi0/1
	set PORT2 Te1/1
	puts "[format "%10s %10s %10s" "$i |" $PORT1 $PORT2]"
}
proc test2 {} {
	puts "test2"
}
