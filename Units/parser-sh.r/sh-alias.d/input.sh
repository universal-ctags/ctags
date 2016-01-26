alias alias1=bar; afteralias() {}

function alias(); alias alias2=foo

func1() {
  alias alias_in_func1=ls
}
func2() alias alias_in_func2=ls
func3() { alias alias_in_func3=ls }

alias alias-with-dash=alias1
