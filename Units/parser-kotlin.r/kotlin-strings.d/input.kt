var used1 = "var unused1 = 1"
var used2 = "\"var\" unused2 = 2"
var used3 = ""
var used4 = "${process("var unused4 = 4")}"
// quote in character literal must not break parser
var used5 = '\"'
