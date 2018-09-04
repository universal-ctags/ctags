type X struct {
	T1
	*T2
	P.T3
	*P.T4
	age T5
	skill *T6
	address, hometown T7
	natualLang,programmingLang *T8
	person P.T9
	parent *P.T10
	lang1, lang2 P.LANG
	lang3, lang4 *P.LANG
	P.Q
	*P.Q
}

type Y struct {
	byte
	b byte
	ss []string
	bss [][]byte
	f func() error
	ifaces []interface{}
	tags  map[string]struct{}
	notify chan<- struct{}
	p *struct {
		x int
		y int
		z int
	}
}
