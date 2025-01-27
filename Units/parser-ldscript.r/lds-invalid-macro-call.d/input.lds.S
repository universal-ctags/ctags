#define S(A) A: AT(ADDR(.altinstr_aux) - LOAD_OFFSET) { \
             *(A) \
}

SECTIONS
{
	_etext = .;
	S(.altinstr_aux
}
