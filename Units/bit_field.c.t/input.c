struct bit_fields {
    unsigned int a: 1;
    unsigned int b: 1;
    unsigned int c: 2;
};

struct {
    unsigned sign  : 1;
    unsigned exp   : _FP_EXPBITS_D;
    unsigned frac1 : _FP_FRACBITS_D - (_FP_IMPLBIT_D != 0) - _FP_W_TYPE_SIZE;
    unsigned frac0 : _FP_W_TYPE_SIZE;
};

struct shortname_info {
	unsigned char lower:1,
		      upper:1,
		      valid:1;
};

// Problem reported by Michael Brown on 23 October 2001.
typedef struct
{
    BYTE 	public: 1;
    BYTE 	bad2: 1;
    BYTE 	group: 1;
    BYTE 	personal: 1;
} bitfield_flags;

typedef struct
{
    BYTE	this;
    BYTE	public;
    BYTE	private;
    BYTE	that;
} mystruct;
