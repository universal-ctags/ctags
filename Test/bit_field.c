// Problem reported by Michael Brown on 23 October 2001.
struct bit_fields {
    unsigned int a: 1;
    unsigned int b: 1;
    unsigned int c: 2;
};

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
