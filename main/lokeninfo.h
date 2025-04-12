typedef uint64_t lokenType;
typedef int lokenKeyword;

typedef struct sLokenInfo {
	enum uint64_t type;
	vString *word;
	lokenKeyword keyword;
} lokenInfo;
