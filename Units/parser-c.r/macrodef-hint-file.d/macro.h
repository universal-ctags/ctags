/* u-ctags '--fields=+{language}{signature}' '--fields-C++=+{macrodef}' -o hint.tags macro.h */
#define DEF(fn, rtype, signature, body)			\
	rtype fn signature BEGIN body END
