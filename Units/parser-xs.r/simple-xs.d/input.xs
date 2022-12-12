/* Derrive from perl5/ext/SDBM_File/SDBM_File.xs */
#define PERL_NO_GET_CONTEXT
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "sdbm.h"

#define fetch_key 0
#define store_key 1
#define fetch_value 2
#define store_value 3

typedef struct {
	DBM *	dbp ;
	SV *    filter[4];
	int     filtering ;
	} SDBM_File_type;

typedef SDBM_File_type * SDBM_File ;
typedef datum datum_key ;
typedef datum datum_value ;

#define sdbm_FETCH(db,key)			sdbm_fetch(db->dbp,key)
#define sdbm_STORE(db,key,value,flags)		sdbm_store(db->dbp,key,value,flags)
#define sdbm_DELETE(db,key)			sdbm_delete(db->dbp,key)
#define sdbm_EXISTS(db,key)			sdbm_exists(db->dbp,key)
#define sdbm_FIRSTKEY(db)			sdbm_firstkey(db->dbp)
#define sdbm_NEXTKEY(db,key)			sdbm_nextkey(db->dbp)


MODULE = SDBM_File	PACKAGE = SDBM_File	PREFIX = sdbm_

PROTOTYPES: DISABLE

SDBM_File
sdbm_TIEHASH(dbtype, filename, flags, mode, pagname=NULL)
	char *		dbtype
	char *		filename
	int		flags
	int		mode
	char *		pagname
	CODE:
	{
	    DBM *	dbp ;

	    RETVAL = NULL ;
	    if (pagname == NULL) {
		dbp = sdbm_open(filename, flags, mode);
	    }
	    else {
		dbp = sdbm_prep(filename, pagname, flags, mode);
	    }
	    if (dbp) {
		RETVAL = (SDBM_File)safecalloc(1, sizeof(SDBM_File_type));
		RETVAL->dbp = dbp ;
	    }

	}
	OUTPUT:
	  RETVAL

void
sdbm_DESTROY(db)
	SDBM_File	db
	CODE:
	if (db) {
	    int i = store_value;
	    sdbm_close(db->dbp);
	    do {
		if (db->filter[i])
		    SvREFCNT_dec_NN(db->filter[i]);
	    } while (i-- > 0);
	    safefree(db) ;
	}

datum_value
sdbm_FETCH(db, key)
	SDBM_File	db
	datum_key	key

int
sdbm_STORE(db, key, value, flags = DBM_REPLACE)
	SDBM_File	db
	datum_key	key
	datum_value	value
	int		flags
    CLEANUP:
	if (RETVAL) {
	    if (RETVAL < 0 && errno == EPERM)
		croak("No write permission to sdbm file");
	    croak("sdbm store returned %d, errno %d, key \"%s\"",
			RETVAL,errno,key.dptr);
	    sdbm_clearerr(db->dbp);
	}

int
sdbm_DELETE(db, key)
	SDBM_File	db
	datum_key	key

int
sdbm_EXISTS(db,key)
	SDBM_File	db
	datum_key	key

datum_key
sdbm_FIRSTKEY(db)
	SDBM_File	db

datum_key
sdbm_NEXTKEY(db, key)
	SDBM_File	db

int
sdbm_error(db)
	SDBM_File	db
	ALIAS:
	sdbm_clearerr = 1
	CODE:
	RETVAL = ix ? sdbm_clearerr(db->dbp) : sdbm_error(db->dbp);
	OUTPUT:
	  RETVAL

SV                                                          *
filter_fetch_key(db, code)
	SDBM_File	db
	SV *		code
	SV *		RETVAL = &PL_sv_undef ;
	ALIAS:
	SDBM_File::filter_fetch_key = fetch_key
	SDBM_File::filter_store_key = store_key
	SDBM_File::filter_fetch_value = fetch_value
	SDBM_File::filter_store_value = store_value
	CODE:
	    DBM_setFilter(db->filter[ix], code);

BOOT:
	{
	    HV *stash = gv_stashpvs("SDBM_File", 1);
	    newCONSTSUB(stash, "PAGFEXT", newSVpvs(PAGFEXT));
	    newCONSTSUB(stash, "DIRFEXT", newSVpvs(DIRFEXT));
	    newCONSTSUB(stash, "PAIRMAX", newSVuv(PAIRMAX));
	}

MODULE = SDBM_File	PACKAGE = SDBM_X	PREFIX = sdbm_X_

INCLUDE: constants.xs

int
sdbm_X_DELETE0(db, key)
	SDBM_File	db
	datum_key	key

#define X "X"

int
sdbm_X_DELETE1(db, key)
	SDBM_File	db
	datum_key	key

#define Y "Y"

double
sin0()

double
sin1();
