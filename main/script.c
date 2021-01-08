/*
*   Copyright (c) 2020, Masatake YAMATO
*   Copyright (c) 2020, Red Hat, Inc.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains ctags specific optscript objects
*/

#include "general.h"  /* must always come first */

#include "debug.h"
#include "entry.h"
#include "field_p.h"
#include "htable.h"			/* For HT_PTR_TO_INT */
#include "optscript.h"
#include "routines.h"

#include <string.h>

EsObject *OPTSCRIPT_ERR_NOTAGENTRY;

extern OptVM *optscriptInit (void)
{
	opt_init ();
	MIO *in  = mio_new_fp (stdin, NULL);
	MIO *out = mio_new_fp (stdout, NULL);
	MIO *err = mio_new_fp (stderr, NULL);

	OptVM *optvm = opt_vm_new (in, out, err);

	mio_unref (err);
	mio_unref (out);
	mio_unref (in);

	OPTSCRIPT_ERR_NOTAGENTRY = es_error_intern ("notagentry");

	return optvm;
}

static EsObject* lrop_get_field_value (OptVM *vm, EsObject *name)
{
	EsObject *nobj = opt_vm_ostack_top (vm);
	if (!es_integer_p (nobj))
		return OPT_ERR_TYPECHECK;

	int n = es_integer_get (nobj);
	tagEntryInfo *e = getEntryInCorkQueue (n);
	if (e == NULL)
		return OPTSCRIPT_ERR_NOTAGENTRY;;

	void * data = es_symbol_get_data (name);
	fieldType ftype = HT_PTR_TO_INT (data);
	EsObject *val = getFieldValue (ftype, e);
	if (es_error_p (val))
		return val;

	opt_vm_ostack_pop (vm);

	if (isFieldValueAvailableAlways (ftype))
	{
		opt_vm_ostack_push (vm, val);
		es_object_unref (val);
	}
	else if (es_null (val))
	{
		opt_vm_ostack_push (vm, es_false);
	}
	else
	{
		opt_vm_ostack_push (vm, val);
		opt_vm_ostack_push (vm, es_true);
		es_object_unref (val);
	}
	return es_false;
}

static EsObject* lrop_set_field_value (OptVM *vm, EsObject *name)
{
	EsObject *indexobj = opt_vm_ostack_peek (vm, 1);
	if (!es_integer_p (indexobj))
		return OPT_ERR_TYPECHECK;

	int n = es_integer_get (indexobj);
	tagEntryInfo *e = getEntryInCorkQueue (n);
	if (e == NULL)
		return OPTSCRIPT_ERR_NOTAGENTRY;;

	void * data = es_symbol_get_data (name);
	fieldType ftype = HT_PTR_TO_INT (data);
	unsigned int fdata_type = getFieldDataType (ftype);

	EsObject *valobj = opt_vm_ostack_top (vm);
	int valtype = es_object_get_type (valobj);

	if (hasFieldValueCheckerForSetter (ftype))
	{
		EsObject *e = checkFieldValueForSetter (ftype, valobj);
		if (!es_object_equal (e, es_false))
			return e;
	}
	else
	{
		if (! (((fdata_type & FIELDTYPE_STRING) && (valtype == OPT_TYPE_STRING))
			   || ((fdata_type & FIELDTYPE_BOOL) && (valtype == ES_TYPE_BOOLEAN))
			   || ((fdata_type & FIELDTYPE_INTEGER) && (valtype == ES_TYPE_INTEGER))))
			return OPT_ERR_TYPECHECK;
	}

	EsObject *r = setFieldValue (ftype, e, valobj);
	if (es_error_p (r))
		return r;

	opt_vm_ostack_pop (vm);
	opt_vm_ostack_pop (vm);

	return es_false;
}

static void optscriptInstallFieldGetter (EsObject *dict, fieldType ftype,
										 vString *op_name, vString *op_desc)
{
	const char *fname = getFieldName (ftype);
	vStringPut (op_name, ':');
	vStringCatS (op_name, fname);
	EsObject *op_sym = es_symbol_intern (vStringValue (op_name));
	es_symbol_set_data (op_sym, HT_INT_TO_PTR (ftype));

	const char *vtype = getFieldGetterValueType (ftype);
	unsigned int fdata_type = getFieldDataType (ftype);

	vStringCatS (op_desc, "int :");
	vStringCatS (op_desc, fname);
	vStringPut (op_desc, ' ');

	if (vtype)
		vStringCatS (op_desc, vtype);
	else
	{
		Assert (fdata_type);
		if (fdata_type & FIELDTYPE_STRING)
			vStringCatS (op_desc, "string|");
		if (fdata_type & FIELDTYPE_INTEGER)
			vStringCatS (op_desc, "int|");
		if (fdata_type & FIELDTYPE_BOOL)
			vStringCatS (op_desc, "bool|");
		vStringChop (op_desc);
	}

	if (!isFieldValueAvailableAlways (ftype))
	{
		vStringPut (op_desc, ' ');
		vStringCatS (op_desc, "true%");
		vStringCatS (op_desc, "int :");
		vStringCatS (op_desc, fname);
		vStringCatS (op_desc, " false");
	}

	EsObject *op = opt_operator_new (lrop_get_field_value,
									 vStringValue (op_name),
									 1, vStringValue (op_desc));
	opt_dict_def (dict, op_sym, op);
	es_object_unref (op);
}

static void optscriptInstallFieldSetter (EsObject *dict, fieldType ftype,
										 vString *op_name, vString *op_desc)
{
	const char *fname = getFieldName (ftype);
	vStringCatS (op_name, fname);
	vStringPut (op_name, ':');

	EsObject *op_sym = es_symbol_intern (vStringValue (op_name));
	es_symbol_set_data (op_sym, HT_INT_TO_PTR (ftype));

	const char *vtype = getFieldSetterValueType (ftype);
	unsigned int fdata_type = getFieldDataType (ftype);
	vStringCatS (op_desc, "int ");

	if (vtype)
		vStringCatS (op_desc, vtype);
	else
	{
		Assert (fdata_type);
		if (fdata_type & FIELDTYPE_STRING)
			vStringCatS (op_desc, "string|");
		if (fdata_type & FIELDTYPE_INTEGER)
			vStringCatS (op_desc, "int|");
		if (fdata_type & FIELDTYPE_BOOL)
			vStringCatS (op_desc, "bool|");
		vStringChop (op_desc);
	}

	vStringPut (op_desc, ' ');
	vStringCatS (op_desc, fname);
	vStringCatS (op_desc, ": -");

	EsObject *op = opt_operator_new (lrop_set_field_value,
									 vStringValue (op_name),
									 2, vStringValue (op_desc));
	opt_dict_def (dict, op_sym, op);
	es_object_unref (op);
}

static void optscriptInstallFieldAccessors (EsObject *dict)
{
	vString *op_name = vStringNew ();
	vString *op_desc = vStringNew ();

	for (fieldType ftype = 0; ftype <= FIELD_BUILTIN_LAST; ftype++)
	{
		if (hasFieldGetter (ftype))
		{
			optscriptInstallFieldGetter (dict, ftype, op_name, op_desc);
			vStringClear (op_name);
			vStringClear (op_desc);
		}
		if (hasFieldSetter (ftype))
		{
			optscriptInstallFieldSetter (dict, ftype, op_name, op_desc);
			vStringClear (op_name);
			vStringClear (op_desc);
		}
	}

	vStringDelete (op_name);
	vStringDelete (op_desc);
}

extern void optscriptInstallProcs (EsObject *dict)
{
	optscriptInstallFieldAccessors (dict);
}

static EsObject *optscript_CorkIndex_sym = es_nil;
extern void optscriptSetup (OptVM *vm, EsObject *dict, int corkIndex)
{
	if (corkIndex != CORK_NIL)
	{
		static EsObject *corkIndex_sym = es_nil;
		if (es_null (corkIndex_sym))
			corkIndex_sym = es_symbol_intern (".");
		EsObject *corkIndex_val = es_integer_new (corkIndex);
		opt_dict_def (dict, corkIndex_sym, corkIndex_val);
		es_object_unref (corkIndex_val);
		optscript_CorkIndex_sym = corkIndex_sym;
	}
}

extern void optscriptTeardown (OptVM *vm, EsObject *dict)
{
	if (!es_null (optscript_CorkIndex_sym))
	{
		opt_dict_undef (dict, optscript_CorkIndex_sym);
		optscript_CorkIndex_sym = es_nil;
	}
}

extern EsObject *optscriptRead (OptVM *vm, const char *src, size_t len)
{
	if (len == 0)
		len = strlen (src);

	MIO *mio = mio_new_memory ((unsigned char *)src, len, NULL, NULL);
	EsObject *obj = opt_vm_read (vm, mio);
	if (es_error_p (obj))
		opt_vm_report_error (vm, obj, NULL);
	mio_unref (mio);
	return obj;
}

extern EsObject* optscriptEval (OptVM *vm, EsObject *code)
{
	static EsObject *exec = es_nil;

	if (es_null (exec))
	{
		MIO *mio = mio_new_memory ((unsigned char*)"//exec", 6, NULL, NULL);
		exec = opt_vm_read (vm, mio);
		if (es_error_p (exec))
		{
			opt_vm_report_error (vm, exec, NULL);
			error (FATAL, "failed in converting //exec to an optscript object");
		}
		mio_unref (mio);
	}

	EsObject *o = opt_vm_eval (vm, code);
	if (es_error_p (o))
	{
		opt_vm_report_error (vm, o, NULL);
		error (FATAL, "failed to push the proc representing the script");
	}
	es_object_unref (o);

	EsObject *r = opt_vm_eval (vm, exec);;
	if (es_error_p (r))
		opt_vm_report_error (vm, r, NULL);
	return r;
}

extern EsObject* optscriptDefine (EsObject *dict,
								  const char *name, EsObject *obj)
{
	EsObject *sym = es_symbol_intern (name);
	opt_dict_def (dict, sym, obj);
	return sym;
}


extern EsObject *optscriptReadAndEval   (OptVM *vm, const char *src, size_t len)
{
	EsObject *obj = optscriptRead (vm, src, len);
	if (es_error_p (obj))
		return obj;

	EsObject *r = optscriptEval (vm, obj);
	es_object_unref (obj);
	return r;
}

extern EsObject *optscriptReadAndDefine (OptVM *vm, EsObject *dict, const char *name,
										 const char *src, size_t len)
{
	EsObject *obj = optscriptRead (vm, src, len);
	if (es_error_p (obj))
		return obj;
	return optscriptDefine (dict, name, obj);
}