/*
  +----------------------------------------------------------------------+
  | PHP Version 5                                                        |
  +----------------------------------------------------------------------+
  | Copyright (c) 1997-2008 The PHP Group                                |
  +----------------------------------------------------------------------+
  | This source file is subject to version 3.01 of the PHP license,      |
  | that is bundled with this package in the file LICENSE, and is        |
  | available through the world-wide-web at the following url:           |
  | http://www.php.net/license/3_01.txt                                  |
  | If you did not receive a copy of the PHP license and are unable to   |
  | obtain it through the world-wide-web, please send a note to          |
  | license@php.net so we can mail you a copy immediately.               |
  +----------------------------------------------------------------------+
  | Author: Antony Dovgal <tony@daylessday.org>                          |
  +----------------------------------------------------------------------+
*/

/* $Id: header,v 1.16.2.1.2.1.2.1 2008/02/07 19:39:50 iliaa Exp $ */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "php.h"
#include "php_ini.h"
#include "ext/standard/info.h"
#include "php_arrr.h"

#include <R.h>
#include <Rinternals.h>
#include <Rembedded.h>
#include <Rdefines.h>
#include <R_ext/Parse.h>
#include <Rversion.h>

#ifdef COMPILE_DL_ARRR
ZEND_GET_MODULE(arrr)
#endif

static zend_class_entry *ce_r;
static zend_object_handlers php_r_handlers;

/* since these functions are not in the headers, I have to declare them myself */
void R_SetErrorHook(void (*hook)(SEXP, char *));
void R_SetWarningHook(void (*hook)(SEXP, char *));

static SEXP php_zval_to_r(zval **value);

/* {{{ internal funcs */

static void php_r_error_handler(SEXP e, char *buf) /* {{{ */
{
	TSRMLS_FETCH();
	php_error_docref(NULL TSRMLS_CC, E_ERROR, "%s", buf);
}
/* }}} */

static void php_r_warning_handler(SEXP e, char *buf) /* {{{ */
{
	TSRMLS_FETCH();
	php_error_docref(NULL TSRMLS_CC, E_WARNING, "%s", buf);
}
/* }}} */

static int php_is_r_primitive(SEXP val, SEXPTYPE *type) /* {{{ */
{
	int is = 0;

	if (GET_LENGTH(GET_DIM(val))) {
		return 0;
	}

	if (GET_LENGTH(GET_CLASS(val))) {
		return 0;
	}

	*type = TYPEOF(val);
	switch (*type) {
		case REALSXP:
		case LGLSXP:
		case STRSXP:
		case INTSXP:
			is = 1;
		default:
			break;
	}

	return is;
}
/* }}} */

static SEXP php_hash_to_r(HashTable *ht) /* {{{ */
{
	SEXP values, keys, r_element;
	int n, i;
	HashPosition pos;
	zval **element;

	n = zend_hash_num_elements(ht);
	if (!n) {
		return 0;
	}

	PROTECT(values = NEW_LIST(n));
	PROTECT(keys = NEW_CHARACTER(n));

	i = 0;
	for (zend_hash_internal_pointer_reset_ex(ht, &pos);
			zend_hash_get_current_data_ex(ht, (void **) &element, &pos) == SUCCESS;
			zend_hash_move_forward_ex(ht, &pos)
		) {
		char *string_key;
		uint string_key_len;
		ulong num_key;

		r_element = php_zval_to_r(element);

		switch (zend_hash_get_current_key_ex(ht, &string_key, &string_key_len, &num_key, 0, &pos)) {
			case HASH_KEY_IS_STRING:
				if (string_key_len > 0) {
					SET_STRING_ELT(keys, i, COPY_TO_USER_STRING(string_key));
				}
				break;

			case HASH_KEY_IS_LONG:
				/* ignore the key */
				break;
		}
		i++;
	}
	SET_NAMES(values, keys);
	UNPROTECT(2);
	return values;
}
/* }}} */

static SEXP php_zval_to_r(zval **value) /* {{{ */
{
	SEXP result = NULL_USER_OBJECT;

	switch (Z_TYPE_PP(value)) {
		case IS_LONG:
			PROTECT(result = NEW_INTEGER(1));
			INTEGER_DATA(result)[0] = Z_LVAL_PP(value);
			UNPROTECT(1);
			break;
		case IS_DOUBLE:
			PROTECT(result = NEW_NUMERIC(1));
			NUMERIC_DATA(result)[0] = Z_DVAL_PP(value);
			UNPROTECT(1);
			break;
		case IS_STRING:
			PROTECT(result = NEW_CHARACTER(1));
			SET_STRING_ELT(result, 0, COPY_TO_USER_STRING(Z_STRVAL_PP(value)));
			UNPROTECT(1);
			break;
		case IS_BOOL:
			PROTECT(result = NEW_LOGICAL(1));
			LOGICAL_DATA(result)[0] = Z_BVAL_PP(value);
			UNPROTECT(1);
			break;
		case IS_ARRAY:
			result = php_hash_to_r(Z_ARRVAL_PP(value));
			break;
		default:
			convert_to_string_ex(value);
			PROTECT(result = NEW_CHARACTER(1));
			SET_STRING_ELT(result, 0, COPY_TO_USER_STRING(Z_STRVAL_PP(value)));
			UNPROTECT(1);
			break;
	}
	return result;
}
/* }}} */

static void php_r_to_zval(SEXP value, zval *result) /* {{{ */
{
	int value_len, i;

	zval_dtor(result);
	array_init(result);

	value_len = GET_LENGTH(value);

	if (value_len == 0) {
		return;
	}
	
	for (i = 0; i < value_len; i++) {
		switch (TYPEOF(value)) {
			case INTSXP:
				add_next_index_long(result, INTEGER_DATA(value)[i]);
				break;
			case REALSXP:
				add_next_index_double(result, NUMERIC_DATA(value)[i]);
				break;
			case LGLSXP:
				add_next_index_bool(result, LOGICAL_DATA(value)[i]);
				break;
			case STRSXP:
				add_next_index_string(result, CHAR(STRING_ELT(value, 0)), 1);
				break;
		}
	}
	return;
}
/* }}} */


/* }}} */

/* {{{ proto void R::init([array argv])
 
 */
static PHP_METHOD(R, init)
{ 
	zval *argv = NULL;
	int argc = 3;
	HashPosition pos;
	char **argv_arr;
	zval **element;
	int i;
	char *r_home;

	if (zend_parse_parameters(ZEND_NUM_ARGS() TSRMLS_CC, "|a", &argv) == FAILURE) {
		return;
	}

	r_home = getenv("R_HOME");
	if (!r_home || r_home[0] == '\0') {
		setenv("R_HOME", PHP_R_DIR, 0);
	}

	R_SetErrorHook(php_r_error_handler);
	R_SetWarningHook(php_r_warning_handler);

	if (argv) {
		argc += zend_hash_num_elements(Z_ARRVAL_P(argv));
	}
	argv_arr = safe_emalloc(argc, sizeof(char *), 0);

	argv_arr[0] = "REmbeddedPHP";
	argv_arr[1] = "--gui=none";
	argv_arr[2] = "--silent";

	if (argv) {
		i = 3;
		for (zend_hash_internal_pointer_reset_ex(Z_ARRVAL_P(argv), &pos);
				zend_hash_get_current_data_ex(Z_ARRVAL_P(argv), (void **) &element, &pos) == SUCCESS;
				zend_hash_move_forward_ex(Z_ARRVAL_P(argv), &pos)
			) {
			convert_to_string_ex(element);
			argv_arr[i] = Z_STRVAL_PP(element); /* no copy here, libR does strdup() itself */
			i++;
		}
	}
	Rf_initEmbeddedR(argc, argv_arr);
	efree(argv_arr);
}
/* }}} */

/* {{{ proto void R::end([bool fatal])
 
 */
static PHP_METHOD(R, end)
{ 
	zend_bool fatal = 0;

	if (zend_parse_parameters(ZEND_NUM_ARGS() TSRMLS_CC, "|b", &fatal) == FAILURE) {
		return;
	}
	
	Rf_endEmbeddedR(fatal ? 1 : 0);
}
/* }}} */

/* {{{ proto mixed R::parseEval(string code[, mixed &result])
 
 */
static PHP_METHOD(R, parseEval)
{ 
	char *code;
	int code_len, error_occured = 0;
	SEXP e1, e2, tmp, val_parse, val, next;
	zval *result = NULL;

	if (zend_parse_parameters(ZEND_NUM_ARGS() TSRMLS_CC, "s|z/", &code, &code_len, &result) == FAILURE) {
		return;
	}

	if (result) {
		zval_dtor(result);
		ZVAL_NULL(result);
	}

	PROTECT(e1 = allocVector(LANGSXP, 2));
	SETCAR(e1, Rf_install("parse"));
	SETCAR(CDR(e1), tmp = NEW_CHARACTER(1));
	SET_STRING_ELT(tmp, 0, COPY_TO_USER_STRING(code));

	next = CDR(e1);
	SET_TAG(next, Rf_install("text"));

	val_parse = R_tryEval(e1, R_GlobalEnv, &error_occured);
	if (error_occured) {
		UNPROTECT(1);
		RETURN_FALSE;
	}

	/* okay, the call succeeded */
	PROTECT(val_parse);

	PROTECT(e2 = allocVector(LANGSXP, 2));
	SETCAR(e2, Rf_install("eval"));
	SETCAR(CDR(e2), val_parse);

	UNPROTECT(1);

	val = R_tryEval(e2, R_GlobalEnv, &error_occured);
	if (error_occured) {
		UNPROTECT(2);
		RETURN_FALSE;
	}

	if (result) {
		php_r_to_zval(val, result);
		UNPROTECT(2);
		RETURN_TRUE;
	} else {
		php_r_to_zval(val, return_value);
		UNPROTECT(2);
	}
}
/* }}} */

/* {{{ proto mixed R::__call(string function_name, array arguments)
 
 */
static PHP_METHOD(R, __call)
{ 
	char *func;
	int func_len, error_occurred = 0, num_args;
	zval *args;
	SEXP e, fun, val, arg, next;
	HashPosition pos;
	zval **element;
	SEXPTYPE type;

	if (zend_parse_parameters(ZEND_NUM_ARGS() TSRMLS_CC, "sa", &func, &func_len, &args) == FAILURE) {
		return;
	}

	fun = Rf_install(func);
	if (!fun) {
		RETURN_FALSE;
	}

	num_args = zend_hash_num_elements(Z_ARRVAL_P(args));

	PROTECT(fun);
	PROTECT(e = allocVector(LANGSXP, num_args + 1));
	SETCAR(e, fun);

	next = CDR(e);

	for(zend_hash_internal_pointer_reset_ex(Z_ARRVAL_P(args), &pos);
		zend_hash_get_current_data_ex(Z_ARRVAL_P(args), (void **)&element, &pos) == SUCCESS;
		zend_hash_move_forward_ex(Z_ARRVAL_P(args), &pos)
		) {

		arg = php_zval_to_r(element);

		SETCAR(next, arg);
		next = CDR(next);
	}

	val = R_tryEval(e, R_GlobalEnv, &error_occurred);

	if (error_occurred) {
		UNPROTECT(2);
		RETURN_FALSE;
	}

	/* okay, the call succeeded */
	PROTECT(val);

	if (val == NULL_USER_OBJECT || GET_LENGTH(val) == 0) {
		/* ignore the return value */
	} else if (php_is_r_primitive(val, &type)) {
		int i;
		array_init(return_value);
		for (i = 0; i < GET_LENGTH(val); i++) {
			switch (type) {
				case STRSXP:
					add_next_index_string(return_value, CHAR(STRING_ELT(val, 0)), 1);
					break;
				case LGLSXP:
					add_next_index_bool(return_value, LOGICAL_DATA(val)[0] ? 1 : 0);
					break;
				case INTSXP:
					add_next_index_long(return_value, INTEGER_DATA(val)[0]);
					break;
				case REALSXP:
					add_next_index_double(return_value, NUMERIC_DATA(val)[0]);
					break;
				default:
					add_next_index_null(return_value);
					break;
			}
		}
		UNPROTECT(3);
		return;
	}

	UNPROTECT(3);
	RETURN_TRUE;
}
/* }}} */

/* {{{ proto mixed R::callWithNames(string function_name, array arguments)
 
 */
static PHP_METHOD(R, callWithNames)
{ 
	char *func;
	int func_len, error_occurred = 0, num_args;
	zval *args;
	SEXP e, fun, val, arg, next;
	HashPosition pos;
	zval **element;

	if (zend_parse_parameters(ZEND_NUM_ARGS() TSRMLS_CC, "sa", &func, &func_len, &args) == FAILURE) {
		return;
	}

	fun = Rf_install(func);
	if (!fun) {
		RETURN_FALSE;
	}

	num_args = zend_hash_num_elements(Z_ARRVAL_P(args));

	PROTECT(fun);
	PROTECT(e = allocVector(LANGSXP, num_args + 1));
	SETCAR(e, fun);

	next = CDR(e);

	for(zend_hash_internal_pointer_reset_ex(Z_ARRVAL_P(args), &pos);
		zend_hash_get_current_data_ex(Z_ARRVAL_P(args), (void **)&element, &pos) == SUCCESS;
		zend_hash_move_forward_ex(Z_ARRVAL_P(args), &pos)
		) {
	
		char *string_key;
		uint string_key_len;
		ulong num_key;

		arg = php_zval_to_r(element);

		switch (zend_hash_get_current_key_ex(Z_ARRVAL_P(args), &string_key, &string_key_len, &num_key, 0, &pos)) {
			case HASH_KEY_IS_STRING:
				if (string_key_len > 0) {
					SET_TAG(next, Rf_install(string_key));
				}
				break;

			case HASH_KEY_IS_LONG:
				/* ignore the key */
				break;
		}
		SETCAR(next, arg);
		next = CDR(next);
	}

	val = R_tryEval(e, R_GlobalEnv, &error_occurred);

	if (error_occurred) {
		UNPROTECT(2);
		RETURN_FALSE;
	}

	/* okay, the call succeeded */
	PROTECT(val);
	php_r_to_zval(val, return_value);
	UNPROTECT(3);
}
/* }}} */

/* {{{ arginfo */
ZEND_BEGIN_ARG_INFO_EX(arginfo_r_init, 0, 0, 0)
	ZEND_ARG_INFO(0, argv)
ZEND_END_ARG_INFO()

ZEND_BEGIN_ARG_INFO_EX(arginfo_r_end, 0, 0, 0)
	ZEND_ARG_INFO(0, fatal)
ZEND_END_ARG_INFO()

ZEND_BEGIN_ARG_INFO_EX(arginfo_r_parseEval, 0, 0, 1)
	ZEND_ARG_INFO(0, code)
	ZEND_ARG_INFO(1, result)
ZEND_END_ARG_INFO()

ZEND_BEGIN_ARG_INFO_EX(arginfo_r_callWithNames, 0, 0, 2)
	ZEND_ARG_INFO(0, function_name)
	ZEND_ARG_INFO(0, arguments)
ZEND_END_ARG_INFO()

ZEND_BEGIN_ARG_INFO_EX(arginfo_r___call, 0, 0, 2)
	ZEND_ARG_INFO(0, function_name)
	ZEND_ARG_INFO(0, arguments)
ZEND_END_ARG_INFO()
/* }}} */

static zend_function_entry r_methods[] = { /* {{{ */
	PHP_ME(R, init, arginfo_r_init, ZEND_ACC_PUBLIC|ZEND_ACC_STATIC)
	PHP_ME(R, end, arginfo_r_end, ZEND_ACC_PUBLIC|ZEND_ACC_STATIC)
	PHP_ME(R, parseEval, arginfo_r_parseEval, ZEND_ACC_PUBLIC)
	PHP_ME(R, __call, arginfo_r___call, ZEND_ACC_PUBLIC)
	PHP_ME(R, callWithNames, arginfo_r_callWithNames, ZEND_ACC_PUBLIC)
	{NULL, NULL, NULL}
};
/* }}} */

const zend_function_entry arrr_functions[] = {
	{NULL, NULL, NULL}
};

PHP_MINIT_FUNCTION(arrr) /* {{{ */
{
	zend_class_entry ce;

	memcpy(&php_r_handlers, zend_get_std_object_handlers(), sizeof(zend_object_handlers));
	php_r_handlers.clone_obj = NULL;
	INIT_CLASS_ENTRY(ce, "R", r_methods);
	ce_r = zend_register_internal_class(&ce TSRMLS_CC);
	return SUCCESS;
}
/* }}} */

PHP_MSHUTDOWN_FUNCTION(arrr) /* {{{ */
{
	return SUCCESS;
}
/* }}} */

PHP_RINIT_FUNCTION(arrr) /* {{{ */
{
	return SUCCESS;
}
/* }}} */

PHP_RSHUTDOWN_FUNCTION(arrr) /* {{{ */
{
	return SUCCESS;
}
/* }}} */

PHP_MINFO_FUNCTION(arrr) /* {{{ */
{
	char ver[512];

	php_info_print_table_start();
	php_info_print_table_header(2, "R support", "enabled");
	slprintf(ver, sizeof(ver), "%s.%s (SVN rev %s)", R_MAJOR, R_MINOR, R_SVN_REVISION);
	php_info_print_table_row(2, "R version", ver);
	php_info_print_table_row(2, "Compile-time R_HOME", PHP_R_DIR);
	php_info_print_table_end();

}
/* }}} */

zend_module_entry arrr_module_entry = {
#if ZEND_MODULE_API_NO >= 20010901
	STANDARD_MODULE_HEADER,
#endif
	"arrr",
	arrr_functions,
	PHP_MINIT(arrr),
	PHP_MSHUTDOWN(arrr),
	PHP_RINIT(arrr),	
	PHP_RSHUTDOWN(arrr),
	PHP_MINFO(arrr),
#if ZEND_MODULE_API_NO >= 20010901
	"0.1",
#endif
	STANDARD_MODULE_PROPERTIES
};


/*
 * Local variables:
 * tab-width: 4
 * c-basic-offset: 4
 * End:
 * vim600: sw=4 ts=4 fdm=marker
 * vim<600: sw=4 ts=4
 */

