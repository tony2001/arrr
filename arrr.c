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

#ifdef COMPILE_DL_ARRR
ZEND_GET_MODULE(arrr)
#endif

static zend_class_entry *ce_r;
static zend_object_handlers php_r_handlers;

void R_SetErrorHook(void (*hook)(SEXP, char *));
void R_SetWarningHook(void (*hook)(SEXP, char *));

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

/* }}} */

/* {{{ proto void R::init(array argv)
 
 */
static PHP_METHOD(R, init)
{ 
	zval *argv;
	int argc = 3;
	HashPosition pos;
	char **argv_arr;
	zval **element;
	int i;

	if (zend_parse_parameters(ZEND_NUM_ARGS() TSRMLS_CC, "a", &argv) == FAILURE) {
		return;
	}

	R_SetErrorHook(php_r_error_handler);
	R_SetWarningHook(php_r_warning_handler);

	argc += zend_hash_num_elements(Z_ARRVAL_P(argv));
	argv_arr = safe_emalloc(argc, sizeof(char *), 0);

	argv_arr[0] = "REmbeddedPHP";
	argv_arr[1] = "--gui=none";
	argv_arr[2] = "--silent";

	i = 3;
	for (zend_hash_internal_pointer_reset_ex(Z_ARRVAL_P(argv), &pos);
			zend_hash_get_current_data_ex(Z_ARRVAL_P(argv), (void **) &element, &pos) == SUCCESS;
			zend_hash_move_forward_ex(Z_ARRVAL_P(argv), &pos)
		) {
		convert_to_string_ex(element);
		argv_arr[i] = Z_STRVAL_PP(element); /* no copy here, libR does strdup() itself */
		i++;
	}
	Rf_initEmbeddedR(argc, argv_arr);
	efree(argv_arr);
}
/* }}} */

/* {{{ proto void R::end(bool fatal)
 
 */
static PHP_METHOD(R, end)
{ 
	zend_bool fatal;

	if (zend_parse_parameters(ZEND_NUM_ARGS() TSRMLS_CC, "b", &fatal) == FAILURE) {
		return;
	}
	
	Rf_endEmbeddedR(fatal ? 1 : 0);
}
/* }}} */

/* {{{ proto bool R::tryEval(string code[, mixed &result])
 
 */
static PHP_METHOD(R, tryEval)
{ 
	char *code;
	int code_len, error_occured = 0;
	SEXP expression, tmp;
	ParseStatus status;

	if (zend_parse_parameters(ZEND_NUM_ARGS() TSRMLS_CC, "s", &code, &code_len) == FAILURE) {
		return;
	}
	
	PROTECT(tmp = mkString(code));
	PROTECT(expression = R_ParseVector(tmp, 1, &status, R_NilValue));
	R_tryEval(VECTOR_ELT(expression, 0), R_GlobalEnv, &error_occured);
	UNPROTECT(2);
	if (error_occured) {
		RETURN_FALSE;
	}
	RETURN_TRUE;
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
	
		arg = NULL_USER_OBJECT;

		switch(Z_TYPE_PP(element)) {
			case IS_LONG:
				PROTECT(arg = NEW_INTEGER(1));
				INTEGER_DATA(arg)[0] = Z_LVAL_PP(element);
				UNPROTECT(1);
				break;
			case IS_STRING:
				PROTECT(arg = NEW_CHARACTER(1));
				SET_STRING_ELT(arg, 0, COPY_TO_USER_STRING(Z_STRVAL_PP(element)));
				UNPROTECT(1);
				break;
			case IS_DOUBLE:
				PROTECT(arg = NEW_NUMERIC(1));
				NUMERIC_DATA(arg)[0] = Z_DVAL_PP(element);
				UNPROTECT(1);
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
		return;
	}

	UNPROTECT(2);
	RETURN_TRUE;
}
/* }}} */

ZEND_BEGIN_ARG_INFO_EX(arginfo_r___call, 0, 0, 2)
	ZEND_ARG_INFO(0, function_name)
	ZEND_ARG_INFO(0, arguments)
ZEND_END_ARG_INFO()

static zend_function_entry r_methods[] = { /* {{{ */
	PHP_ME(R, init, NULL, ZEND_ACC_PUBLIC|ZEND_ACC_STATIC)
	PHP_ME(R, end, NULL, ZEND_ACC_PUBLIC|ZEND_ACC_STATIC)
	PHP_ME(R, tryEval, NULL, ZEND_ACC_PUBLIC|ZEND_ACC_STATIC)
	PHP_ME(R, __call, arginfo_r___call, ZEND_ACC_PUBLIC)
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
	php_info_print_table_start();
	php_info_print_table_header(2, "arrr support", "enabled");
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

