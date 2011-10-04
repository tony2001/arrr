dnl $Id$

PHP_ARG_WITH(arrr, for R support,
[  --with-arrr=[DIR]             Include R support])

if test "$PHP_ARRR" != "no"; then
  SEARCH_PATH="/usr /usr/local /opt"
  SEARCH_FOR="/include/Rembedded.h"

  if test -d "$PHP_ARRR"; then
    AC_MSG_CHECKING([for R headers in $PHP_ARRR])
    if test -r "$PHP_ARRR/$SEARCH_FOR"; then 
      ARRR_DIR=$PHP_ARRR
      AC_MSG_RESULT([found])
    fi
  else
    AC_MSG_CHECKING([for R headers in default path])
    for i in $SEARCH_PATH ; do
      if test -r $i/$SEARCH_FOR; then
        ARRR_DIR=$i
        AC_MSG_RESULT(found in $i)
      fi
    done
  fi

  if test -z "$ARRR_DIR"; then
    AC_MSG_RESULT([not found])
    AC_MSG_ERROR([Unable to find R headers])
  fi

  LIBNAME=R
  LIBSYMBOL=Rf_initEmbeddedR

  PHP_CHECK_LIBRARY($LIBNAME,$LIBSYMBOL,
  [
    PHP_ADD_LIBRARY_WITH_PATH($LIBNAME, $ARRR_DIR/lib, ARRR_SHARED_LIBADD)
    AC_DEFINE(HAVE_ARRRLIB,1,[ ])
  ],[
    AC_MSG_ERROR([wrong arrr lib version or lib not found])
  ],[
    -L$ARRR_DIR/lib
  ])
 
  PHP_ADD_LIBRARY(Rblas)
  PHP_ADD_INCLUDE($ARRR_DIR/include)
  PHP_SUBST(ARRR_SHARED_LIBADD)

  PHP_NEW_EXTENSION(arrr, arrr.c, $ext_shared)
fi
