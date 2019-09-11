AC_INIT([librepoll], [0.0.0])
AC_CONFIG_SRCDIR([.])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

PKG_CHECK_MODULES([GUILE], [guile-2.2])
GUILE_PKG([2.2])
GUILE_PROGS
if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your guile-2.x installation.])
fi

dnl TODO guile-json
dnl TODO guile-sqlite3
dnl TODO guile-commonmark

guilemoduledir="${datarootdir}/guile/site/$GUILE_EFFECTIVE_VERSION"
guileobjectdir="${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"

AC_SUBST([guilemoduledir])
AC_SUBST([guileobjectdir])
AC_SUBST([guile])

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([run])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

AC_OUTPUT
