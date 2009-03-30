#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.63])
AC_INIT([Erlang RS232 serial interface], [0], [hun@n-dimensional.de], [eserial])
AC_CONFIG_SRCDIR([c_src/eppp_serial.h])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_AUX_DIR([auxdir])
AM_INIT_AUTOMAKE([
   -Wall -Werror
   1.10
   foreign
])

# Checks for programs.
AC_PROG_CC
AC_ERLANG_NEED_ERLC

# Checks for libraries.
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR(AC_PACKAGE_TARNAME, AC_PACKAGE_VERSION)
AM_CONDITIONAL([EPPP_SERIAL_ONLY], [true])

# Checks for header files.
AC_CHECK_HEADERS([fcntl.h stdlib.h string.h sys/param.h termios.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.
AC_CHECK_FUNCS([select])

# Generate output
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
