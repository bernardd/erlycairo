AC_INIT(libcairo wrapper library for Erlang, SVN, 
             rsaccon@gmail.com, erlycairo)
dnl Require autoconf version >=2.59c. first one with erlang macros
AC_PREREQ(2.59c)
AC_COPYRIGHT(Copyright (C) 2007 Roberto Saccon)
dnl AC_REVISION($Id$)

dnl Require automake version >=1.9.5.
AM_INIT_AUTOMAKE(1.9.5)

dnl C-related configuration.
AC_PROG_CC
AC_STDC_HEADERS

dnl Erlang-related configuration.
AC_ERLANG_CHECK_LIB(erl_interface)
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR(AC_PACKAGE_TARNAME, AC_PACKAGE_VERSION)
erlcflags="+debug_info"
AC_SUBST([ERLCFLAGS], [$erlcflags])

dnl Information about libcairo
PKG_CHECK_MODULES(cairo, [cairo-png])

dnl Specify the files to instantiate.
AC_CONFIG_FILES([ \
	Makefile \
	src/Makefile \
	c_src/Makefile \
	src/erlycairo/Makefile \
	src/demo/Makefile \
	doc/Makefile \
	])

AC_OUTPUT
