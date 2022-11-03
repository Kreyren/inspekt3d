dnl -*- Autoconf -*-

dnl configure.ac
dnl
dnl Copyright 2018 Kavalogic, Inc.
dnl
dnl This file is part of Inspekt3d.
dnl
dnl Inspekt3d is free software: you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as
dnl published by the Free Software Foundation, either version 3 of the
dnl License, or (at your option) any later version.
dnl
dnl Inspekt3d is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl Lesser General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public
dnl License along with this program.  If not, see
dnl <http://www.gnu.org/licenses/>.

dnl *** Process this file with autoconf to produce a configure script. ***

AC_INIT([Inspekt3d], [0.0.0],, [Inspekt3d], [https://www.github.com/sjm-tl-gh/inspekt3d])
dnl AC_CONFIG_SRCDIR is a file found in the source tree as a check
AC_CONFIG_SRCDIR([inspekt3d])
AM_INIT_AUTOMAKE

AC_COPYRIGHT([Copyright 2018 Kavalogic, Inc.  See the file COPYING for licensing and distribution terms.])
AH_TOP([/* Copyright 2018 Kavalogic, Inc.  See the file COPYING for licensing and distribution terms. */])

GUILE_PKG([3.0 2.2])
GUILE_PROGS
if test "x$GUILD" = "x"; then
   dnl Do an alternate check for 'guild'
   AC_CHECK_PROG([GUILD], guild, guild)
   if test "x$GUILD" = "x"; then
      AC_MSG_ERROR(['guild' binary not found; please check your guile-3.0 2.2 installation.])
   fi
fi

PKG_CHECK_MODULES([LIBFIVE], [libfive],
  [PKG_CHECK_VAR([LIBFIVE_GUILE_LIBDIR], [libfive], [libdir],
    [],
    [AC_MSG_ERROR([Could not determine libdir for libfive.])])],
  [AC_MSG_ERROR([Guile bindings for libfive are missing; please install them.])])

GUILE_MODULE_AVAILABLE([have_guile_opengl], [(gl)])
if test "x$have_guile_opengl" != "xyes"; then
  AC_MSG_ERROR([Guile OpenGL is missing; please install it.])
fi

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([compile-env], [chmod +x compile-env])
AC_OUTPUT
