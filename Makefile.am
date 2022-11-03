# Makefile.am
#
# Copyright 2018 Kavalogic, Inc.
#
# This file is part of Inspekt3d.
#
# Inspekt3d is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# Inspekt3d is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU General Public
# License along with this program.  If not, see
# <http://www.gnu.org/licenses/>.

# specify not GNU
AUTOMAKE_OPTIONS = foreign

moddir=$(prefix)/share/guile/site/@GUILE_EFFECTIVE_VERSION@
godir=$(libdir)/guile/@GUILE_EFFECTIVE_VERSION@/ccache

GOBJECTS = $(SOURCES:%.scm=%.go)

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_go_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	./compile-env $(GUILD) compile $(GUILE_WARNINGS) -o "$@" "$<"

SOURCES =                  \
  inspekt3d/camera.scm     \
  inspekt3d/gl-missing.scm \
  inspekt3d/library.scm    \
  inspekt3d/shader.scm     \
  inspekt3d/viewer.scm     \
${NULL}

CLEANFILES = $(GOBJECTS)

EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)
EXTRA_DIST += COPYING INSTALL README.md examples compile-env.in
