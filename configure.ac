dnl Process this file with autoconf to produce a configure script.
dnl
dnl This code is part of the SML/NJ system (http://smlnj.org)
dnl
dnl COPYRIGHT (c) 2018 The Fellowship of SML/NJ
dnl All rights reserved.
dnl

AC_INIT(asdl-gen,3.0,)

AC_PREREQ(2.60)
AC_COPYRIGHT([[COPYRIGHT (c) 2018 The Fellowship of SML/NJ]])
AC_CONFIG_SRCDIR(src/lib/asdl/std-types.asdl)
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_MACRO_DIR(config)

dnl
dnl include additional macros
dnl
sinclude(config/check_smlnj.m4)
sinclude(config/check_smlnj_heap_suffix.m4)
sinclude(config/ax_cxx_compile_stdcxx.m4)

dnl
dnl get host information
dnl
AC_CANONICAL_HOST

dnl
dnl define various directory paths
dnl
ASDL_ROOT=`pwd`
ASDL_MKDIR=$ASDL_ROOT/mk
ASDL_BINDIR=$ASDL_ROOT/bin
ASDL_LIBDIR=$ASDL_ROOT/lib
ASDL_INCDIR=$ASDL_ROOT/include
ASDL_SRCDIR=$ASDL_ROOT/src
ASDLGEN_SRCDIR=$ASDL_SRCDIR/asdlgen

AC_SUBST(ASDL_ROOT)
AC_SUBST(ASDL_MKDIR)
AC_SUBST(ASDL_BINDIR)
AC_SUBST(ASDL_LIBDIR)
AC_SUBST(ASDL_INCDIR)
AC_SUBST(ASDL_SRCDIR)
AC_SUBST(ASDLGEN_SRCDIR)

dnl check for standard programs
dnl
AC_PROG_CPP
AC_PROG_CC([clang gcc cc])
AC_PROG_CXX([clang++ g++ c++])
AX_CXX_COMPILE_STDCXX(11,[],[mandatory])
AC_PROG_INSTALL
AC_PROG_MAKE_SET

## TODO --enable options for other target languages: Java, Haskell, OCaml, ...
## also check for language implementations

#################### SML configuration ####################

dnl
dnl We support building with both SML/NJ (the default) or MLton.
dnl

dnl first check for --with-mlton=path flag
dnl
AC_ARG_WITH([mlton],
  [AS_HELP_STRING([--with-mlton=<path-to-mlton>],
    [use the MLton Standard ML compiler to build Diderot])],
  [with_mlton=yes],[with_mlton=no])
if test x"$with_mlton" = xyes ; then
  if test x"$ac_cv_path_with_mlton" = x ; then
    # look for mlton in the PATH
    AC_PATH_PROGS(with_mlton, mlton, no)
    if test x"$MLTON" = xno ; then
      AC_MSG_ERROR([unable to find mlton executable; please specify the path])
    fi
  else
    with_mlton=$ac_cv_path_with_mlton
    case $with_mlton in
      /*) ;;
      *) AC_MSG_ERROR([please specify absolute path for mlton executable]) ;;
    esac
    if test ! -x $with_mlton ; then
      AC_MSG_ERROR([invalid path for mlton; $with_mlton is not executable])
    fi
  fi
fi

if test x"$with_mlton" = xno ; then
  # we are using SML/NJ
  #

  dnl check that we have a compatible version of SML/NJ
  dnl
  CHECK_SMLNJ([AC_MSG_ERROR([unable to find SML/NJ installation; check your PATH or set SMLNJ_CMD])])
  if test $SMLNJ_MAJOR_VERSION -lt 110 \
    -o $SMLNJ_MINOR_VERSION -lt 83
  then
    AC_MSG_ERROR([installation requires SML/NJ version 110.83+ (available from smlnj.org)])
  fi

  dnl determine the heap-image suffix
  dnl
  CHECK_SMLNJ_HEAP_SUFFIX([
    AC_MSG_ERROR([unsupported configuration ${host_cpu}-${host_os}])])

  dnl look for ml-makedepend
  dnl
  tmpPATH="$SMLNJ_PATH:$PATH"
  AC_PATH_PROG(ML_MAKEDEPEND, ml-makedepend, ":", $tmpPATH)
  AC_SUBST(ML_MAKEDEPEND)

  dnl
  dnl look for ml-build
  dnl
  tmpPATH="$SMLNJ_PATH:$PATH"
  AC_PATH_PROG(ML_BUILD, ml-build, none, $tmpPATH)
  if test $ML_BUILD = none ; then
    AC_MSG_ERROR([ml-build not found])
  fi
  AC_SUBST(ML_BUILD)
  SML=smlnj

  HEAP_IMAGE_DIR=$DIDEROT_BINDIR/.heap
  INSTALL_SMLNJ_WRAPPER=$DIDEROT_BINDIR/install-sml-wrapper.sh
  AC_SUBST(HEAP_IMAGE_DIR)
  AC_SUBST(INSTALL_SMLNJ_WRAPPER)

else

  # we are using MLton
  MLTON=$with_mlton
  AC_SUBST(MLTON)
  SML=mlton

fi
AC_SUBST(SML)

#################### end SML configuration ####################

#################### Multiprecision integer configuration ####################

# hack for now
#
AC_DEFINE(
  [ASDL_USE_GNU_MP], 1,
  [Define to 1 to use the GNU Multiprecision Library.])
CPPFLAGS="$CPPFLAGS -I/opt/local/include"

#################### end Multiprecision integer configuration ####################

#################### Library configuration ####################

dnl FIXME: this works for now, but we should probably switch to libtool
dnl
AC_PATH_PROG(LD, [ld], [none])
if test x$LD = xnone ; then
  AC_MSG_ERROR([cannot find ld command])
fi
AC_SUBST(LD)

dnl
dnl OS-specific linking issues
dnl
CXXFLAG_fPIC=""
case "$host_os" in
  darwin*)
    DSLEXT="dylib"
    LD_STATIC=$LD
    LD_DYNAMIC="$CXX -dynamiclib"
  ;;
  linux*)
  # On Linux we include the -rpath option to pick up dynamically-loaded libraries
  # and the -fPIC compiler flag to generate position-indenpendent code
    CXXFLAG_fPIC="-fPIC"
    DSLEXT="so"
    LD_STATIC=$LD
    LD_DYNAMIC="$CXX -shared"
  ;;
esac

AC_SUBST(LD_STATIC)
AC_SUBST(LD_DYNAMIC)
AC_SUBST(CXXFLAG_fPIC)
AC_SUBST(DSLEXT)

#################### end Library configuration ####################

#################### config.h ####################

AH_TOP([
/*
 * This code is part of the SML/NJ System (http://smlnj.org)
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ.
 */

#ifndef _ASDL_CONFIG_H_
#define _ASDL_CONFIG_H_

])
AH_BOTTOM([

#endif /* !_ASDL_CONFIG_H_ */
])

#################### end config.h ####################

#################### write output ####################

dnl The compiler Makefile depends on which SML system we are
dnl using.
if test x"$SML" = xmlton ; then
  SRC_MAKEFILE_IN=":src/asdlgen/Makefile_mlton.in"
  FRAGMENTS_MAKEFILE="src/gen/fragments/Makefile"
else
  SRC_MAKEFILE_IN=""
  FRAGMENTS_MAKEFILE=""
fi

AC_CONFIG_FILES(
dnl
dnl ***** Makefiles *****
  Makefile
  doc/Makefile
  doc/manual-2.0/Makefile
  doc/manual-3.0/Makefile
  examples/Makefile
  src/Makefile
  src/asdlgen/Makefile$SRC_MAKEFILE_IN
  $FRAGMENTS_MAKEFILE
  src/lib/Makefile
  src/lib/cxx/Makefile
  src/tests/Makefile
  src/tests/basics/Makefile
dnl
dnl
  src/asdlgen/common/config.sml:src/asdlgen/common/config_sml.in
)

AC_CONFIG_HEADERS(src/lib/cxx/include/asdl/config.h:config/config_h.in)

dnl
dnl shell scripts
dnl
AC_CONFIG_FILES([
  bin/install-sml-wrapper.sh:config/install-sml-wrapper_sh.in
], [chmod +x bin/install-sml-wrapper.sh])
AC_CONFIG_FILES([
  src/gen/fragments/mkfrags.sh:src/gen/fragments/mkfrags_sh.in
], [chmod +x src/gen/fragments/mkfrags.sh])
AC_CONFIG_FILES([
  src/gen/fragments/mkmk.sh:src/gen/fragments/mkmk_sh.in
], [chmod +x src/gen/fragments/mkmk.sh])

AC_OUTPUT

#################### end write output ####################
