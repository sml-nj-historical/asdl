/*! \file asdl-integer.hxx
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#ifndef _ASDL_INTEGER_HXX_
#define _ASDL_INTEGER_HXX_

#if defined(ASDL_USE_GNU_MP)
#  include <gmpxx.h>
#endif

namespace asdl {

  // multiprecision integers
    class integer {
      public:
	int32_t toInt32 ();
	int64_t toInt64 ();
	uint32_t toUInt32 ();
	uint64_t toUInt64 ();

      private:
#if define(ASDL_USE_GNU_MP)
	mpz_class _rep;
#endif
    };


};

#endif //! _ASDL_INTEGER_HXX_
