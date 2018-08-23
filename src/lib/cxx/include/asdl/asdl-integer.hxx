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

#ifndef _ASDL_HXX_
#  error do not include "asdl-integer.hxx" directly; instead include "asdl.hxx"
#endif

#if defined(ASDL_USE_GNU_MP)
#  include <gmp.h>
#endif

namespace asdl {

  // multiprecision integers
    class integer {
      public:
	integer () { mpz_init(this->_rep); }
	integer (int32_t a);
	integer (uint32_t a);
	integer (int64_t a);
	integer (uint64_t a);

	~integer () { mpz_clear(this->_rep); }

	int toInt ();
	unsigned int toUInt ();
	int32_t toInt32 ();
	int64_t toInt64 ();
	uint32_t toUInt32 ();
	uint64_t toUInt64 ();

      private:
#if defined(ASDL_USE_GNU_MP)
	mpz_ptr _rep;
#endif
    };

#ifdef XXX
    inline integer operator+ (integer const &a)
    {
    }
    inline integer operator- (integer const &a)
    {
    }
    inline integer operator~ (integer const &a)
    {
    }

    inline integer operator+ (integer const &a, integer const &b)
    {
    }
    inline integer operator- (integer const &a, integer const &b)
    {
    }
    inline integer operator* (integer const &a, integer const &b)
    {
    }
    inline integer operator/ (integer const &a, integer const &b)
    {
    }
    inline integer operator% (integer const &a, integer const &b)
    {
    }
    inline integer operator& (integer const &a, integer const &b)
    {
    }
    inline integer operator| (integer const &a, integer const &b)
    {
    }
    inline integer operator^ (integer const &a, integer const &b)
    {
    }

//    __GMP_DEFINE_BINARY_FUNCTION_UI(operator<<, __gmp_binary_lshift)
//    __GMP_DEFINE_BINARY_FUNCTION_UI(operator>>, __gmp_binary_rshift)

    inline bool operator== (integer const &a, integer const &b)
    {
    }
    inline bool operator!= (integer const &a, integer const &b)
    {
    }
    inline bool operator< (integer const &a, integer const &b)
    {
    }
    inline bool operator<= (integer const &a, integer const &b)
    {
    }
    inline bool operator> (integer const &a, integer const &b)
    {
    }
    inline bool operator>= (integer const &a, integer const &b)
    {
    }

    inline integer & operator+= (integer const &a, integer const &b)
    {
    }
    inline integer & operator-= (integer const &a, integer const &b)
    {
    }
    inline integer & operator*= (integer const &a, integer const &b)
    {
    }
    inline integer & operator/= (integer const &a, integer const &b)
    {
    }
    inline integer & operator%= (integer const &a, integer const &b)
    {
    }

    inline integer & operator&= (integer const &a, integer const &b)
    {
    }
    inline integer & operator|= (integer const &a, integer const &b)
    {
    }
    inline integer & operator^= (integer const &a, integer const &b)
    {
    }

//    __GMPZ_DEFINE_COMPOUND_OPERATOR_UI(operator<<=, __gmp_binary_lshift)
//    __GMPZ_DEFINE_COMPOUND_OPERATOR_UI(operator>>=, __gmp_binary_rshift)

//    __GMPZ_DEFINE_INCREMENT_OPERATOR(operator++, __gmp_unary_increment)
//    __GMPZ_DEFINE_INCREMENT_OPERATOR(operator--, __gmp_unary_decrement)

#endif

} // namespace asdl

#endif //! _ASDL_INTEGER_HXX_
