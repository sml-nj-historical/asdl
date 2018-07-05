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
#if define(ASDL_USE_GNU_MP)
	mpz_class _rep;
#endif
    };

    integer operator+ (integer const &a)
    {
    }
    integer operator- (integer const &a)
    {
    }
    integer operator~ (integer const &a)
    {
    }

    integer operator+ (integer const &a, integer const &b)
    {
    }
    integer operator- (integer const &a, integer const &b)
    {
    }
    integer operator* (integer const &a, integer const &b)
    {
    }
    integer operator/ (integer const &a, integer const &b)
    {
    }
    integer operator% (integer const &a, integer const &b)
    {
    }
    integer operator& (integer const &a, integer const &b)
    {
    }
    integer operator| (integer const &a, integer const &b)
    {
    }
    integer operator^ (integer const &a, integer const &b)
    {
    }

    __GMP_DEFINE_BINARY_FUNCTION_UI(operator<<, __gmp_binary_lshift)
    __GMP_DEFINE_BINARY_FUNCTION_UI(operator>>, __gmp_binary_rshift)

    bool operator== (integer const &a, integer const &b)
    {
    }
    bool operator!= (integer const &a, integer const &b)
    {
    }
    bool operator< (integer const &a, integer const &b)
    {
    }
    bool operator<= (integer const &a, integer const &b)
    {
    }
    bool operator> (integer const &a, integer const &b)
    {
    }
    bool operator>= (integer const &a, integer const &b)
    {
    }

    integer & operator+= (integer const &a, integer const &b)
    {
    }
    integer & operator-= (integer const &a, integer const &b)
    {
    }
    integer & operator*= (integer const &a, integer const &b)
    {
    }
    integer & operator/= (integer const &a, integer const &b)
    {
    }
    integer & operator%= (integer const &a, integer const &b)
    {
    }

    integer & operator&= (integer const &a, integer const &b)
    {
    }
    integer & operator|= (integer const &a, integer const &b)
    {
    }
    integer & operator^= (integer const &a, integer const &b)
    {
    }

    __GMPZ_DEFINE_COMPOUND_OPERATOR_UI(operator<<=, __gmp_binary_lshift)
    __GMPZ_DEFINE_COMPOUND_OPERATOR_UI(operator>>=, __gmp_binary_rshift)

    __GMPZ_DEFINE_INCREMENT_OPERATOR(operator++, __gmp_unary_increment)
    __GMPZ_DEFINE_INCREMENT_OPERATOR(operator--, __gmp_unary_decrement)

} // namespace asdl

#endif //! _ASDL_INTEGER_HXX_
