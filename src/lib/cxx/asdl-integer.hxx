/*! \file asdl-integer.hxx
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#ifndef _ASDL_INTEGER_HXX_
#define _ASDL_INTEGER_HXX_

#include <cstdint>
#include <vector>

namespace asdl {

    class ostream;
    class istream;

    class integer {
      public:

      // constructors
        integer (); // ZERO
        integer (uint32_t n);
        integer (int32_t n);
        integer (uint64_t n);
        integer (int64_t n);

      // destructor
        ~integer () { }

      // pickle/unpickle operations
	asdl::ostream pickle (asdl::ostream &os);

	integer (asdl::istream &is);

      // extract fixed-size integers (both checked and unchecked versions)
        bool get_uint8 (uint8_t &n);
        uint8_t to_uint8 ();
        bool get_int8 (int8_t &n);
        int8_t to_int8 ();
        bool get_uint16 (uint16_t &n);
        uint16_t to_uint16 ();
        bool get_int16 (int16_t &n);
        int16_t to_int16 ();
        bool get_uint32 (uint32_t &n);
        uint32_t to_uint32 ();
        bool get_int32 (int32_t &n);
        int32_t to_int32 ();
        bool get_uint64 (uint64_t &n);
        uint64_t to_uint64 ();
        bool get_int64 (int64_t &n);
        int64_t to_int64 ();

      private:
      // default representation of MP ints is sign + digits in LSB to MSB order
	bool _sign;
	std::vector<uint32_t> _digits;

    };

} // namespace asdl

#endif // !_ASDL_INTEGER_HXX_

