/*! \file asdl-integer.cxx
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "asdl-stream.hxx"
#include "asdl-integer.hxx"

namespace asdl {

    integer::integer ()
      : _sign(false), _digits{0}
    { }

    integer::integer (uint32_t n)
      : _sign(false), _digits{n}
    { }

    integer::integer (int32_t n)
      : _sign(n < 0), _digits{ static_cast<uint32_t>(abs(n)) }
    { }

    integer::integer (uint64_t n)
      : _sign(false)
    {
	if (n < 0x100000000) {
	    this->_digits.push_back(uint32_t(n));
	}
	else {
	    this->_digits.push_back(uint32_t(n));
	    this->_digits.push_back(uint32_t(n >> 32));
	}
    }

    integer::integer (int64_t n)
      : _sign(n < 0)
    {
	uint64_t ui = static_cast<uint64_t>(abs(n));
	if (ui < 0x100000000) {
	    this->_digits.push_back(uint32_t(ui));
	}
	else {
	    this->_digits.push_back(uint32_t(ui));
	    this->_digits.push_back(uint32_t(ui >> 32));
	}
    }

    asdl::ostream integer::pickle (asdl::ostream &os)
    {
    }

    integer::integer (asdl::istream &is)
    {
    }

  // extract fixed-size integers (both checked and unchecked versions)
    bool integer::get_uint8 (uint8_t &n);
    uint8_t integer::to_uint8 ();

    bool integer::get_int8 (int8_t &n);
    int8_t integer::to_int8 ();

    bool integer::get_uint16 (uint16_t &n);
    uint16_t integer::to_uint16 ();

    bool integer::get_int16 (int16_t &n);
    int16_t integer::to_int16 ();

    bool integer::get_uint32 (uint32_t &n);
    uint32_t integer::to_uint32 ();

    bool integer::get_int32 (int32_t &n);
    int32_t integer::to_int32 ();

    bool integer::get_uint64 (uint64_t &n);
    uint64_t integer::to_uint64 ();

    bool integer::get_int64 (int64_t &n);
    int64_t integer::to_int64 ();

} // namespace asdl
