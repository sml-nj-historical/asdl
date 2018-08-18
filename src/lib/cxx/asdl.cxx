/*! \file asdl.cxx
 *
 * ASDL runtime support for C++
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "asdl/asdl.hxx"

namespace asdl {

    void encode_int (outstream & os, int i);

    void encode_uint (outstream & os, unsigned int ui);

    void encode_string (outstream & os, std::string const &s);

    int decode_int (instream &is)
    {
	unsigned char b0 = is.getb();
	unsigned int v = b0 & 0x1F;
	switch (b0 >> 6) {
	  case 3: v = (v << 8) | is.getb();
	  case 2: v = (v << 8) | is.getb();
	  case 1: v = (v << 8) | is.getb();
	  default:
	    break;
	}
	if ((b0 & 0x20) != 0) {
	    return -static_cast<int>(v);
	}
	else {
	    return static_cast<int>(v);
	}
    }

    unsigned int decode_uint (instream &is)
    {
	unsigned char b0 = is.getb();
	unsigned int v = b0 & 0x3F;
	switch (b0 >> 6) {
	  case 3: v = (v << 8) | is.getb();
	  case 2: v = (v << 8) | is.getb();
	  case 1: v = (v << 8) | is.getb();
	  default:
	    break;
	}
	return v;
    }

    std::string decode_string (instream &is);

} // namespace asdl
