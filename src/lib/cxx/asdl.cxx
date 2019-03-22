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
#include <fstream>
#include <sstream>
#include <iostream> // for debuggging
#include <cassert>

namespace asdl {

    file_outstream::file_outstream (std::string const &file)
	: outstream(new std::ofstream(file, std::ios_base::out | std::ios_base::binary))
    {
// FIXME: need to check for failure
    }

    memory_outstream::memory_outstream ()
	: outstream(new std::ostringstream(std::ios_base::out | std::ios_base::binary))
    {
    }

    std::string memory_outstream::get_pickle () const
    {
	return static_cast<std::ostringstream *>(this->_os)->str();
    }

    file_instream::file_instream (std::string const &file)
	: instream(new std::ifstream(file, std::ios_base::in | std::ios_base::binary))
    {
// FIXME: need to check for failure
    }

    memory_instream::memory_instream (std::string const &data)
	: instream(new std::istringstream(data, std::ios_base::in | std::ios_base::binary))
    {
    }

    void encode_int (outstream & os, int i)
    {
	assert ((-536870912 <= i) && (i < 536870912));
	unsigned int sign, ui;
	if (i < 0) {
	    sign = 0x20;
	    ui = -(i+1);
	} else {
	    sign = 0;
	    ui = i;
	}
	if (ui <= 0x1f) { // one byte
	    os.putb (static_cast<unsigned char>(sign | ui));
	}
	else if (ui <= 0x1fff) { // two bytes
	    os.putb (static_cast<unsigned char>(0x40 | sign | (ui >> 8)));
	    os.putb (static_cast<unsigned char>(ui));
	}
	else if (ui <= 0x1fffff) { // three bytes
	    os.putb (static_cast<unsigned char>(0x80 | sign | (ui >> 16)));
	    os.putb (static_cast<unsigned char>(ui >> 8));
	    os.putb (static_cast<unsigned char>(ui));
	}
	else { // four bytes
	    os.putb (static_cast<unsigned char>(0xc0 | sign | (ui >> 24)));
	    os.putb (static_cast<unsigned char>(ui >> 16));
	    os.putb (static_cast<unsigned char>(ui >> 8));
	    os.putb (static_cast<unsigned char>(ui));
	}

    }

    void encode_uint (outstream & os, unsigned int ui)
    {
	if (ui <= 0x3f) { // one byte
	    os.putb (static_cast<unsigned char>(ui));
	}
	else if (ui <= 0x3fff) { // two bytes
	    os.putb (static_cast<unsigned char>(0x40 | (ui >> 8)));
	    os.putb (static_cast<unsigned char>(ui));
	}
	else if (ui <= 0x3fffff) { // three bytes
	    os.putb (static_cast<unsigned char>(0x80 | (ui >> 16)));
	    os.putb (static_cast<unsigned char>(ui >> 8));
	    os.putb (static_cast<unsigned char>(ui));
	}
	else { // four bytes
	    assert (ui <= 0x3fffffff);
	    os.putb (static_cast<unsigned char>(0xc0 | (ui >> 24)));
	    os.putb (static_cast<unsigned char>(ui >> 16));
	    os.putb (static_cast<unsigned char>(ui >> 8));
	    os.putb (static_cast<unsigned char>(ui));
	}
    }

    void encode_string (outstream & os, std::string const &s)
    {
	encode_uint (os, s.length());
	for (auto it = s.cbegin(); it != s.cend(); ++it) {
	    os.putb(static_cast<unsigned char>(*it));
	}
    }

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
	    return -static_cast<int>(v) - 1;
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

    std::string decode_string (instream &is)
    {
	std::string result;
	unsigned int len = decode_uint(is);
	result.reserve(len);
	for (unsigned int i = 0;  i < len;  i++) {
	    result.push_back(is.getc());
	}
	return result;
    }

} // namespace asdl
