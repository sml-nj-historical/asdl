/*! \file asdl.hxx
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#ifndef _ASDL_HXX_
#define _ASDL_HXX_

#include <string>
#include <vector>
#include <istream>
#include <ostream>

namespace asdl {

  //! exception for decoding error
    class decode_exception {
    };

  //! ASDL output stream
    class outstream {
      public:

	void putc (char c) { this->_os.put(c); }
	void putb (unsigned char c) { this->_os.put(c); }

      private:
	std::ostream _os;
    };

  //! ASDL input stream
    class instream {
      public:

	char getc () { return this->_is.get(); }
	unsigned char getb () { return static_cast<unsigned char>(this->_is.get()); }

      private:
	std::istream _is;
    };

  //! wrapper for immediate values
    template <typename T>
    class box {
      public:
	box (T v) : _v(v) { }
	box (instream &is);
	~box () { }
	T value () const { return this->_v; }

      private:
	T _v;
    };

  /***** functions *****/

  // encode basic values
    void encode_bool (outstream & os, bool b)
    {
	if (b) { os.putb(0); } else { os.putb(1); }
    }
    void encode_int (outstream & os, int i);
    void encode_uint (outstream & os, unsigned int ui);
    void encode_tag8 (outstream & os, unsigned int ui)
    {
	os.putb(static_cast<unsigned char>(ui));
    }
    void encode_tag16 (outstream & os, unsigned int ui)
    {
	os.putb(static_cast<unsigned char>(ui >> 8));
	os.putb(static_cast<unsigned char>(ui));
    }
    void encode_string (outstream & os, std::string const &s);

  // decode basic values
    bool decode_bool (instream &is)
    {
	return (is.getc() != 0);
    }
    int decode_int (instream &is);
    unsigned int decode_uint (instream &is);
    unsigned int decode_tag8 (instream &is)
    {
	return is.getb();
    }
    unsigned int decode_tag16 (instream &is)
    {
	unsigned int b0 = is.getb();
	unsigned int b1 = is.getb();
	return (b0 << 8) + b1;
    }
    std::string decode_string (instream &is);

} // namespace asdl

#endif /* !_ASDL_HXX_ */
