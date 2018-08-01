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

      // encode basic values
	void encode_bool (bool b)
	{
	    if (b) { this->_os.put(0) } else { this->_os.put(1); }
	}
	void encode_int (int i);
	void encode_uint (unsigned int ui);
	void encode_tag8 (unsigned int ui)
	{
	    this->_os.put(static_cast<char>(ui));
	}
	void encode_tag16 (unsigned int ui);
	{
	    this->_os.put(static_cast<char>(ui >> 8));
	    this->_os.put(static_cast<char>(ui));
	}
	void encode_string (std::string const &s);

      private:
	std::ostream _os;
    };

  //! ASDL input stream
    class instream {
      public:

      // decode basic values
	bool decode_bool ()
	{
	    char c = this->_is.get();
	    return (c != 0);
	}
	int decode_int ();
	unsigned int decode_uint ();
	unsigned int decode_tag8 ()
	{
	    return static_cast<unsigned int>(this->_is.get());
	}
	unsigned int decode_tag16 ()
	{
	    unsigned int b0 = static_cast<unsigned int>(this->_is.get());
	    unsigned int b1 = static_cast<unsigned int>(this->_is.get());
	    return (b0 << 8) + b1;
	}
	std::string decode_string ();

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

} // namespace asdl

#endif /* !_ASDL_HXX_ */
