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

#include "config.h"

#include <string>
#include <vector>
#include <istream>
#include <ostream>
#include <memory>

namespace asdl {

  //! forward declarations of integer and identifier classes
    class integer;
    class identifier;

  //! exception for decoding error
    class decode_exception {
    };

  //! ASDL output stream
    class outstream {
      public:
	explicit outstream (std::ostream *os) : _os(os) { }

      // no copying allowed!
	outstream (outstream const &) = delete;
	outstream &operator= (outstream const &) = delete;

      // move operations
	outstream (outstream &&os) noexcept
	{
	    this->_os = os._os;
	    os._os = nullptr;
	}
	outstream &operator= (outstream &&rhs) noexcept
	{
	    if (this != &rhs) {
		this->_os = rhs._os;
		rhs._os = nullptr;
	    }
	    return *this;
	}

	~outstream ()
	{
	    if (this->_os != nullptr) {
		delete this->_os;
	    }
	}

	void putc (char c) { this->_os->put(c); }
	void putb (unsigned char c) { this->_os->put(c); }

      protected:
	std::ostream *_os;
    };

  //! ASDL file outstream
    class file_outstream : public outstream {
      public:
	explicit file_outstream (std::string const &file);

      // no copying allowed!
	file_outstream (file_outstream const &) = delete;
	file_outstream &operator= (file_outstream const &) = delete;

	void close () { this->_os->flush(); }
    };

  //! ASDL memory outstream
    class memory_outstream : public outstream {
      public:
	explicit memory_outstream (std::string const &data);

      // no copying allowed!
	memory_outstream (memory_outstream const &) = delete;
	memory_outstream &operator= (memory_outstream const &) = delete;

	std::string get_pickle () const;
    };

  //! ASDL input stream
    class instream {
      public:
	explicit instream (std::istream *is) : _is(is) { }

      // no copying allowed!
	instream (instream const &) = delete;
	instream &operator= (instream const &) = delete;

      // move operations
	instream (instream &&is) noexcept
	{
	    this->_is = is._is;
	    is._is = nullptr;
	}
	instream &operator= (instream &&rhs) noexcept
	{
	    if (this != &rhs) {
		this->_is = rhs._is;
		rhs._is = nullptr;
	    }
	    return *this;
	}

	~instream ()
	{
	    if (this->_is != nullptr) {
		delete this->_is;
	    }
	}

	char getc () { return this->_is->get(); }
	unsigned char getb () { return static_cast<unsigned char>(this->_is->get()); }

      protected:
	std::istream *_is;
    };

  //! ASDL file instream
    class file_instream : public instream {
      public:
	explicit file_instream (std::string const &file);

      // no copying allowed!
	file_instream (file_instream const &) = delete;
	file_instream &operator= (file_instream const &) = delete;
    };

  //! ASDL memory outstream
    class memory_instream : public instream {
      public:
	explicit memory_instream (std::string const &data);

      // no copying allowed!
	memory_instream (memory_instream const &) = delete;
	memory_instream &operator= (memory_instream const &) = delete;
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
    void encode_string (outstream & os, std::string const & s);
    void encode_integer (outstream & os, integer const & i);

  // decode basic values
    bool decode_bool (instream & is)
    {
	return (is.getc() != 0);
    }
    int decode_int (instream & is);
    unsigned int decode_uint (instream & is);
    unsigned int decode_tag8 (instream & is)
    {
	return is.getb();
    }
    unsigned int decode_tag16 (instream & is)
    {
	unsigned int b0 = is.getb();
	unsigned int b1 = is.getb();
	return (b0 << 8) + b1;
    }
    std::string decode_string (instream & is);
    integer decode_integer (instream & is);

} // namespace asdl

#include "asdl-integer.hxx"
//#include "asdl-identifier.hxx"

#endif /* !_ASDL_HXX_ */
