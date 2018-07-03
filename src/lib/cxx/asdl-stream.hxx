/*! \file asdl-stream.hxx
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#ifndef _ASDL_STREAM_HXX_
#define _ASDL_STREAM_HXX_

#include <string>
#include <iostream>
#include <iterator>

#include "asdl-integer.hxx"

namespace asdl {

    class istream {
      public:

      // create a stream from an existing C++ input stream (should be in binary mode!)
	istream (std::istream &is);
      // create a stream from a file name
	istream (std::string const &file);

      // input iterator
	std::iterator<std::input_iterator_tag, char> begin ();

      // read basic values from the input pickle stream
	bool rd_bool ();
	int rd_int ();
	integer rd_integer ();
	std::string rd_string ();

      private:

    };

    class ostream {
      public:

      // create a stream from an existing C++ input stream (should be in binary mode!)
	ostream (std::ostream &os);
      // create a stream from a file name
	ostream (std::string &file);

      // write basic values to the output pickle stream
	void wr_bool (bool b);
	void wr_int (int i);
	void wr_integer (integer const &n);
	void wr_string (std::string const s);

      private:

    };

} // namespace asdl

#endif // !_ASDL_STREAM_HXX_
