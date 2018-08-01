/*! \file asdl-stream.cxx
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include <asdl.hxx>

namespace asdl {

  /***** class asdl::ostream methods *****/

    void encode_bool (bool b)
    {
	if (b) { this->_os.put(0) } else { this->_os.put(1); }
    }

    void encode_int (int i);
    void encode_uint (unsigned int ui);
    void encode_tag8 (unsigned int ui);
    void encode_tag16 (unsigned int ui);
    void encode_string (std::string const &s);

  /***** class asdl::istream methods *****/

} // namespace asdl
