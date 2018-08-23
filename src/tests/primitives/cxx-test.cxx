/*! \file cxx-test.cxx
 *
 * \author John Reppy
 *
 * C++ test driver for testing primitive pickling operations.
 */

/*
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "asdl/asdl.hxx"
#include <iostream>
#include <ctype.h>

std::string to_string (bool x) { if (x) return "true"; else return "false"; }
bool same (bool x, bool y) { return (x == y); }
bool same (int x, int y) { return (x == y); }
bool same (unsigned int x, unsigned int y) { return (x == y); }
std::string to_string (std::string const &x)
{
    std::string res = "\"";
    for (auto it = x.cbegin();  it != x.cend();  ++it) {
	if (isprint(*it)) {
	    res.push_back (*it);
	} else {
	    switch (*it) {
	      case '\n': res = res + "\\n"; break;
	      case '\t': res = res + "\\t"; break;
	      case '\r': res = res + "\\r"; break;
	      default:
	      // NOTE: this conversion is not really correct, but it should suffice
		res = res + "\\" + to_string(static_cast<int>(*it));
		break;
	    }
	}
    }
    res = res + "\"";
    return res;
}
bool same (std::string const &x, std::string const &y) { return (x == y); }

template<typename T>
void check (
    std::string const &msg,
    void (*encode)(asdl::outstream &, T),
    T (*decode)(asdl::instream &),
    T const &x)
{
    std::cout << "check " << msg << ": unpickle(pickle " << to_string(x) << ")";

    asdl::memory_outstream os;
    encode (os, x);
    asdl::memory_instream is(os.get_pickle());
    T y = decode(is);

    if (! same(x, y)) {
	std::cout << " fail (" << to_string(y) << ")\n";
    }
    else {
	std::cout << " ok\n";
    }

}

template<typename T>
void check (
    std::string const &msg,
    void (*encode)(asdl::outstream &, T const &),
    T (*decode)(asdl::instream &),
    T const &x)
{
    std::cout << "check " << msg << ": unpickle(pickle " << to_string(x) << ")";

    asdl::memory_outstream os;
    encode (os, x);
    asdl::memory_instream is(os.get_pickle());
    T y = decode(is);

    if (! same(x, y)) {
	std::cout << " fail (" << to_string(y) << ")\n";
    }
    else {
	std::cout << " ok\n";
    }

}

void check_bool (bool x)
{
    check<bool>("boolean", asdl::encode_bool, asdl::decode_bool, x);
}

void check_int (int x)
{
    check<int>("int", asdl::encode_int, asdl::decode_int, x);
}

void check_uint (unsigned int x)
{
    check<unsigned int>("uint", asdl::encode_uint, asdl::decode_uint, x);
}

void check_string (std::string const &x)
{
    check<std::string>("string", asdl::encode_string, asdl::decode_string, x);
}

int main ()
{

    check_bool (true);
    check_bool (false);

    check_int (0);
    check_int (-1);
    check_int (1);
    check_int (-32);
    check_int (-31);
    check_int (31);
    check_int (32);
    check_int (-8192);
    check_int (-8191);
    check_int (8191);
    check_int (8192);
    check_int (-2097152);
    check_int (-2097151);
    check_int (2097151);
    check_int (2097152);
    check_int (-536870912);	// lower bound
    check_int (536870911);	// upper bound

    check_uint (0x0);
    check_uint (0x1);
    check_uint (0x3f);
    check_uint (0x100);
    check_uint (0x3fff);
    check_uint (0x10000);
    check_uint (0x3fffff);
    check_uint (0x1000000);
    check_uint (0x3fffffff);	// upper bound

    check_string ("");
    check_string (" ");
    check_string ("hello world\n");

    return 0;
}
