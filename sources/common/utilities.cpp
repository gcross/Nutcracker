//@+leo-ver=5-thin
//@+node:gcross.20110125202132.2160: * @thin utilities.cpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110125202132.2161: ** << Includes >>
#include <string>

#include "utilities.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110125202132.2162: ** << Usings >>
//@-<< Usings >>

//@+others
//@+node:gcross.20110125202132.2163: ** struct Exception
Exception::Exception(string const& message) : message(message) { }

const char* Exception::what() const throw() { return message.c_str(); }

Exception::~Exception() throw() { }
//@-others

}
//@-leo
