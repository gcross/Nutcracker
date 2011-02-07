//@+leo-ver=5-thin
//@+node:gcross.20110125202132.2156: * @thin utilities.hpp
//@@language cplusplus

#ifndef NUTCRACKER_UTILITIES_HPP
#define NUTCRACKER_UTILITIES_HPP

//@+<< Includes >>
//@+node:gcross.20110125202132.2157: ** << Includes >>
#include <boost/format.hpp>
#include <boost/move/move.hpp>
#include <complex>
#include <exception>
#include <stdint.h>
#include <string>
#include <typeinfo>
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110125202132.2158: ** << Usings >>
using namespace boost;
using namespace std;
//@-<< Usings >>

//@+others
//@+node:gcross.20110125202132.2159: ** struct Exception
struct Exception : public std::exception {
    string const message;
    Exception(string const& message);
    virtual char const* what() const throw();
    virtual ~Exception() throw();
};
//@+node:gcross.20110202200838.1710: ** Exceptions
//@+node:gcross.20110202200838.1709: *3* BadProgrammerException
struct BadProgrammerException : public Exception {
    BadProgrammerException(string const& message) : Exception("BAD PROGRAMMER!!! --- " + message) {}
};
//@+node:gcross.20110206185121.1786: *3* BadLabelException
struct BadLabelException : public BadProgrammerException {
    BadLabelException(string const& symbol, type_info const& type)
      : BadProgrammerException((
          format("Attempted to access templated symbol %1% with invalid type label %2%.")
            % symbol
            % type.name()
        ).str())
    {}
};
//@+node:gcross.20110127123226.2857: ** Functions
inline complex<double> c(double x, double y) { return complex<double>(x,y); }

template<typename T> inline T copyAndReset(T& x) {
    T const old_x = x;
    x = 0;
    return old_x;
}

template<typename T> inline void moveArrayToFrom(T*& to, T*& from) {
    if(to) delete[] to;
    to = copyAndReset(from);
}

extern "C" double dznrm2_(uint32_t const* n, complex<double>* const x, uint32_t const* incx);
inline double dznrm2(uint32_t const n, complex<double>* const x, uint32_t const incx=1) { return dznrm2_(&n,x,&incx); }
//@+node:gcross.20110129220506.1652: ** Macros
#define REPEAT(n) for(unsigned int _counter##__LINE__ = 0; _##counter##__LINE__ < n; ++_##counter##__LINE__)
//@-others

}

#endif
//@-leo
