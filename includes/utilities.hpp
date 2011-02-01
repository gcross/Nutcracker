//@+leo-ver=5-thin
//@+node:gcross.20110125202132.2156: * @thin utilities.hpp
//@@language cplusplus

#ifndef NUTCRACKER_UTILITIES_HPP
#define NUTCRACKER_UTILITIES_HPP

//@+<< Includes >>
//@+node:gcross.20110125202132.2157: ** << Includes >>
#include <complex>
#include <exception>
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110125202132.2158: ** << Usings >>
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
//@+node:gcross.20110127123226.2857: ** Functions
inline complex<double> c(double x, double y) { return complex<double>(x,y); }
//@+node:gcross.20110129220506.1652: ** Macros
#define REPEAT(n) for(unsigned int _##n##counter##__LINE__ = 0; _##n##counter##__LINE__ < n; ++_##n##counter##__LINE__)
//@-others

}

#endif
//@-leo
