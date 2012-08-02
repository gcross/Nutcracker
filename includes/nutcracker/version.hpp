//@+leo-ver=5-thin
//@+node:gcross.20110910164923.2927: * @file version.hpp
//@@language cplusplus
#ifndef NUTCRACKER_VERSION_HPP
#define NUTCRACKER_VERSION_HPP

//@+<< Includes >>
//@+node:gcross.20110910164923.2929: ** << Includes >>
#include <boost/container/vector.hpp>
#include <boost/tokenizer.hpp>
#include <boost/range/iterator_range.hpp>
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110910164923.2930: ** << Usings >>
//@-<< Usings >>

//@+others
//@+node:gcross.20110910164923.2931: ** Values
extern std::string const version_string;
extern boost::container::vector<unsigned int> version;
//@-others

}

#endif
//@-leo
