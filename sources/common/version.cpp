//@+leo-ver=5-thin
//@+node:gcross.20110910164923.2938: * @file version.cpp
//@@language cplusplus
//@+<< Includes >>
//@+node:gcross.20110910164923.2940: ** << Includes >>
#include <boost/lexical_cast.hpp>
#include <boost/range/adaptor/transformed.hpp>
#include <boost/range/algorithm/copy.hpp>
#include <boost/range/iterator_range.hpp>
#include <boost/tokenizer.hpp>
#include <iterator>

#include "nutcracker/version.hpp"

#include "_config.h"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110910164923.2941: ** << Usings >>
//@-<< Usings >>

//@+others
//@+node:gcross.20110910164923.2942: ** Functions
//@+node:gcross.20110910164923.2943: *3* decodeVersionString
static boost::container::vector<unsigned int> decodeVersionString(std::string const& version_string) {
    boost::container::vector<unsigned int> version;

    boost::copy(
        boost::tokenizer<boost::char_separator<char> >(
            version_string,
            boost::char_separator<char>(".")
        ) | boost::adaptors::transformed(&boost::lexical_cast<unsigned int,std::string>),
        std::back_inserter(version)
    );

    return boost::move(version);
}
//@+node:gcross.20110910164923.2944: ** Values
std::string const version_string(Nutcracker_VERSION);
boost::container::vector<unsigned int> version = decodeVersionString(version_string);
//@-others

}
//@-leo
