#include <boost/foreach.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/tokenizer.hpp>
#include <iterator>

#include "nutcracker/version.hpp"

#include "_config.h"

namespace Nutcracker {


static boost::container::vector<unsigned int> decodeVersionString(std::string const& version_string) {
    boost::container::vector<unsigned int> version;

    BOOST_FOREACH(
        std::string const& component_as_string,
        boost::tokenizer<boost::char_separator<char> >(
            version_string,
            boost::char_separator<char>(".")
        )
    ) {
        version.push_back(boost::lexical_cast<unsigned int,std::string>(component_as_string));
    }

    return boost::move(version);
}
std::string const version_string(Nutcracker_VERSION);
boost::container::vector<unsigned int> version = decodeVersionString(version_string);

}
