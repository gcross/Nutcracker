#ifndef NUTCRACKER_VERSION_HPP
#define NUTCRACKER_VERSION_HPP

#include <boost/container/vector.hpp>
#include <boost/tokenizer.hpp>
#include <boost/range/iterator_range.hpp>

namespace Nutcracker {


extern std::string const version_string;
extern boost::container::vector<unsigned int> version;

}

#endif
