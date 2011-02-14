//@+leo-ver=5-thin
//@+node:gcross.20110213161858.1810: * @thin states.hpp
//@@language cplusplus

#ifndef NUTCRACKER_STATES_HPP
#define NUTCRACKER_STATES_HPP

//@+<< Includes >>
//@+node:gcross.20110213161858.1811: ** << Includes >>
#include <boost/container/vector.hpp>

#include "tensors.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110213161858.1812: ** << Usings >>
using boost::container::vector;
//@-<< Usings >>

//@+others
//@+node:gcross.20110213161858.1813: ** Functions
StateVector computeStateVector(vector<StateSiteAny const*> state_sites);
//@-others

}

#endif
//@-leo
