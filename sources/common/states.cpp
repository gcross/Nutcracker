//@+leo-ver=5-thin
//@+node:gcross.20110213161858.1816: * @thin states.cpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110213161858.1817: ** << Includes >>
#include "core.hpp"
#include "states.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110213161858.1818: ** << Usings >>
namespace ublas = boost::numeric::ublas;
//@-<< Usings >>

//@+others
//@+node:gcross.20110213161858.1820: ** Functions
//@+node:gcross.20110213161858.1821: *3* computeStateVector
StateVector computeStateVector(vector<StateSiteAny const*> state_sites) {
    if(state_sites.size() == 0) return StateVector();
    StateVectorFragment current_fragment(make_trivial);
    BOOST_FOREACH(StateSiteAny const* state_site,state_sites) {
        StateVectorFragment next_fragment =
            extendStateVectorFragment(
                 current_fragment
                ,*state_site
            );
        current_fragment = boost::move(next_fragment);
    }
    return current_fragment;
}
//@-others

}
//@-leo
