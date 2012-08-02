//@+leo-ver=5-thin
//@+node:gcross.20110510004855.2256: * @file flat.cpp
//@@language cplusplus
//@+<< Documentation >>
//@+node:gcross.20110510004855.2183: ** << Documentation >>
/*!
\file flat.cpp
\brief Classes and functions relating to flat representations of states
*/
//@-<< Documentation >>

//@+<< Includes >>
//@+node:gcross.20110510004855.2259: ** << Includes >>
#include "nutcracker/flat.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110510004855.2260: ** << Usings >>
//@-<< Usings >>

//@+others
//@+node:gcross.20110510004855.2265: ** Values
StateVectorFragment const StateVectorFragment::trivial(make_trivial);
//@+node:gcross.20110510004855.2261: ** Functions
//@+node:gcross.20110510004855.2263: *3* extendStateVectorFragment
StateVectorFragment extendStateVectorFragment(
      StateVectorFragment const& old_fragment
    , StateSiteAny const& state_site
) {
    StateVectorFragment new_fragment
        (PhysicalDimension(
            old_fragment.physicalDimension()
           *state_site.physicalDimension()
         )
        ,state_site.rightDimension(as_dimension)
        );
    Core::extend_state_vector_fragment(
         old_fragment | state_site
        ,state_site.rightDimension()
        ,old_fragment.physicalDimension()
        ,state_site.physicalDimension()
        ,old_fragment
        ,state_site
        ,new_fragment
    );
    return boost::move(new_fragment);
}
//@-others

}
//@-leo
