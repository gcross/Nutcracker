/*!
\file flat.cpp
\brief Classes and functions relating to flat representations of states
*/

#include "nutcracker/flat.hpp"

namespace Nutcracker {


StateVectorFragment const StateVectorFragment::trivial(make_trivial);
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

}
