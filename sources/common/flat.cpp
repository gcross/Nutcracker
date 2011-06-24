//@+leo-ver=5-thin
//@+node:gcross.20110510004855.2256: * @file flat.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110510004855.2258: ** << License >>
//@+at
// Copyright (c) 2011, Gregory Crosswhite
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
// 
//     * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//@@c
//@-<< License >>

//@+<< Documentation >>
//@+node:gcross.20110510004855.2183: ** << Documentation >>
/*!
\file flat.cpp
\brief Classes and functions relating to flat representations of states
*/
//@-<< Documentation >>

//@+<< Includes >>
//@+node:gcross.20110510004855.2259: ** << Includes >>
#include "flat.hpp"
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
