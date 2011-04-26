//@+leo-ver=5-thin
//@+node:gcross.20110214155808.1862: * @file boundaries.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110220182654.2024: ** << License >>
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

//@+<< Includes >>
//@+node:gcross.20110214155808.1863: ** << Includes >>
#include "boundaries.hpp"
#include "core.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110214155808.1864: ** << Usings >>
//@-<< Usings >>

//@+others
//@+node:gcross.20110214155808.1873: ** Functions
//@+node:gcross.20110214155808.1874: *3* computeExpectationValueAtSite
complex<double> computeExpectationValueAtSite(
      ExpectationBoundary<Left> const& left_boundary
    , StateSite<Middle> const& state_site
    , OperatorSite const& operator_site
    , ExpectationBoundary<Right> const& right_boundary
) {
    return Core::compute_expectation(
         left_boundary | state_site
        ,state_site | right_boundary
        ,left_boundary | operator_site
        ,operator_site | right_boundary
        ,operator_site | state_site
        ,left_boundary
        ,state_site
        ,operator_site.numberOfMatrices(),operator_site,operator_site
        ,right_boundary
    );
}
//@+node:gcross.20110214155808.1875: *3* contractExpectationBoundaries
complex<double> contractExpectationBoundaries(
      ExpectationBoundary<Left> const& left_boundary
    , ExpectationBoundary<Right> const& right_boundary
) {
    return Core::contract_expectation_boundaries(
         connectDimensions(
             "left boundary state"
            ,left_boundary.stateDimension(as_unsigned_integer)
            ,"right boundary state"
            ,right_boundary.stateDimension(as_unsigned_integer)
         )
        ,connectDimensions(
             "left boundary operator"
            ,left_boundary.operatorDimension(as_unsigned_integer)
            ,"right boundary operator"
            ,right_boundary.operatorDimension(as_unsigned_integer)
         )
        ,left_boundary
        ,right_boundary
    );
}
//@+node:gcross.20110215235924.2010: *3* contractSOSLeft
ExpectationBoundary<Left> contractSOSLeft(
      ExpectationBoundary<Left> const& old_boundary
    , StateSite<Left> const& state_site
    , OperatorSite const& operator_site
) {
    return Unsafe::contractSOSLeft(old_boundary,state_site,operator_site);
}
//@+node:gcross.20110214155808.1877: *3* contractSOSRight
ExpectationBoundary<Right> contractSOSRight(
      ExpectationBoundary<Right> const& old_boundary
    , StateSite<Right> const& state_site
    , OperatorSite const& operator_site
) {
    ExpectationBoundary<Right> new_boundary
        (OperatorDimension(operator_site.leftDimension(as_unsigned_integer))
        ,StateDimension(state_site.leftDimension(as_unsigned_integer))
        );
    Core::contract_sos_right(
         state_site.leftDimension(as_unsigned_integer)
        ,state_site | old_boundary
        ,operator_site.leftDimension(as_unsigned_integer)
        ,operator_site | old_boundary
        ,operator_site | state_site
        ,old_boundary
        ,operator_site.numberOfMatrices(),operator_site,operator_site
        ,state_site
        ,new_boundary
    );
    return boost::move(new_boundary);
}
//@+node:gcross.20110216193817.1923: *3* contractSSLeft
OverlapBoundary<Left> contractSSLeft(
      OverlapBoundary<Left> const& old_boundary
    , OverlapSite<Left> const& overlap_site
    , StateSite<Left> const& state_site
) {
    return Unsafe::contractSSLeft(old_boundary,overlap_site,state_site);
}
//@+node:gcross.20110214155808.1879: *3* contractSSRight
OverlapBoundary<Right> contractSSRight(
      OverlapBoundary<Right> const& old_boundary
    , OverlapSite<Right> const& overlap_site
    , StateSite<Right> const& state_site
) {
    OverlapBoundary<Right> new_boundary
        (OverlapDimension(overlap_site.leftDimension(as_unsigned_integer))
        ,StateDimension(state_site.leftDimension(as_unsigned_integer))
        );
    Core::contract_ss_right(
         overlap_site.leftDimension(as_unsigned_integer)
        ,overlap_site | old_boundary
        ,state_site.leftDimension(as_unsigned_integer)
        ,state_site | old_boundary
        ,overlap_site | state_site
        ,old_boundary
        ,overlap_site
        ,state_site
        ,new_boundary
    );
    return boost::move(new_boundary);
}
//@+node:gcross.20110215235924.2011: *3* Unsafe
namespace Unsafe {

//@+others
//@+node:gcross.20110214155808.1878: *4* contractSSLeft
OverlapBoundary<Left> contractSSLeft(
      OverlapBoundary<Left> const& old_boundary
    , OverlapSiteAny const& overlap_site
    , StateSiteAny const& state_site
) {
    OverlapBoundary<Left> new_boundary
        (OverlapDimension(overlap_site.rightDimension(as_unsigned_integer))
        ,StateDimension(state_site.rightDimension(as_unsigned_integer))
        );
    Core::contract_ss_left(
         old_boundary | overlap_site
        ,overlap_site.rightDimension(as_unsigned_integer)
        ,old_boundary | state_site
        ,state_site.rightDimension(as_unsigned_integer)
        ,overlap_site | state_site
        ,old_boundary
        ,overlap_site
        ,state_site
        ,new_boundary
    );
    return boost::move(new_boundary);
}
//@+node:gcross.20110214155808.1876: *4* contractSOSLeft
ExpectationBoundary<Left> contractSOSLeft(
      ExpectationBoundary<Left> const& old_boundary
    , StateSiteAny const& state_site
    , OperatorSite const& operator_site
) {
    ExpectationBoundary<Left> new_boundary
        (OperatorDimension(operator_site.rightDimension(as_unsigned_integer))
        ,StateDimension(state_site.rightDimension(as_unsigned_integer))
        );
    Core::contract_sos_left(
         old_boundary | state_site
        ,state_site.rightDimension(as_unsigned_integer)
        ,old_boundary | operator_site
        ,operator_site.rightDimension(as_unsigned_integer)
        ,operator_site | state_site
        ,old_boundary
        ,operator_site.numberOfMatrices(),operator_site,operator_site
        ,state_site
        ,new_boundary
    );
    return boost::move(new_boundary);
}
//@-others

}
//@-others

}
//@-leo
