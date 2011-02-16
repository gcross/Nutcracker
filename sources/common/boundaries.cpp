//@+leo-ver=5-thin
//@+node:gcross.20110214155808.1862: * @thin boundaries.cpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110214155808.1863: ** << Includes >>
#include "boundaries.hpp"
#include "connectors.hpp"
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
//@+node:gcross.20110214155808.1876: *3* contractSOSLeft
ExpectationBoundary<Left> contractSOSLeft(
      ExpectationBoundary<Left> const& old_boundary
    , StateSite<Left> const& state_site
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
//@+node:gcross.20110214155808.1878: *3* contractSSLeft
OverlapBoundary<Left> contractSSLeft(
      OverlapBoundary<Left> const& old_boundary
    , OverlapSite<Left> const& overlap_site
    , StateSite<Left> const& state_site
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
//@-others

}
//@-leo
