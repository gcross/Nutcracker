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
//@-<< Usings >>

//@+others
//@+node:gcross.20110214164734.2007: ** Values
StateVectorFragment const StateVectorFragment::trivial(make_trivial);
//@+node:gcross.20110214155808.1917: ** Exceptions
//@+node:gcross.20110214155808.1918: *3* NormalizationError
NormalizationError::NormalizationError(int info)
    : Exception(
        (format("Numerical error encountered when normalizing a state site (info = %1%)")
            % info
        ).str()
      )
    , info(info)
{ }
//@+node:gcross.20110213161858.1820: ** Functions
//@+node:gcross.20110214155808.1984: *3* extendStateVectorFragment
StateVectorFragment extendStateVectorFragment(
      StateVectorFragment const& old_fragment
    , StateSiteAny const& state_site
) {
    StateVectorFragment new_fragment
        (PhysicalDimension(
            old_fragment.physicalDimension(as_unsigned_integer)
           *state_site.physicalDimension(as_unsigned_integer)
         )
        ,state_site.rightDimension()
        );
    Core::extend_state_vector_fragment(
         old_fragment | state_site
        ,state_site.rightDimension(as_unsigned_integer)
        ,old_fragment.physicalDimension(as_unsigned_integer)
        ,state_site.physicalDimension(as_unsigned_integer)
        ,old_fragment
        ,state_site
        ,new_fragment
    );
    return boost::move(new_fragment);
}
//@+node:gcross.20110124175241.1649: *3* increaseDimensionBetweenXY
IncreaseDimensionBetweenResult<Right,Right> increaseDimensionBetweenRightRight(
      unsigned int new_dimension
    , StateSite<Right> const& old_site_1
    , StateSite<Right> const& old_site_2
) { return Unsafe::increaseDimensionBetween<Right,Right>(new_dimension,old_site_1,old_site_2); }

IncreaseDimensionBetweenResult<Middle,Right>increaseDimensionBetweenMiddleRight(
      unsigned int new_dimension
    , StateSite<Middle> const& old_site_1
    , StateSite<Right> const& old_site_2
) { return Unsafe::increaseDimensionBetween<Middle,Right>(new_dimension,old_site_1,old_site_2); }
//@+node:gcross.20110124175241.1654: *3* moveSiteCursorLeft
MoveSiteCursorResult<Left> moveSiteCursorLeft(
      StateSite<Middle> const& old_state_site_2
    , StateSite<Left> const& old_state_site_1
) {
    StateSite<Middle> new_state_site_1(dimensionsOf(old_state_site_1));
    StateSite<Right> new_state_site_2(dimensionsOf(old_state_site_2));
    unsigned int const info =
    Core::norm_denorm_going_left(
         old_state_site_1.leftDimension(as_unsigned_integer)
        ,old_state_site_1 | old_state_site_2
        ,old_state_site_2.rightDimension(as_unsigned_integer)
        ,old_state_site_1.physicalDimension(as_unsigned_integer)
        ,old_state_site_2.physicalDimension(as_unsigned_integer)
        ,old_state_site_1
        ,old_state_site_2
        ,new_state_site_1
        ,new_state_site_2
    );
    if(info != 0) throw NormalizationError(info);
    return MoveSiteCursorResult<Left>
            (boost::move(new_state_site_1)
            ,boost::move(new_state_site_2)
            );
}
//@+node:gcross.20110125120748.1518: *3* moveSiteCursorRight
MoveSiteCursorResult<Right> moveSiteCursorRight(
      StateSite<Middle> const& old_state_site_1
    , StateSite<Right> const& old_state_site_2
) {
    StateSite<Left> new_state_site_1(dimensionsOf(old_state_site_1));
    StateSite<Middle> new_state_site_2(dimensionsOf(old_state_site_2));
    unsigned int const info =
    Core::norm_denorm_going_right(
         old_state_site_1.leftDimension(as_unsigned_integer)
        ,old_state_site_1 | old_state_site_2
        ,old_state_site_2.rightDimension(as_unsigned_integer)
        ,old_state_site_1.physicalDimension(as_unsigned_integer)
        ,old_state_site_2.physicalDimension(as_unsigned_integer)
        ,old_state_site_1
        ,old_state_site_2
        ,new_state_site_1
        ,new_state_site_2
    );
    if(info != 0) throw NormalizationError(info);
    return MoveSiteCursorResult<Right>
            (boost::move(new_state_site_2)
            ,boost::move(new_state_site_1)
            );
}
//@+node:gcross.20110124175241.1645: *3* randomStateSiteMiddle
StateSite<Middle> randomStateSiteMiddle(
      const PhysicalDimension physical_dimension
    , const LeftDimension left_dimension
    , const RightDimension right_dimension
) {
    StateSite<Middle> state_site
        (physical_dimension
        ,left_dimension
        ,right_dimension
        );
    Core::rand_unnorm_state_site_tensor(
         *right_dimension
        ,*left_dimension
        ,*physical_dimension
        ,state_site
    );  
    return boost::move(state_site);
}
//@+node:gcross.20110124175241.1647: *3* randomStateSiteRight
StateSite<Right> randomStateSiteRight(
      const PhysicalDimension physical_dimension
    , const LeftDimension left_dimension
    , const RightDimension right_dimension
) {
    if((*right_dimension) > (*physical_dimension)*(*left_dimension)) {
        throw NotEnoughDegreesOfFreedomToNormalizeError(
                 "right"
                ,*right_dimension
                ,"physical"
                ,*physical_dimension
                ,"left"
                ,*left_dimension
        );
    }
    if((*left_dimension) > (*physical_dimension)*(*right_dimension)) {
        throw NotEnoughDegreesOfFreedomToNormalizeError(
                 "left"
                ,*left_dimension
                ,"physical"
                ,*physical_dimension
                ,"right"
                ,*right_dimension
        );
    }
    StateSite<Right> state_site
        (physical_dimension
        ,left_dimension
        ,right_dimension
        );
    Core::rand_norm_state_site_tensor(
         *right_dimension
        ,*left_dimension
        ,*physical_dimension
        ,state_site
    );
    return boost::move(state_site);
}
//@-others

}
//@-leo
