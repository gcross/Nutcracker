//@+leo-ver=5-thin
//@+node:gcross.20110213161858.1816: * @thin states.cpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110213161858.1817: ** << Includes >>
#include "connectors.hpp"
#include "core.hpp"
#include "states.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110213161858.1818: ** << Usings >>
namespace ublas = boost::numeric::ublas;
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
//@+node:gcross.20110213161858.1821: *3* computeStateVector
StateVector computeStateVector(vector<StateSiteAny const*> const& state_sites) {
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
//@+node:gcross.20110126102637.2188: *3* computeMiddleOverlapSiteFromStateSite
OverlapSite<Middle> computeOverlapSiteFromStateSite(StateSite<Middle> const& state_site) {
    OverlapSite<Middle> overlap_site(dimensionsOf(state_site));
    Core::form_overlap_site_tensor(
         state_site.rightDimension(as_unsigned_integer)
        ,state_site.leftDimension(as_unsigned_integer)
        ,state_site.physicalDimension(as_unsigned_integer)
        ,state_site
        ,overlap_site
    );
    return boost::move(overlap_site);
}
//@+node:gcross.20110126102637.2189: *3* computeOverlapSitesFromStateSitesAndNormalize
OverlapSitesFromStateSitesAndNormalizeResult computeOverlapSitesFromStateSitesAndNormalize(
      StateSite<Middle> const& middle_state_site
     ,StateSite<Right> const& right_state_site
) {
    OverlapSite<Left> left_overlap_site_from_middle_state_site(dimensionsOf(middle_state_site));
    OverlapSite<Middle> middle_overlap_site_from_middle_state_site(dimensionsOf(middle_state_site));
    StateSite<Middle> middle_state_site_from_right_state_site(dimensionsOf(right_state_site));
    OverlapSite<Right> right_overlap_site_from_right_state_site(dimensionsOf(right_state_site));
    Core::form_norm_overlap_tensors(
         middle_state_site.leftDimension(as_unsigned_integer)
        ,middle_state_site | right_state_site
        ,right_state_site.rightDimension(as_unsigned_integer)
        ,middle_state_site.physicalDimension(as_unsigned_integer)
        ,right_state_site.physicalDimension(as_unsigned_integer)
        ,middle_state_site
        ,right_state_site
        ,left_overlap_site_from_middle_state_site
        ,middle_overlap_site_from_middle_state_site
        ,middle_state_site_from_right_state_site
        ,right_overlap_site_from_right_state_site
    );
    return
        OverlapSitesFromStateSitesAndNormalizeResult(
             boost::move(left_overlap_site_from_middle_state_site)
            ,boost::move(middle_overlap_site_from_middle_state_site)
            ,boost::move(middle_state_site_from_right_state_site)
            ,boost::move(right_overlap_site_from_right_state_site)
        );
}
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
//@+node:gcross.20110124175241.1649: *3* increaseDimensionBetween
template<typename side1,typename side2>
static IncreaseDimensionBetweenResult<side1,side2> implIncreaseDimensionBetween(
      unsigned int const new_dimension
    , StateSite<side1> const& old_site_1
    , StateSite<side2> const& old_site_2
) {
    unsigned int const old_dimension = old_site_1 | old_site_2;
    assert(new_dimension >= old_dimension);

    StateSite<side1> new_site_1(
         old_site_1.physicalDimension()
        ,old_site_1.leftDimension()
        ,RightDimension(new_dimension)
    );
    StateSite<side2> new_site_2(
         old_site_2.physicalDimension()
        ,LeftDimension(new_dimension)
        ,old_site_2.rightDimension()
    );

    int const info =
        new_dimension > old_dimension
            ? Core::increase_bandwidth_between(
                 old_site_1.leftDimension(as_unsigned_integer)
                ,old_dimension
                ,old_site_2.rightDimension(as_unsigned_integer)
                ,old_site_1.physicalDimension(as_unsigned_integer)
                ,old_site_2.physicalDimension(as_unsigned_integer)
                ,new_dimension
                ,old_site_1
                ,old_site_2
                ,new_site_1
                ,new_site_2
              )
            : Core::norm_denorm_going_left(
                 old_site_1.leftDimension(as_unsigned_integer)
                ,old_dimension
                ,old_site_2.rightDimension(as_unsigned_integer)
                ,old_site_1.physicalDimension(as_unsigned_integer)
                ,old_site_2.physicalDimension(as_unsigned_integer)
                ,old_site_1
                ,old_site_2
                ,new_site_1
                ,new_site_2
              )
    ;
    if(info != 0) throw NormalizationError(info);
    return IncreaseDimensionBetweenResult<side1,side2>
        (boost::move(new_site_1)
        ,boost::move(new_site_2)
        );
}

IncreaseDimensionBetweenResult<Right,Right> increaseDimensionBetweenRightRight(
      unsigned int new_dimension
    , StateSite<Right> const& old_site_1
    , StateSite<Right> const& old_site_2
) { return implIncreaseDimensionBetween(new_dimension,old_site_1,old_site_2); }

IncreaseDimensionBetweenResult<Middle,Right>increaseDimensionBetweenMiddleRight(
      unsigned int new_dimension
    , StateSite<Middle> const& old_site_1
    , StateSite<Right> const& old_site_2
) { return implIncreaseDimensionBetween(new_dimension,old_site_1,old_site_2); }
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
