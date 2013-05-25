// Includes {{{
#include <boost/lambda/lambda.hpp>
#include <boost/range/adaptor/sliced.hpp>
#include <boost/range/adaptor/strided.hpp>
#include <boost/range/algorithm/copy.hpp>
#include <boost/range/algorithm/fill.hpp>
#include <boost/range/algorithm/transform.hpp>

#include "nutcracker/core.hpp"
#include "nutcracker/states.hpp"
// }}}

namespace Nutcracker {

// Usings {{{
using boost::adaptors::sliced;
using boost::adaptors::strided;

namespace lambda = boost::lambda;
// }}}

NormalizationError::NormalizationError(int info) // {{{
    : std::runtime_error(
        (format("Numerical error encountered when normalizing a state site (info = %1%)")
            % info
        ).str()
      )
    , info(info)
{ } // }}}

StateSite<None> constructStateSite( // {{{
      PhysicalDimension const physical_dimension
    , LeftDimension const left_dimension
    , RightDimension const right_dimension
    , vector<StateSiteLink> const& links
) {
    StateSite<None> state_site
        (physical_dimension
        ,left_dimension
        ,right_dimension
        );
    boost::fill(state_site,c(0,0));

    BOOST_FOREACH(
         StateSiteLink const& link
        ,links
    ) {
        assert(link.from > 0);
        assert(link.from <= state_site.leftDimension());
        assert(link.to > 0);
        assert(link.to <= state_site.rightDimension());
        assert(link.label->size() == *physical_dimension);
        boost::copy(
            *link.label,
            (state_site
                | sliced((link.to-1) + (link.from-1) * (*right_dimension),state_site.size())
                | strided((*left_dimension)*(*right_dimension))
            ).begin()
        );
    }

    return boost::move(state_site);
} // }}}

IncreaseDimensionBetweenResult<Right,Right> increaseDimensionBetweenRightRight( // {{{
      unsigned int new_dimension
    , StateSite<Right> const& old_site_1
    , StateSite<Right> const& old_site_2
) { return Unsafe::increaseDimensionBetween<Right,Right>(new_dimension,old_site_1,old_site_2); } // }}}

IncreaseDimensionBetweenResult<Middle,Right>increaseDimensionBetweenMiddleRight( // {{{
      unsigned int new_dimension
    , StateSite<Middle> const& old_site_1
    , StateSite<Right> const& old_site_2
) { return Unsafe::increaseDimensionBetween<Middle,Right>(new_dimension,old_site_1,old_site_2); }
MoveSiteCursorResult<Left> moveSiteCursorLeft(
      StateSite<Middle> const& old_state_site_2
    , StateSite<Left> const& old_state_site_1
) {
    return Unsafe::moveSiteCursorLeft(old_state_site_1,old_state_site_2);
} // }}}

namespace Unsafe { // {{{

MoveSiteCursorResult<Left> moveSiteCursorLeft( // {{{
      StateSiteAny const& old_state_site_1
    , StateSiteAny const& old_state_site_2
) {
    old_state_site_2.assertCanBeLeftNormalized();
    StateSite<Middle> new_state_site_1(dimensionsOf(old_state_site_1));
    StateSite<Right> new_state_site_2(dimensionsOf(old_state_site_2));
    unsigned int const info =
    Core::norm_denorm_going_left(
         old_state_site_1.leftDimension()
        ,connectDimensions(
            "left state site right",
            old_state_site_1.rightDimension(),
            "right state site left",
            old_state_site_2.leftDimension()
         )
        ,old_state_site_2.rightDimension()
        ,old_state_site_1.physicalDimension()
        ,old_state_site_2.physicalDimension()
        ,old_state_site_1
        ,old_state_site_2
        ,new_state_site_1
        ,new_state_site_2
    );
    if(info != 0) throw NormalizationError(info);
    boost::transform(
        new_state_site_1,
        new_state_site_1.begin(),
        lambda::_1 / new_state_site_1.norm()
    );
    return MoveSiteCursorResult<Left>
            (boost::move(new_state_site_1)
            ,boost::move(new_state_site_2)
            );
} // }}}

} // }}}

MoveSiteCursorResult<Right> moveSiteCursorRight( // {{{
      StateSite<Middle> const& old_state_site_1
    , StateSite<Right> const& old_state_site_2
) {
    old_state_site_1.assertCanBeRightNormalized();
    StateSite<Left> new_state_site_1(dimensionsOf(old_state_site_1));
    StateSite<Middle> new_state_site_2(dimensionsOf(old_state_site_2));
    unsigned int const info =
    Core::norm_denorm_going_right(
         old_state_site_1.leftDimension()
        ,old_state_site_1 | old_state_site_2
        ,old_state_site_2.rightDimension()
        ,old_state_site_1.physicalDimension()
        ,old_state_site_2.physicalDimension()
        ,old_state_site_1
        ,old_state_site_2
        ,new_state_site_1
        ,new_state_site_2
    );
    if(info != 0) throw NormalizationError(info);
    boost::transform(
        new_state_site_2,
        new_state_site_2.begin(),
        lambda::_1 / new_state_site_2.norm()
    );
    return MoveSiteCursorResult<Right>
            (boost::move(new_state_site_2)
            ,boost::move(new_state_site_1)
            );
} // }}}

StateSite<Middle> randomStateSiteMiddle( // {{{
      const PhysicalDimension physical_dimension
    , const LeftDimension left_dimension
    , const RightDimension right_dimension
) {
    StateSite<Middle> state_site
        (physical_dimension
        ,left_dimension
        ,right_dimension
        );
    state_site.assertCanBeNormalized();
    Core::rand_unnorm_state_site_tensor(
         *right_dimension
        ,*left_dimension
        ,*physical_dimension
        ,state_site
    );  
    return boost::move(state_site);
} // }}}

StateSite<Right> randomStateSiteRight( // {{{
      const PhysicalDimension physical_dimension
    , const LeftDimension left_dimension
    , const RightDimension right_dimension
) {
    StateSite<Right> state_site
        (physical_dimension
        ,left_dimension
        ,right_dimension
        );
    state_site.assertCanBeNormalized();
    Core::rand_norm_state_site_tensor(
         *right_dimension
        ,*left_dimension
        ,*physical_dimension
        ,state_site
    );
    return boost::move(state_site);
} // }}}

}
