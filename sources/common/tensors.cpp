#include <boost/lambda/lambda.hpp>
#include <boost/range/algorithm/transform.hpp>
#include <map>
#include <yaml-cpp/yaml.h>

#include "nutcracker/core.hpp"
#include "nutcracker/tensors.hpp"
#include "nutcracker/utilities.hpp"

namespace Nutcracker {

namespace lambda = boost::lambda;

using std::make_pair;
using std::map;

OperatorSite const OperatorSite::trivial(make_trivial);

DEFINE_DUMMY_PARAMETER(AsDimension,as_dimension)
DEFINE_DUMMY_PARAMETER(MakeTrivial,make_trivial)

optional<string> const normalizationOf<Left>::value("left");
optional<string> const normalizationOf<Middle>::value("middle");
optional<string> const normalizationOf<Right>::value("right");
optional<string> const normalizationOf<None>::value(none);
StateSite<Middle> StateSiteAny::normalize() const {
    double const
        normalization = norm(),
        normalization_factor = 1.0 / std::sqrt(normalization);
    if(std::abs(1-normalization) < 1e-13) {
        return StateSite<Middle>(copyFrom(*this));
    } else {
        StateSite<Middle> normalized_state_site(dimensionsOf(*this));
        boost::transform(
            *this,
            normalized_state_site.begin(),
            lambda::_1 * normalization_factor
        );
        return boost::move(normalized_state_site);
    }
}

StateSite<Left> normalizeLeft(StateSite<Middle> const& state_site) {{{
    StateSite<Left> normalized_state_site(dimensionsOf(state_site));
    Core::norm_for_left(
        state_site.rightDimension(),
        state_site.leftDimension(),
        state_site.physicalDimension(),
        state_site,
        normalized_state_site
    );
    return boost::move(normalized_state_site);
}}}

StateSite<Right> normalizeRight(StateSite<Middle> const& state_site) {{{
    StateSite<Right> normalized_state_site(dimensionsOf(state_site));
    Core::norm_for_right(
        state_site.rightDimension(),
        state_site.leftDimension(),
        state_site.physicalDimension(),
        state_site,
        normalized_state_site
    );
    return boost::move(normalized_state_site);
}}}

}
