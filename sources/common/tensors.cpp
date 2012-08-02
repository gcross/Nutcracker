//@+leo-ver=5-thin
//@+node:gcross.20110215235924.1990: * @file tensors.cpp
//@@language cplusplus
//@+<< Includes >>
//@+node:gcross.20110215235924.1991: ** << Includes >>
#include <boost/lambda/lambda.hpp>
#include <boost/range/algorithm/transform.hpp>
#include <map>
#include <yaml-cpp/yaml.h>

#include "nutcracker/tensors.hpp"
#include "nutcracker/utilities.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110215235924.1992: ** << Usings >>
namespace lambda = boost::lambda;

using std::make_pair;
using std::map;
//@-<< Usings >>

//@+others
//@+node:gcross.20110215235924.2000: ** Values
OperatorSite const OperatorSite::trivial(make_trivial);

DEFINE_DUMMY_PARAMETER(AsDimension,as_dimension)
DEFINE_DUMMY_PARAMETER(MakeTrivial,make_trivial)

optional<string> const normalizationOf<Left>::value("left");
optional<string> const normalizationOf<Middle>::value("middle");
optional<string> const normalizationOf<Right>::value("right");
optional<string> const normalizationOf<None>::value(none);
//@+node:gcross.20110827234144.2621: ** Classes
//@+node:gcross.20110827234144.2622: *3* StateSiteAny
//@+node:gcross.20110827234144.2623: *4* normalize
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
//@-others

}
//@-leo
