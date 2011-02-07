//@+leo-ver=5-thin
//@+node:gcross.20110206185121.1777: * @thin operators.cpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110206185121.1778: ** << Includes >>
#include <boost/range/algorithm/copy.hpp>
#include <stdint.h>

#include "operators.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110206185121.1779: ** << Usings >>
using namespace boost;
//@-<< Usings >>

//@+others
//@+node:gcross.20110206185121.1783: ** Functions
//@+node:gcross.20110206185121.1784: *3* constructOperatorSite
OperatorSite constructOperatorSite(
      PhysicalDimension const physical_dimension
    , LeftDimension const left_dimension
    , RightDimension const right_dimension
    , vector<OperatorLink> const& links
) {
    assert(links.size() > 0);

    OperatorSite operator_site
        (links.size()
        ,physical_dimension
        ,left_dimension
        ,right_dimension
        );
    uint32_t* index_data = operator_site;
    complex<double>* matrix_data = operator_site;

    BOOST_FOREACH(
         OperatorLink const& link
        ,links
    ) {
        *index_data++ = link.from;
        *index_data++ = link.to;
        assert(link.matrix.size1() == *physical_dimension);
        assert(link.matrix.size2() == *physical_dimension);
        matrix_data = copy(link.matrix.data(),matrix_data);
    }

    return boost::move(operator_site);
}
//@-others

}
//@-leo
