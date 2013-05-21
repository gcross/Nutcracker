// Includes {{{
#include <boost/assign/list_of.hpp>

#include "nutcracker/infinite_operators.hpp"
#include "nutcracker/operators.hpp"
// }}}

namespace Nutcracker {

// Usings {{{
using boost::assign::list_of;
// }}}

InfiniteOperator constructExternalFieldInfiniteOperator(MatrixConstPtr const& matrix) { // {{{
    PhysicalDimension const physical_dimension(matrix->size1());
    assert(*physical_dimension == matrix->size2());
    MatrixConstPtr I = identityMatrix(*physical_dimension);
    return InfiniteOperator(
        OperatorBoundary<Left>(fillWithRange(list_of(1)(0))),
        constructOperatorSite(
            physical_dimension,
            LeftDimension(2),
            RightDimension(2),
            list_of
                (OperatorSiteLink(1,1,I))
                (OperatorSiteLink(1,2,matrix))
                (OperatorSiteLink(2,2,I))
        ),
        OperatorBoundary<Right>(fillWithRange(list_of(0)(1)))
    );
} // }}}

}
