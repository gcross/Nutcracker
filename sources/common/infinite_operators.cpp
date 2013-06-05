// Includes {{{
#include <boost/assign/list_of.hpp>
#include <boost/make_shared.hpp>

#include "nutcracker/infinite_operators.hpp"
#include "nutcracker/operators.hpp"
// }}}

namespace Nutcracker {

// Usings {{{
using boost::assign::list_of;

using boost::make_shared;
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

InfiniteOperator constructTransverseIsingModelInfiniteOperator(double spin_coupling_strength) {{{
    using namespace Pauli;
    MatrixConstPtr const
          &X1 = X
        ,  X2 = make_shared<Matrix const>(-spin_coupling_strength*(*X))
        ;
    return InfiniteOperator(
        OperatorBoundary<Left>(fillWithRange(list_of(1)(0)(0))),
        constructOperatorSite(
            PhysicalDimension(2),
            LeftDimension(3),
            RightDimension(3),
            list_of
                (OperatorSiteLink(1,1,I ))
                (OperatorSiteLink(1,2,X1))
                (OperatorSiteLink(2,3,X2))
                (OperatorSiteLink(1,3,Z ))
                (OperatorSiteLink(3,3,I ))
        ),
        OperatorBoundary<Right>(fillWithRange(list_of(0)(0)(1)))
    );
}}}

InfiniteOperator constructXYModelInfiniteOperator(double spin_coupling_strength) {{{
    using namespace Pauli;
    MatrixConstPtr const
          &X1 = X
        ,  X2 = make_shared<Matrix const>(-(*X))
        , &Y1 = Y
        ,  Y2 = make_shared<Matrix const>(-spin_coupling_strength*(*Y))
        ;
    return InfiniteOperator(
        OperatorBoundary<Left>(fillWithRange(list_of(1)(0)(0)(0))),
        constructOperatorSite(
            PhysicalDimension(2),
            LeftDimension(4),
            RightDimension(4),
            list_of
                (OperatorSiteLink(1,1,I ))
                (OperatorSiteLink(1,2,X1))
                (OperatorSiteLink(2,4,X2))
                (OperatorSiteLink(1,3,Y1))
                (OperatorSiteLink(3,4,Y2))
                (OperatorSiteLink(4,4,I ))
        ),
        OperatorBoundary<Right>(fillWithRange(list_of(0)(0)(0)(1)))
    );
}}}

}
