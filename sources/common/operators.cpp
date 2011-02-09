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
//@+node:gcross.20110207115918.1781: *3* constructExternalFieldOperators
Operators constructExternalFieldOperators(
      unsigned int const number_of_operators
    , Matrix const& matrix
) {
    assert(number_of_operators > 0);
    PhysicalDimension const physical_dimension(matrix.size1());
    assert(*physical_dimension == matrix.size2());
    Operators operators;
    operators.reserve(number_of_operators);
    if(number_of_operators == 1) {
        operators.push_back(shared_ptr<OperatorSite const>(new OperatorSite(
            constructOperatorSite(
                 physical_dimension
                ,LeftDimension(1)
                ,RightDimension(1)
                ,list_of(OperatorLink(1,1,matrix))
            )
        )));
    } else {
        Matrix I = identityMatrix(*physical_dimension);
        operators.push_back(shared_ptr<OperatorSite const>(new OperatorSite(
            constructOperatorSite(
                 physical_dimension
                ,LeftDimension(1)
                ,RightDimension(2)
                ,list_of
                    (OperatorLink(1,1,I))
                    (OperatorLink(1,2,matrix))
            )
        )));
        shared_ptr<OperatorSite const> const middle(new OperatorSite(
            constructOperatorSite(
                 physical_dimension
                ,LeftDimension(2)
                ,RightDimension(2)
                ,list_of
                    (OperatorLink(1,1,I))
                    (OperatorLink(1,2,matrix))
                    (OperatorLink(2,2,I))
            )
        ));
        REPEAT(number_of_operators-2) {
            operators.push_back(middle);
        }
        operators.push_back(shared_ptr<OperatorSite const>(new OperatorSite(
            constructOperatorSite(
                 physical_dimension
                ,LeftDimension(2)
                ,RightDimension(1)
                ,list_of
                    (OperatorLink(1,1,matrix))
                    (OperatorLink(2,1,I))
            )
        )));
    }
    return boost::move(operators);
}
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
//@+node:gcross.20110208195213.1789: *3* constructTransverseIsingModelOperators
Operators constructTransverseIsingModelOperators(
      unsigned int const number_of_operators
    , double spin_coupling_strength
) {
    using namespace Pauli;
    assert(number_of_operators > 1);
    Matrix const
          &X1 = X
        ,  X2 = spin_coupling_strength*X
        ;
    Operators operators;
    operators.reserve(number_of_operators);
    operators.push_back(shared_ptr<OperatorSite const>(new OperatorSite(
        constructOperatorSite(
             PhysicalDimension(2)
            ,LeftDimension(1)
            ,RightDimension(3)
            ,list_of
                (OperatorLink(1,1,I ))
                (OperatorLink(1,2,X1))
                (OperatorLink(1,3,Z ))
        )
    )));
    shared_ptr<OperatorSite const> const middle(new OperatorSite(
        constructOperatorSite(
             PhysicalDimension(2)
            ,LeftDimension(3)
            ,RightDimension(3)
            ,list_of
                (OperatorLink(1,1,I ))
                (OperatorLink(1,2,X1))
                (OperatorLink(2,3,X2))
                (OperatorLink(1,3,Z ))
                (OperatorLink(3,3,I ))
        )
    ));
    REPEAT(number_of_operators-2) {
        operators.push_back(middle);
    }
    operators.push_back(shared_ptr<OperatorSite const>(new OperatorSite(
        constructOperatorSite(
             PhysicalDimension(2)
            ,LeftDimension(3)
            ,RightDimension(1)
            ,list_of
                (OperatorLink(1,1,Z ))
                (OperatorLink(2,1,X2))
                (OperatorLink(3,1,I ))
        )
    )));
    return boost::move(operators);
}
//@+node:gcross.20110207120702.1780: *3* identityMatrix
Matrix identityMatrix(unsigned int const n) {
    Matrix I(n,n);
    I.clear();
    BOOST_FOREACH(unsigned int const i, irange(0u,n)) {
        I(i,i) = 1;
    }
    return I;
}
//@-others

}
//@-leo
