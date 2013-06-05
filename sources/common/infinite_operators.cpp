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

InfiniteOperator constructHaldaneShastryModelInfiniteOperator() {{{
    using namespace Pauli;
    /*
    unsigned int const N = 6; // 100 terms
    double const
        A[N] = {0.50951664, -0.20231104,  0.20397574,  0.35912581,  0.01910218, 0.11059066},
        B[N] = {0.06454575,  0.97028356,  0.97028357,  0.34784606,  0.87545221, 0.66781956};
    */
    /*
    unsigned int const N = 6; // 1000 terms
    double const
        A[N] = {1.94645362e-04,   3.63208295e-01,   4.82332008e-01, 1.25279202e-01,   2.57554578e-02,   3.23038783e-03},
        B[N] = {0.99024014,  0.32257894,  0.05925048,  0.63222797,  0.8459558 , 0.95118551};
    */
    /*
    unsigned int const N = 9; // 100 terms
    double const
        A[N] = {-1.39798300e+01,   1.28034382e+01,   5.61535861e+00, -1.26781677e+01,   1.94601671e-04,  -5.61212883e+00, 4.82347734e-01,   1.40055820e+01,   3.63205362e-01},
        B[N] = { 0.84596776,  0.63229175,  0.95118618,  0.6322922 ,  0.99024125, 0.95118618,  0.0592535 ,  0.84596776,  0.32259284};
    */
    /*
    unsigned int const N = 9; // 1000 terms
    double const
        A[N] = {-9.73820003e+00,   9.60463447e+00,   2.50281627e-04, -6.69263635e+00,   9.07184922e+00,  -9.06757536e+00, 4.97380869e-01,   6.95763717e+00,   3.66659722e-01},
        B[N] = {0.7328277 ,  0.73084504,  0.98893763,  0.77498963,  0.94435163, 0.94435164,  0.06205325,  0.775321  ,  0.33845366};
    */
    unsigned int const N = 9; // 10000 terms
    double const
        A[N] = {6.18505736e-04,   3.56927507e-01,   7.04055807e-05, 1.77859581e-02,   3.78493975e-03,   6.54917336e-02, 1.83235170e-01,   4.09930918e-06,   3.72081681e-01},
        B[N] = {0.97613415,  0.22719877,  0.99279374,  0.85346561,  0.93626109, 0.70473391,  0.48229086,  0.99858369,  0.0402559};
    unsigned int const last = 2+3*N, Xoff = 2+0*N, Yoff = 2+1*N, Zoff = 2+2*N;
    vector<OperatorSiteLink> links;
    links.push_back(OperatorSiteLink(1,1,I));
    links.push_back(OperatorSiteLink(last,last,I));
    for(unsigned int i = 0; i < N; ++i) {
        links.push_back(OperatorSiteLink(1,Xoff+i,make_shared<Matrix const>(-A[i]*(*X))));
        links.push_back(OperatorSiteLink(1,Yoff+i,make_shared<Matrix const>(-A[i]*(*Y))));
        links.push_back(OperatorSiteLink(1,Zoff+i,make_shared<Matrix const>(-A[i]*(*Z))));

        MatrixConstPtr IB = make_shared<Matrix const>(B[i]*(*I));
        links.push_back(OperatorSiteLink(Xoff+i,Xoff+i,IB));
        links.push_back(OperatorSiteLink(Yoff+i,Yoff+i,IB));
        links.push_back(OperatorSiteLink(Zoff+i,Zoff+i,IB));

        links.push_back(OperatorSiteLink(Xoff+i,last,X));
        links.push_back(OperatorSiteLink(Yoff+i,last,Y));
        links.push_back(OperatorSiteLink(Zoff+i,last,Z));
    }
    vector<complex<double> > left_boundary(last,0), right_boundary(last,0);
    left_boundary[0] = 1;
    right_boundary[last-1] = 1;
    return InfiniteOperator(
        OperatorBoundary<Left>(fillWithRange(left_boundary)),
        constructOperatorSite(
            PhysicalDimension(2),
            LeftDimension(last),
            RightDimension(last),
            links
        ),
        OperatorBoundary<Right>(fillWithRange(right_boundary))
    );
}}}

}
