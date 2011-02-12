//@+leo-ver=5-thin
//@+node:gcross.20110206185121.1758: * @thin operators.hpp
//@@language cplusplus

#ifndef NUTCRACKER_OPERATORS_HPP
#define NUTCRACKER_OPERATORS_HPP

//@+<< Includes >>
//@+node:gcross.20110206185121.1759: ** << Includes >>
#include <boost/assign/list_of.hpp>
#include <boost/container/vector.hpp>
#include <boost/move/move.hpp>
#include <boost/numeric/ublas/matrix.hpp>
#include <boost/range/algorithm/reverse_copy.hpp>
#include <boost/smart_ptr/shared_ptr.hpp>
#include <complex>
#include <vector>

#include "tensors.hpp"
#include "utilities.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110206185121.1760: ** << Usings >>
using boost::assign::list_of;
using boost::container::vector;
using boost::copy;
using boost::numeric::ublas::matrix;
using boost::reverse_copy;
using boost::shared_ptr;
//@-<< Usings >>

//@+others
//@+node:gcross.20110207005827.1775: ** Typedefs
typedef matrix<complex<double> > Matrix;
typedef vector<shared_ptr<OperatorSite const> > Operators;
//@+node:gcross.20110206185121.1761: ** struct OperatorLink
struct OperatorLink {
    unsigned int from, to;
    Matrix matrix;

    OperatorLink(
          unsigned int const from
        , unsigned int const to
        , Matrix const& matrix
    ) : from(from)
      , to(to)
      , matrix(matrix)
    { }

    OperatorLink(OperatorLink const& other)
      : from(other.from)
      , to(other.to)
      , matrix(other.matrix)
    { }

    OperatorLink(BOOST_RV_REF(OperatorLink) other)
      : from(copyAndReset(other.from))
      , to(copyAndReset(other.to))
      , matrix()
    { matrix.swap(other.matrix); }

    OperatorLink& operator=(OperatorLink const& other) {
        from = other.from;
        to = other.to;
        matrix = other.matrix;
        return *this;
    }    

    OperatorLink& operator=(BOOST_RV_REF(OperatorLink) other) {
        from = copyAndReset(other.from);
        to = copyAndReset(other.to);
        matrix.resize(0,false);
        matrix.swap(other.matrix);
        return *this;
    }
};
//@+node:gcross.20110206185121.1771: ** Functions
OperatorSite constructOperatorSite(
      PhysicalDimension const physical_dimension
    , LeftDimension const left_dimension
    , RightDimension const right_dimension
    , vector<OperatorLink> const& links
);

Operators constructExternalFieldOperators(
      unsigned int const number_of_operators
    , Matrix const& matrix
);

Operators constructTransverseIsingModelOperators(
      unsigned int const number_of_operators
    , double spin_coupling_strength
);

Matrix identityMatrix(unsigned int const n);

//@+others
//@+node:gcross.20110207005827.1774: *3* diagonalMatrix
template<typename T> Matrix diagonalMatrix(T const& data) {
    unsigned int const n = data.size();
    Matrix matrix(n,n);
    matrix.clear();
    unsigned int i = 0; 
    typename T::const_iterator px = data.begin();
    while(px != data.end()) { matrix(i,i) = *px; ++i; ++px; }
    return matrix;
}
//@+node:gcross.20110207005827.1771: *3* squareMatrix
template<typename T> Matrix squareMatrix(T const& data) {
    unsigned int const n = (unsigned int)sqrt(data.size());
    assert(n*n == data.size());
    Matrix matrix(n,n);
    copy(data,matrix.data().begin());
    return matrix;
}
//@-others
//@+node:gcross.20110207005827.1772: ** Values
namespace Pauli {
    Matrix const
         I = identityMatrix(2)
        ,X = squareMatrix(list_of
                (0)(1)
                (1)(0)
             )
        ,Y = squareMatrix(list_of
                (c(0,0))(c(0,-1))
                (c(0,1))(c(0,0))
             )
        ,Z = diagonalMatrix(list_of(1)(-1))
        ;
}
//@-others

}

#endif
//@-leo