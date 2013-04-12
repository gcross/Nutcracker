/*!
\file utilities.cpp
\brief Utility classes and functions
*/

#include <boost/range/numeric.hpp>
#include <boost/range/irange.hpp>
#include <numeric>
#include <string>

#include "nutcracker/operators.hpp"
#include "nutcracker/utilities.hpp"

namespace Nutcracker {

using boost::accumulate;
using boost::irange;

using std::ceil;
using std::make_pair;
using std::multiplies;

unsigned long long choose(unsigned int const n, unsigned int const k) {
    assert(k <= n);
    return accumulate(irange((n-k)+1,n+1),1,multiplies<unsigned long long>()) / accumulate(irange(1u,k+1),1,multiplies<unsigned long long>());
}
vector<unsigned int> computeBandwidthDimensionSequence(
    unsigned int const requested_bandwidth_dimension
   ,vector<unsigned int> const& physical_dimensions
) {
    unsigned int const middle_index = (physical_dimensions.size()+1)/2;

    vector<unsigned int> forward_bandwidth_dimensions(1,1);
    BOOST_FOREACH(
         unsigned int const d
        ,make_pair(
             makeProductIterator(physical_dimensions.begin())
            ,makeProductIterator(physical_dimensions.begin()+middle_index)
         )
    ) {
        if(d >= requested_bandwidth_dimension) break;
        forward_bandwidth_dimensions.push_back(d);
    }

    vector<unsigned int> reverse_bandwidth_dimensions(1,1);
    BOOST_FOREACH(
         unsigned int const d
        ,make_pair(
             makeProductIterator(physical_dimensions.rbegin())
            ,makeProductIterator(physical_dimensions.rbegin()+middle_index)
         )
    ) {
        if(d >= requested_bandwidth_dimension) break;
        reverse_bandwidth_dimensions.push_back(d);
    }

    if(forward_bandwidth_dimensions.size() == middle_index+1
    || reverse_bandwidth_dimensions.size() == middle_index+1
    ) {
        throw RequestedBandwidthDimensionTooLargeError(
                 requested_bandwidth_dimension
                ,min(forward_bandwidth_dimensions.size() == middle_index+1 ? forward_bandwidth_dimensions.back() : numeric_limits<unsigned int>::max()
                    ,reverse_bandwidth_dimensions.size() == middle_index+1 ? reverse_bandwidth_dimensions.back() : numeric_limits<unsigned int>::max()
                    )
        );
    }

    fill_n(
         back_inserter(forward_bandwidth_dimensions)
        ,physical_dimensions.size()+1
            - forward_bandwidth_dimensions.size()
            - reverse_bandwidth_dimensions.size()
        ,requested_bandwidth_dimension
    );

    reverse_copy(
         reverse_bandwidth_dimensions
        ,back_inserter(forward_bandwidth_dimensions)
    );

    return boost::move(forward_bandwidth_dimensions);
}
unsigned int computeDigitsOfPrecision(double const tolerance) {
    double const estimated_precision = ceil(-log10(tolerance));
    return estimated_precision > 0 ? (unsigned int)estimated_precision : 1;
}
MatrixPtr identityMatrix(unsigned int const n) {
    Matrix* I = new Matrix(n,n,c(0,0));
    BOOST_FOREACH(unsigned int const i, irange(0u,n)) {
        (*I)(i,i) = 1;
    }
    return MatrixPtr(I);
}
namespace Pauli {
    MatrixConstPtr const
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

namespace Qubit {
    VectorConstPtr const
         Up = basisVector(2,0)
        ,Down = basisVector(2,1)
        ;
}

}
