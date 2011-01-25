//@+leo-ver=5-thin
//@+node:gcross.20110124161335.2009: * @thin tensors.hpp
//@@language cplusplus

#ifndef NUTCRACKER_TENSORS_HPP
#define NUTCRACKER_TENSORS_HPP

//@+<< Includes >>
//@+node:gcross.20110124161335.2010: ** << Includes >>
#include <blitz/array.h>
#include <boost/format.hpp>
#include <complex>
#include <exception>
#include <stdint.h>
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110124161335.2011: ** << Usings >>
using namespace blitz;
using namespace boost;
using namespace std;
//@-<< Usings >>

//@+others
//@+node:gcross.20110124175241.1520: ** enum Side
enum Side { Left, Right, Middle };
//@+node:gcross.20110125120748.1516: ** struct DimensionsOf
struct DimensionsOf {} dimensionsOf;
//@+node:gcross.20110124161335.2012: ** Classes
//@+node:gcross.20110124175241.1530: *3* Boundary
//@+node:gcross.20110124161335.2013: *4* ExpectationBoundary
template<Side side> class ExpectationBoundary {
    Array<complex<double>,3> data;
public:
    ExpectationBoundary(
          unsigned int const state_dimension
        , unsigned int const operator_dimension
    ) : data(operator_dimension
            ,state_dimension
            ,state_dimension
        )
    { }

    unsigned int operatorDimension() const { return data.extent(0); }
    unsigned int stateDimension() const { return data.extent(1); }

    operator complex<double>*() { return data.data(); }
    operator complex<double> const*() const { return data.data(); }
};
//@+node:gcross.20110124175241.1526: *4* OverlapBoundary
template<Side side> class OverlapBoundary {
    Array<complex<double>,2> data;
public:
    OverlapBoundary(
          unsigned int const state_dimension
    ) : data(state_dimension
            ,state_dimension
        )
    { }

    unsigned int stateDimension() const { return data.extent(0); }

    operator complex<double>*() { return data.data(); }
    operator complex<double> const*() const { return data.data(); }
};
//@+node:gcross.20110126102637.2192: *3* OverlapVectorTrio
struct OverlapVectorTrio {
    OverlapBoundary<Left> const left_boundary;
    OverlapBoundary<Right> const right_boundary;
    OverlapSite<Middle> const middle_site;
};
//@+node:gcross.20110124175241.1538: *3* ProjectorMatrix
class ProjectorMatrix {
    Array<complex<double>,2> reflector_data;
    Array<complex<double>,1> coefficient_data;
    Array<uint32_t,1> swap_data;
    unsigned int const subspace_dimension;
public:
    ProjectorMatrix()
      : reflector_data(0,0)
      , coefficient_data(0)
      , swap_data(0)
      , subspace_dimension(0)
    { }

    ProjectorMatrix(
          Array<complex<double>,2> const& reflector_data
        , Array<complex<double>,1> const& coefficient_data
        , Array<uint32_t,1> const& swap_data
        , unsigned int const subspace_dimension
    ) : reflector_data(reflector_data)
      , coefficient_data(coefficient_data)
      , swap_data(swap_data)
      , subspace_dimension(subspace_dimension)
    { }

    unsigned int numberOfProjectors() const { return reflector_data.extent(0); }
    unsigned int projectorLength() const { return reflector_data.extent(1); }
    unsigned int numberOfReflectors() const { return coefficient_data.extent(0); }
    unsigned int subspaceDimension() const { return subspace_dimension; }

    complex<double>* reflectorData() { return reflector_data.data(); }
    complex<double> const* reflectorData() const { return reflector_data.data(); }

    complex<double>* coefficientData() { return coefficient_data.data(); }
    complex<double> const* coefficientData() const { return coefficient_data.data(); }

    uint32_t* swapData() { return swap_data.data(); }
    uint32_t const* swapData() const { return swap_data.data(); }

    operator bool() const { return numberOfProjectors() != 0; }

};
//@+node:gcross.20110124175241.1531: *3* Site
//@+node:gcross.20110124175241.1533: *4* OperatorSite
class OperatorSite {
    unsigned int const left_dimension, right_dimension;
    Array<uint32_t,2> index_data;
    Array<complex<double>,3> matrix_data;
public:
    OperatorSite(
          unsigned int const number_of_matrices
        , unsigned int const physical_dimension
        , unsigned int const left_dimension
        , unsigned int const right_dimension
    ) : left_dimension(left_dimension)
      , right_dimension(right_dimension)
      , index_data(number_of_matrices,2)
      , matrix_data(
             number_of_matrices
            ,physical_dimension
            ,physical_dimension
        )
    { }

    unsigned int physicalDimension() const { return matrix_data.extent(1); }
    unsigned int leftDimension() const { return left_dimension; }
    unsigned int rightDimension() const { return right_dimension; }
    unsigned int numberOfMatrices() const { return matrix_data.extent(0); }

    operator complex<double>*() { return matrix_data.data(); }
    operator complex<double> const*() const { return matrix_data.data(); }

    operator uint32_t*() { return index_data.data(); }
    operator uint32_t const*() const { return index_data.data(); }
};
//@+node:gcross.20110124175241.1537: *4* OverlapSite
template<Side side> class OverlapSite {
    Array<complex<double>,3> data;
public:
    OverlapSite(
          unsigned int const physical_dimension
        , unsigned int const left_dimension
        , unsigned int const right_dimension
    ) : data(right_dimension
            ,physical_dimension
            ,left_dimension
        )
    { }

    template<Side other_side> OverlapSite(
          DimensionsOf const _
        , StateSite<other_side> const& state_site
    ) : data(state_site.physicalDimension()
            ,state_site.leftDimension()
            ,state_site.rightDimension()
        )
    { }

    unsigned int physicalDimension() const { return data.extent(1); }
    unsigned int leftDimension() const { return data.extent(2); }
    unsigned int rightDimension() const { return data.extent(0); }

    operator complex<double>*() { return data.data(); }
    operator complex<double> const*() const { return data.data(); }
};
//@+node:gcross.20110124175241.1535: *4* StateSite
template<Side side> class StateSite {
    Array<complex<double>,3> data;
public:
    StateSite(
          unsigned int const physical_dimension
        , unsigned int const left_dimension
        , unsigned int const right_dimension
    ) : data(physical_dimension
            ,left_dimension
            ,right_dimension
        )
    { }

    template<Side other_side> StateSite(
          const DimensionsOf _
        , StateSite<other_side> other_state_site
    ) : data(other_state_site.physicalDimension()
            ,other_state_site.leftDimension()
            ,other_state_site.rightDimension()
        )
    { }

    unsigned int physicalDimension() const { return data.extent(0); }
    unsigned int leftDimension() const { return data.extent(1); }
    unsigned int rightDimension() const { return data.extent(2); }

    unsigned int flatDimension() const { return data.size(); }

    operator complex<double>*() { return data.data(); }
    operator complex<double> const*() const { return data.data(); }
};
//@+node:gcross.20110125120748.2431: ** Connectors
//@+node:gcross.20110125120748.2434: *3* exception DimensionMismatch
struct DimensionMismatch : public std::exception {
    const string message;

    DimensionMismatch(
          const char* n1
        , unsigned int const d1
        , const char* n2
        , unsigned int const d2
    ) : message((format("%1% dimension (%2%) does not match %3% dimension (%4%)") % n1 % d1 % n2 % d2).str())
    { }

    virtual const char* what() { return message.c_str(); }

    virtual ~DimensionMismatch() throw() { }
};
//@+node:gcross.20110125120748.2435: *3* function connectDimensions
inline unsigned int connectDimensions(
      const char* n1
    , unsigned int const d1
    , const char* n2
    , unsigned int const d2
) {
    if(d1 != d2) throw DimensionMismatch(n1,d1,n2,d2);
    return d1;
}
//@+node:gcross.20110125120748.2439: *3* operator||
//@+node:gcross.20110125120748.2449: *4* ExpectationBoundary<Left> || OperatorSite
inline unsigned int operator||(
      ExpectationBoundary<Left> const& expectation_boundary
    , OperatorSite const& operator_site
) {
    return connectDimensions(
         "left expectation boundary state"
        ,expectation_boundary.operatorDimension()
        ,"operator site left"
        ,operator_site.leftDimension()
    );
}
//@+node:gcross.20110125120748.2441: *4* ExpectationBoundary<Left> || StateSite<Middle>
inline unsigned int operator||(
      ExpectationBoundary<Left> const& expectation_boundary
    , StateSite<Middle> const& state_site
) {
    return connectDimensions(
         "left expectation boundary state"
        ,expectation_boundary.stateDimension()
        ,"middle state site left"
        ,state_site.leftDimension()
    );
}
//@+node:gcross.20110125120748.2451: *4* OperatorSite || ExpectationBoundary<Right>
inline unsigned int operator||(
      OperatorSite const& operator_site
    , ExpectationBoundary<Right> const& expectation_boundary
) {
    return connectDimensions(
         "operator site right"
        ,operator_site.rightDimension()
        ,"right expectation boundary state"
        ,expectation_boundary.operatorDimension()
    );
}
//@+node:gcross.20110125120748.2457: *4* OperatorSite || StateSite<Middle>
template<Side side> inline unsigned int operator||(
      OperatorSite const& operator_site
    , StateSite<side> const& state_site
) {
    return connectDimensions(
         "operator site physical"
        ,operator_site.physicalDimension()
        ,"middle state site physical"
        ,state_site.physicalDimension()
    );
}
//@+node:gcross.20110125120748.2445: *4* OverlapBoundary<Left> || StateSite<Middle>
inline unsigned int operator||(
      OverlapBoundary<Left> const& overlap_boundary
    , StateSite<Middle> const& state_site
) {
    return connectDimensions(
         "left overlap boundary state"
        ,overlap_boundary.stateDimension()
        ,"middle state site left"
        ,state_site.leftDimension()
    );
}
//@+node:gcross.20110125120748.2453: *4* OverlapSite<Middle> || OperatorSite
template<Side side> inline unsigned int operator||(
      OverlapSite<side> const& overlap_site
    , StateSite<side> const& state_site
) {
    return connectDimensions(
         "overlap site physical"
        ,overlap_site.physicalDimension()
        ,"state site physical"
        ,state_site.physicalDimension()
    );
}

//@+node:gcross.20110126102637.2200: *4* ProjectorMatrix || StateSite<Middle>
inline unsigned int operator||(
      ProjectorMatrix const& projector_matrix
    , StateSite<Middle> const& state_site
) {
    return connectDimensions(
         "state site flat"
        ,state_site.flatDimension()
        ,"projector length"
        ,projector_matrix.projectorLength()
    );
}
//@+node:gcross.20110125120748.2437: *4* StateSite<Left> || StateSite<Middle>
inline unsigned int operator||(
      StateSite<Left> const& state_site_1
    , StateSite<Middle> const& state_site_2
) {
    return connectDimensions(
         "left state site right"
        ,state_site_1.rightDimension()
        ,"middle state site left"
        ,state_site_2.leftDimension()
    );
}
//@+node:gcross.20110125120748.2443: *4* StateSite<Middle> || ExpectationBoundary<Left>
inline unsigned int operator||(
      StateSite<Middle> const& state_site
    , ExpectationBoundary<Right> const& expectation_boundary
) {
    return connectDimensions(
         "middle state site right"
        ,state_site.rightDimension()
        ,"right expectation boundary state"
        ,expectation_boundary.stateDimension()
    );
}
//@+node:gcross.20110125120748.2447: *4* StateSite<Middle> || OverlapBoundary<Right>
inline unsigned int operator||(
      StateSite<Middle> const& state_site
    , OverlapBoundary<Right> const& overlap_boundary
) {
    return connectDimensions(
         "middle state site right"
        ,state_site.rightDimension()
        ,"right overlap boundary state"
        ,overlap_boundary.stateDimension()
    );
}
//@+node:gcross.20110125120748.2436: *4* StateSite<Middle> || StateSite<Right>
inline unsigned int operator||(
      StateSite<Middle> const& state_site_1
    , StateSite<Right> const& state_site_2
) {
    return connectDimensions(
         "middle state site right"
        ,state_site_1.rightDimension()
        ,"right state site left"
        ,state_site_2.leftDimension()
    );
}
//@-others

}

#endif
//@-leo
