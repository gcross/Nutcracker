//@+leo-ver=5-thin
//@+node:gcross.20110124161335.2009: * @thin tensors.hpp
//@@language cplusplus

#ifndef NUTCRACKER_TENSORS_HPP
#define NUTCRACKER_TENSORS_HPP

//@+<< Includes >>
//@+node:gcross.20110124161335.2010: ** << Includes >>
#include <boost/assign/list_of.hpp>
#include <boost/concept_check.hpp>
#include <boost/format.hpp>
#include <boost/range/algorithm/copy.hpp>
#include <boost/range/concepts.hpp>
#include <boost/smart_ptr/scoped_array.hpp>
#include <boost/utility.hpp>
#include <complex>
#include <exception>
#include <stdint.h>
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110124161335.2011: ** << Usings >>
using namespace boost;
using namespace boost::assign;
using namespace std;
//@-<< Usings >>

//@+others
//@+node:gcross.20110124175241.1520: ** enum Side
enum Side { Left, Right, Middle };
//@+node:gcross.20110125120748.1516: ** struct DimensionsOf
extern struct DimensionsOf {} const dimensionsOf;
//@+node:gcross.20110124161335.2012: ** Classes
//@+node:gcross.20110126150230.1601: *3* BaseTensor
struct BaseTensor : public noncopyable {
    unsigned int const size;
protected:
    scoped_array<complex<double> > data;
public:

    BaseTensor() : size(0), data(0) { };

    BaseTensor(unsigned int const size)
      : size(size)
      , data(new complex<double>[size])
    { }

    template<typename Range> BaseTensor(Range const& init)
      : size(init.size())
      , data(new complex<double>[size])
    {
        BOOST_CONCEPT_ASSERT(( RandomAccessRangeConcept<Range> ));
        copy(init,data.get());
    }

    operator complex<double>*() { return data.get(); }
    operator complex<double> const*() const { return data.get(); }

    complex<double>* begin() { return data.get(); }
    complex<double> const* begin() const { return data.get(); }

    complex<double>* end() { return data.get()+size; }
    complex<double> const* end() const { return data.get()+size; }
};
//@+node:gcross.20110124175241.1530: *3* Boundary
//@+node:gcross.20110124161335.2013: *4* ExpectationBoundary
template<Side side> struct ExpectationBoundary : public BaseTensor {
    unsigned int const state_dimension, operator_dimension;

    ExpectationBoundary(
          unsigned int const state_dimension
        , unsigned int const operator_dimension
    ) : BaseTensor(state_dimension*state_dimension*operator_dimension)
      , state_dimension(state_dimension)
      , operator_dimension(operator_dimension)
    { }

    template<typename Range> ExpectationBoundary(
          unsigned int const state_dimension
        , Range const& init
    ) : BaseTensor(init)
      , state_dimension(state_dimension)
      , operator_dimension(size/(state_dimension*state_dimension))
    { }

    static ExpectationBoundary const trivial;
};

template<Side side> ExpectationBoundary<side> const ExpectationBoundary<side>::trivial(1,list_of(1));
//@+node:gcross.20110124175241.1526: *4* OverlapBoundary
template<Side side> struct OverlapBoundary : public BaseTensor {
    unsigned int const state_dimension;

    OverlapBoundary(
          unsigned int const state_dimension
    ) : BaseTensor(state_dimension*state_dimension)
      , state_dimension(state_dimension)
    { }

    template<typename Range> OverlapBoundary(
          Range const& init
    ) : BaseTensor(init)
      , state_dimension((unsigned int)sqrt(size))
    { }

    static OverlapBoundary const trivial;
};

template<Side side> OverlapBoundary<side> const OverlapBoundary<side>::trivial(list_of(1));
//@+node:gcross.20110124175241.1538: *3* ProjectorMatrix
class ProjectorMatrix {
    scoped_array<complex<double> > reflector_data, coefficient_data;
    scoped_array<uint32_t> swap_data;
public:
    unsigned int const
          number_of_projectors
        , projector_length
        , number_of_reflectors
        , subspace_dimension
        ;
    ProjectorMatrix()
      : reflector_data(0)
      , coefficient_data(0)
      , swap_data(0)
      , number_of_projectors(0)
      , projector_length(0)
      , number_of_reflectors(0)
      , subspace_dimension(0)
    { }

    ProjectorMatrix(
          unsigned int const number_of_projectors
        , unsigned int const projector_length
        , unsigned int const number_of_reflectors
        , unsigned int const subspace_dimension
        , complex<double>* reflector_data
        , complex<double>* coefficient_data
        , uint32_t* swap_data
    ) : reflector_data(reflector_data)
      , coefficient_data(coefficient_data)
      , swap_data(swap_data)
      , number_of_projectors(number_of_projectors)
      , projector_length(projector_length)
      , number_of_reflectors(number_of_reflectors)
      , subspace_dimension(subspace_dimension)
    { }

    complex<double>* reflectorData() { return reflector_data.get(); }
    complex<double> const* reflectorData() const { return reflector_data.get(); }

    complex<double>* coefficientData() { return coefficient_data.get(); }
    complex<double> const* coefficientData() const { return coefficient_data.get(); }

    uint32_t* swapData() { return swap_data.get(); }
    uint32_t const* swapData() const { return swap_data.get(); }

    operator bool() const { return number_of_projectors != 0; }

};
//@+node:gcross.20110124175241.1531: *3* Site
//@+node:gcross.20110124175241.1533: *4* OperatorSite
struct OperatorSite : public BaseTensor {
    unsigned int const
          number_of_matrices
        , physical_dimension
        , left_dimension
        , right_dimension
        ;
protected:
    scoped_array<uint32_t> index_data;
public:

    OperatorSite(
          unsigned int const number_of_matrices
        , unsigned int const physical_dimension
        , unsigned int const left_dimension
        , unsigned int const right_dimension
    ) : BaseTensor(number_of_matrices*physical_dimension*physical_dimension)
      , number_of_matrices(number_of_matrices)
      , physical_dimension(physical_dimension)
      , left_dimension(left_dimension)
      , right_dimension(right_dimension)
      , index_data(new uint32_t[number_of_matrices*2])
    { }

    template<typename Range1, typename Range2> OperatorSite(
          unsigned int const left_dimension
        , unsigned int const right_dimension
        , Range1 const& matrix_init
        , Range2 const& index_init
    ) : BaseTensor(matrix_init)
      , number_of_matrices(index_init.size()/2)
      , physical_dimension((unsigned int)sqrt(size/number_of_matrices))
      , left_dimension(left_dimension)
      , right_dimension(right_dimension)
      , index_data(new uint32_t[index_init.size()])
    {
        BOOST_CONCEPT_ASSERT(( RandomAccessRangeConcept<Range2> ));
        copy(index_init,index_data.get());
    }

    operator uint32_t*() { return index_data.get(); }
    operator uint32_t const*() const { return index_data.get(); }

    static OperatorSite const trivial;
};
//@+node:gcross.20110124175241.1535: *4* StateSite
template<Side side> struct StateSite : public BaseTensor {
    unsigned int const
          physical_dimension
        , left_dimension
        , right_dimension
        ;

    StateSite(
          unsigned int const physical_dimension
        , unsigned int const left_dimension
        , unsigned int const right_dimension
    ) : BaseTensor(physical_dimension*left_dimension*right_dimension)
      , physical_dimension(physical_dimension)
      , left_dimension(left_dimension)
      , right_dimension(right_dimension)
    { }

    template<typename Range> StateSite(
          unsigned int const left_dimension
        , unsigned int const right_dimension
        , Range const& init
    ) : BaseTensor(init)
      , physical_dimension(size/(left_dimension*right_dimension))
      , left_dimension(left_dimension)
      , right_dimension(right_dimension)
    { }

    template<Side other_side> StateSite(
          DimensionsOf const _
        , StateSite<other_side> const& other_site
    ) : BaseTensor(
             other_site.physical_dimension
            *other_site.left_dimension
            *other_site.right_dimension
        )
      , physical_dimension(other_site.physical_dimension)
      , left_dimension(other_site.left_dimension)
      , right_dimension(other_site.right_dimension)
    { }

    static StateSite const trivial;
};

template<Side side> StateSite<side> const StateSite<side>::trivial(1,1,list_of(1));
//@+node:gcross.20110124175241.1537: *4* OverlapSite
template<Side side> struct OverlapSite : public BaseTensor {
    unsigned int const
          physical_dimension
        , left_dimension
        , right_dimension
        ;
    OverlapSite(
          unsigned int const physical_dimension
        , unsigned int const left_dimension
        , unsigned int const right_dimension
    ) : BaseTensor(physical_dimension*left_dimension*right_dimension)
      , physical_dimension(physical_dimension)
      , left_dimension(left_dimension)
      , right_dimension(right_dimension)
    { }

    template<typename Range> OverlapSite(
          unsigned int const left_dimension
        , unsigned int const right_dimension
        , Range const& init
    ) : BaseTensor(init)
      , physical_dimension(size/(left_dimension*right_dimension))
      , left_dimension(left_dimension)
      , right_dimension(right_dimension)
    { }

    template<Side other_side> OverlapSite(
          DimensionsOf const _
        , StateSite<other_side> const& other_site
    ) : BaseTensor(
             other_site.physical_dimension
            *other_site.left_dimension
            *other_site.right_dimension
        )
      , physical_dimension(other_site.physical_dimension)
      , left_dimension(other_site.left_dimension)
      , right_dimension(other_site.right_dimension)
    { }

    static OverlapSite const trivial;
};

template<Side side> OverlapSite<side> const OverlapSite<side>::trivial(1,1,list_of(1));
//@+node:gcross.20110126102637.2192: *3* OverlapVectorTrio
struct OverlapVectorTrio {
    shared_ptr<OverlapBoundary<Left> const> left_boundary;
    shared_ptr<OverlapBoundary<Right> const> right_boundary;
    shared_ptr<OverlapSite<Middle> const> middle_site;
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
        ,expectation_boundary.operator_dimension
        ,"operator site left"
        ,operator_site.left_dimension
    );
}
//@+node:gcross.20110125120748.2441: *4* ExpectationBoundary<Left> || StateSite<Middle>
inline unsigned int operator||(
      ExpectationBoundary<Left> const& expectation_boundary
    , StateSite<Middle> const& state_site
) {
    return connectDimensions(
         "left expectation boundary state"
        ,expectation_boundary.state_dimension
        ,"middle state site left"
        ,state_site.left_dimension
    );
}
//@+node:gcross.20110125120748.2451: *4* OperatorSite || ExpectationBoundary<Right>
inline unsigned int operator||(
      OperatorSite const& operator_site
    , ExpectationBoundary<Right> const& expectation_boundary
) {
    return connectDimensions(
         "operator site right"
        ,operator_site.right_dimension
        ,"right expectation boundary state"
        ,expectation_boundary.operator_dimension
    );
}
//@+node:gcross.20110125120748.2457: *4* OperatorSite || StateSite<Middle>
template<Side side> inline unsigned int operator||(
      OperatorSite const& operator_site
    , StateSite<side> const& state_site
) {
    return connectDimensions(
         "operator site physical"
        ,operator_site.physical_dimension
        ,"middle state site physical"
        ,state_site.physical_dimension
    );
}
//@+node:gcross.20110125120748.2445: *4* OverlapBoundary<Left> || StateSite<Middle>
inline unsigned int operator||(
      OverlapBoundary<Left> const& overlap_boundary
    , StateSite<Middle> const& state_site
) {
    return connectDimensions(
         "left overlap boundary state"
        ,overlap_boundary.state_dimension
        ,"middle state site left"
        ,state_site.left_dimension
    );
}
//@+node:gcross.20110125120748.2453: *4* OverlapSite<Middle> || OperatorSite
template<Side side> inline unsigned int operator||(
      OverlapSite<side> const& overlap_site
    , StateSite<side> const& state_site
) {
    return connectDimensions(
         "overlap site physical"
        ,overlap_site.physical_dimension
        ,"state site physical"
        ,state_site.physical_dimension
    );
}
//@+node:gcross.20110126102637.2200: *4* ProjectorMatrix || StateSite<Middle>
inline unsigned int operator||(
      ProjectorMatrix const& projector_matrix
    , StateSite<Middle> const& state_site
) {
    return connectDimensions(
         "state site size"
        ,state_site.size
        ,"projector length"
        ,projector_matrix.projector_length
    );
}
//@+node:gcross.20110125120748.2437: *4* StateSite<Left> || StateSite<Middle>
inline unsigned int operator||(
      StateSite<Left> const& state_site_1
    , StateSite<Middle> const& state_site_2
) {
    return connectDimensions(
         "left state site right"
        ,state_site_1.right_dimension
        ,"middle state site left"
        ,state_site_2.left_dimension
    );
}
//@+node:gcross.20110125120748.2443: *4* StateSite<Middle> || ExpectationBoundary<Left>
inline unsigned int operator||(
      StateSite<Middle> const& state_site
    , ExpectationBoundary<Right> const& expectation_boundary
) {
    return connectDimensions(
         "middle state site right"
        ,state_site.right_dimension
        ,"right expectation boundary state"
        ,expectation_boundary.state_dimension
    );
}
//@+node:gcross.20110125120748.2447: *4* StateSite<Middle> || OverlapBoundary<Right>
inline unsigned int operator||(
      StateSite<Middle> const& state_site
    , OverlapBoundary<Right> const& overlap_boundary
) {
    return connectDimensions(
         "middle state site right"
        ,state_site.right_dimension
        ,"right overlap boundary state"
        ,overlap_boundary.state_dimension
    );
}
//@+node:gcross.20110125120748.2436: *4* StateSite<Middle> || StateSite<Right>
inline unsigned int operator||(
      StateSite<Middle> const& state_site_1
    , StateSite<Right> const& state_site_2
) {
    return connectDimensions(
         "middle state site right"
        ,state_site_1.right_dimension
        ,"right state site left"
        ,state_site_2.left_dimension
    );
}
//@-others

}

#endif
//@-leo
