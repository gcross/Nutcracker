//@+leo-ver=5-thin
//@+node:gcross.20110214155808.1852: * @thin boundaries.hpp
//@@language cplusplus

#ifndef NUTCRACKER_BOUNDARIES_HPP
#define NUTCRACKER_BOUNDARIES_HPP

//@+<< Includes >>
//@+node:gcross.20110214155808.1853: ** << Includes >>
#include "operators.hpp"
#include "states.hpp"
#include "tensors.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110214155808.1854: ** << Usings >>
//@-<< Usings >>

//@+others
//@+node:gcross.20110214164734.1943: ** Tensors
//@+node:gcross.20110214164734.1944: *3* ExpectationBoundary
template<typename side> class ExpectationBoundary : public BaseTensor {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(ExpectationBoundary)
    OperatorDimension operator_dimension;
    StateDimension state_dimension;
public:
    ExpectationBoundary() {}

    ExpectationBoundary(BOOST_RV_REF(ExpectationBoundary) other)
      : BaseTensor(boost::move(static_cast<BaseTensor&>(other)))
      , operator_dimension(boost::move(other.operator_dimension))
      , state_dimension(boost::move(other.state_dimension))
    { }

    ExpectationBoundary(
          OperatorDimension const operator_dimension
        , StateDimension const state_dimension
    ) : BaseTensor((*operator_dimension)*(*state_dimension)*(*state_dimension))
      , operator_dimension(operator_dimension)
      , state_dimension(state_dimension)
    { }

    template<typename other_side> ExpectationBoundary(
          CopyFrom<ExpectationBoundary<other_side> const> const other
    ) : BaseTensor(other)
      , operator_dimension(other->operator_dimension)
      , state_dimension(other->state_dimension)
    { }

    template<typename G> ExpectationBoundary(
          OperatorDimension const operator_dimension
        , StateDimension const state_dimension
        , FillWithGenerator<G> const generator
    ) : BaseTensor((*operator_dimension)*(*state_dimension)*(*state_dimension),generator)
      , operator_dimension(operator_dimension)
      , state_dimension(state_dimension)
    { }

    template<typename Range> ExpectationBoundary(
          OperatorDimension const operator_dimension
        , FillWithRange<Range> const init
    ) : BaseTensor(init)
      , operator_dimension(operator_dimension)
      , state_dimension((unsigned int)sqrt(size()/(*operator_dimension)))
    { }

    ExpectationBoundary(
          MakeTrivial const make_trivial
    ) : BaseTensor(make_trivial)
      , operator_dimension(1)
      , state_dimension(1)
    { }

    ExpectationBoundary& operator=(BOOST_RV_REF(ExpectationBoundary) other) {
        if(this == &other) return *this;
        BaseTensor::operator=(boost::move(static_cast<BaseTensor&>(other)));
        operator_dimension = boost::move(other.operator_dimension);
        state_dimension = boost::move(other.state_dimension);
        return *this;
    }

    void swap(ExpectationBoundary& other) {
        if(this == &other) return;
        BaseTensor::swap(other);
        std::swap(operator_dimension,other.operator_dimension);
        std::swap(state_dimension,other.state_dimension);
    }

    OperatorDimension operatorDimension() const { return operator_dimension; }
    unsigned int operatorDimension(AsUnsignedInteger _) const { return *operator_dimension; }

    StateDimension stateDimension() const { return state_dimension; }
    unsigned int stateDimension(AsUnsignedInteger _) const { return *state_dimension; }

    static ExpectationBoundary const trivial;
};

template<typename side> ExpectationBoundary<side> const ExpectationBoundary<side>::trivial(make_trivial);
//@+node:gcross.20110214164734.1945: *3* OverlapBoundary
template<typename side> class OverlapBoundary : public BaseTensor {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(OverlapBoundary)
    OverlapDimension overlap_dimension;
    StateDimension state_dimension;
public:
    OverlapBoundary() {}

    OverlapBoundary(BOOST_RV_REF(OverlapBoundary) other)
      : BaseTensor(boost::move(static_cast<BaseTensor&>(other)))
      , overlap_dimension(boost::move(other.overlap_dimension))
      , state_dimension(boost::move(other.state_dimension))
    { }

    OverlapBoundary(
          OverlapDimension const overlap_dimension
        , StateDimension const state_dimension
    ) : BaseTensor((*overlap_dimension)*(*state_dimension))
      , overlap_dimension(overlap_dimension)
      , state_dimension(state_dimension)
    { }

    template<typename other_side> OverlapBoundary(
          CopyFrom<OverlapBoundary<other_side> const> const other
    ) : BaseTensor(other)
      , overlap_dimension(other->overlap_dimension)
      , state_dimension(other->state_dimension)
    { }

    template<typename G> OverlapBoundary(
          OverlapDimension const overlap_dimension
        , StateDimension const state_dimension
        , FillWithGenerator<G> const generator
    ) : BaseTensor((*overlap_dimension)*(*state_dimension),generator)
      , overlap_dimension(overlap_dimension)
      , state_dimension(state_dimension)
    { }

    template<typename Range> OverlapBoundary(
          OverlapDimension const overlap_dimension
        , FillWithRange<Range> const init
    ) : BaseTensor(init)
      , overlap_dimension(overlap_dimension)
      , state_dimension(size()/(*overlap_dimension))
    { }

    OverlapBoundary(
          MakeTrivial const make_trivial
    ) : BaseTensor(make_trivial)
      , overlap_dimension(1)
      , state_dimension(1)
    { }

    OverlapBoundary& operator=(BOOST_RV_REF(OverlapBoundary) other) {
        if(this == &other) return *this;
        BaseTensor::operator=(boost::move(static_cast<BaseTensor&>(other)));
        overlap_dimension = boost::move(other.overlap_dimension);
        state_dimension = boost::move(other.state_dimension);
        return *this;
    }

    void swap(OverlapBoundary& other) {
        if(this == &other) return;
        BaseTensor::swap(other);
        std::swap(overlap_dimension,other.overlap_dimension);
        std::swap(state_dimension,other.state_dimension);
    }

    OverlapDimension overlapDimension() const { return overlap_dimension; }
    unsigned int overlapDimension(AsUnsignedInteger _) const { return *overlap_dimension; }

    StateDimension stateDimension() const { return state_dimension; }
    unsigned int stateDimension(AsUnsignedInteger _) const { return *state_dimension; }

    static OverlapBoundary const trivial;
};

template<typename side> OverlapBoundary<side> const OverlapBoundary<side>::trivial(make_trivial);
//@+node:gcross.20110214155808.1856: ** Functions
complex<double> computeExpectationValue(
      ExpectationBoundary<Left> const& left_boundary
    , StateSite<Middle> const& state_site
    , OperatorSite const& operator_site
    , ExpectationBoundary<Right> const& right_boundary
);

complex<double> contractExpectationBoundaries(
      ExpectationBoundary<Left> const& left_boundary
    , ExpectationBoundary<Right> const& right_boundary
);

ExpectationBoundary<Left> contractSOSLeft(
      ExpectationBoundary<Left> const& old_boundary
    , StateSite<Left> const& state_site
    , OperatorSite const& operator_site
);

ExpectationBoundary<Right> contractSOSRight(
      ExpectationBoundary<Right> const& old_boundary
    , StateSite<Right> const& state_site
    , OperatorSite const& operator_site
);

OverlapBoundary<Left> contractSSLeft(
      OverlapBoundary<Left> const& old_boundary
    , OverlapSite<Left> const& overlap_site
    , StateSite<Left> const& state_site
);

OverlapBoundary<Right> contractSSRight(
      OverlapBoundary<Right> const& old_boundary
    , OverlapSite<Right> const& overlap_site
    , StateSite<Right> const& state_site
);
//@+node:gcross.20110214155808.1858: ** struct contract
template<typename side> struct contract {};

template<> struct contract<Left> {
    static ExpectationBoundary<Left> SOS(
          ExpectationBoundary<Left> const& old_boundary
        , StateSite<Left> const& state_site
        , OperatorSite const& operator_site
    ) { return contractSOSLeft(old_boundary,state_site,operator_site); }

    static OverlapBoundary<Left> SS(
          OverlapBoundary<Left> const& old_boundary
        , OverlapSite<Left> const& overlap_site
        , StateSite<Left> const& state_site
    ) { return contractSSLeft(old_boundary,overlap_site,state_site); }
};

template<> struct contract<Right> {
    static ExpectationBoundary<Right> SOS(
          ExpectationBoundary<Right> const& old_boundary
        , StateSite<Right> const& state_site
        , OperatorSite const& operator_site
    ) { return contractSOSRight(old_boundary,state_site,operator_site); }

    static OverlapBoundary<Right> SS(
          OverlapBoundary<Right> const& old_boundary
        , OverlapSite<Right> const& overlap_site
        , StateSite<Right> const& state_site
    ) { return contractSSRight(old_boundary,overlap_site,state_site); }
};
//@-others

}

#endif
//@-leo
