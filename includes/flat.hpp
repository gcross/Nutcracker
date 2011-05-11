//@+leo-ver=5-thin
//@+node:gcross.20110510004855.2218: * @file flat.hpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110510004855.2252: ** << License >>
//@+at
// Copyright (c) 2011, Gregory Crosswhite
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
// 
//     * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//@@c
//@-<< License >>

//@+<< Documentation >>
//@+node:gcross.20110510004855.2220: ** << Documentation >>
/*!
\file flat.hpp
\brief Classes and functions relating to flat representations of states
*/
//@-<< Documentation >>

#ifndef NUTCRACKER_FLATTENING_HPP
#define NUTCRACKER_FLATTENING_HPP

//@+<< Includes >>
//@+node:gcross.20110510004855.2254: ** << Includes >>
#include <boost/bind.hpp>
#include <boost/concept_check.hpp>
#include <boost/numeric/ublas/vector.hpp>
#include <boost/range/adaptor/transformed.hpp>
#include <boost/range/concepts.hpp>

#include "core.hpp"
#include "tensors.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110510004855.2253: ** << Usings >>
using boost::adaptors::transformed;
using boost::SinglePassRangeConcept;
//@-<< Usings >>

//! \defgroup Flat Flat representations
//! @{

//@+others
//@+node:gcross.20110510004855.2269: ** Type aliases
//! The flat representation of quantum states
/* This is just a typedef to the vector class provided in the Boost.uBlas library. */
typedef boost::numeric::ublas::vector<complex<double> > StateVector;
//@+node:gcross.20110510004855.2249: ** Tensors
//@+node:gcross.20110510004855.2236: *3* class StateVectorFragment
//@+<< Description >>
//@+node:gcross.20110510004855.2237: *4* << Description >>
//! An intermediate result of constructing a flat state vector from a matrix product state.
/*!
\image html state_vector_fragment_tensor.png
\image latex state_vector_fragment_tensor.eps

This tensor represents one of the intermediate results of converting a matrix product state into the flat vector representation of a state.  It has two ranks:  the physical dimension which corresponds to the state space of the qubits that we have flattened so far, and a right dimension that connects this state vector fragment to the remaining qubits in the matrix product state.  When the matrix product state has been completely flattened the physical dimension will be equal to the dimension of the original Hilbert space and the right dimension will be equal to be one, and at that point this object can be casted to a StateVector without raising an error.

\note
See the documentation in BaseTensor for a description of the policy of how data ownership in tensors works.  (Short version: tensors own their data, which can be moved but not copied unless you explicitly ask for a copy to be made.)

\see BaseTensor
*/
//@-<< Description >>
class StateVectorFragment : public BaseTensor {
    //@+others
    //@+node:gcross.20110510004855.2238: *4* [Move support]
    private:

    BOOST_MOVABLE_BUT_NOT_COPYABLE(StateVectorFragment)
    //@+node:gcross.20110510004855.2239: *4* Assignment
    //! \name Assignment
    //! @{

    public:

    //! Moves the data (and dimensions) from \c other to \c this and invalidates \c other.
    StateVectorFragment& operator=(BOOST_RV_REF(StateVectorFragment) other) {
        if(this == &other) return *this;
        BaseTensor::operator=(boost::move(static_cast<BaseTensor&>(other)));
        physical_dimension = boost::move(other.physical_dimension);
        right_dimension = boost::move(other.right_dimension);
        return *this;
    }

    //! @}
    //@+node:gcross.20110510004855.2240: *4* Constructors
    //! \name Constructors
    //! @{

    public:

    //! Construct an invalid tensor (presumably into which you will eventually move data from elsewhere).
    StateVectorFragment() {}

    //! Move the data (and dimensions) from \c other into \c this and invalidate \c other.
    StateVectorFragment(BOOST_RV_REF(StateVectorFragment) other)
      : BaseTensor(boost::move(static_cast<BaseTensor&>(other)))
      , physical_dimension(boost::move(other.physical_dimension))
      , right_dimension(boost::move(other.right_dimension))
    { }

    //! Initialize the dimensions with the given values and allocate memory for an array of the appropriate size.
    StateVectorFragment(
          PhysicalDimension const physical_dimension
        , RightDimension const right_dimension
    ) : BaseTensor((*physical_dimension)*(*right_dimension))
      , physical_dimension(physical_dimension)
      , right_dimension(right_dimension)
    { }

    //! Constructs a tensor using data supplied from a generator.
    /*! \see BaseTensor( unsigned int const size, FillWithGenerator<G> const generator) */
    template<typename G> StateVectorFragment(
          PhysicalDimension const physical_dimension
        , RightDimension const right_dimension
        , FillWithGenerator<G> const generator
    ) : BaseTensor((*physical_dimension)*(*right_dimension),generator)
      , physical_dimension(physical_dimension)
      , right_dimension(right_dimension)
    { }

    //! Constructs a tensor using data supplied from a range.
    /*!
    \note The physical dimension is inferred automatically from the size of the range and the left and right dimensions.
    \see BaseTensor(FillWithRange<Range> const init)
    */
    template<typename Range> StateVectorFragment(
          PhysicalDimension const physical_dimension
        , FillWithRange<Range> const init
    ) : BaseTensor(init)
      , physical_dimension(physical_dimension)
      , right_dimension(size()/(*physical_dimension))
    { }

    //! Sets all dimensions to 1, and then allocates an array of size one and fills it with the value 1.
    StateVectorFragment(
          MakeTrivial const make_trivial
    ) : BaseTensor(make_trivial)
      , physical_dimension(1)
      , right_dimension(1)
    { }

    //! @}
    //@+node:gcross.20110510004855.2241: *4* Casts
    //! \name Casts

    //! @{

    public:

    //! Casts this tensor to a flat StateVector as long as the right dimension is 1.
    operator StateVector() const {
        assert(rightDimension(as_unsigned_integer) == 1);
        StateVector v(size());
        copy(*this,v.data().begin());
        return v;
    }

    //! @}
    //@+node:gcross.20110510004855.2242: *4* Dimension information
    //! \name Dimension information
    //! @{

    public:

    //! Returns the physical dimension of this tensor.
    PhysicalDimension physicalDimension() const { return physical_dimension; }
    //! Unwraps the physical dimension of this tensor and directly returns an unsigned integer.
    unsigned int physicalDimension(AsUnsignedInteger _) const { return *physical_dimension; }

    //! Returns the right dimension of this tensor.
    RightDimension rightDimension() const { return right_dimension; }
    //! Unwraps the right dimension of this tensor and directly returns an unsigned integer.
    unsigned int rightDimension(AsUnsignedInteger _) const { return *right_dimension; }

    //! @}
    //@+node:gcross.20110510004855.2243: *4* Fields
    private:

    //! The physical dimension of the site (which corresponds to the state space of the qubits that have been flattened).
    PhysicalDimension physical_dimension;

    //! The right dimension of this site.
    RightDimension right_dimension;
    //@+node:gcross.20110510004855.2244: *4* Miscellaneous
    public:

    //! Connects this tensor's right dimension of to the left dimension of \c state_site.
    /*! \see TensorConnectors */
    unsigned int operator|(StateSiteAny const& state_site) const {
        return connectDimensions(
             "fragment right"
            ,rightDimension(as_unsigned_integer)
            ,"state site left"
            ,state_site.leftDimension(as_unsigned_integer)
        );
    }

    //! The trivial state vector fragment tensor with all dimensions one and containing the single value 1.
    static StateVectorFragment const trivial;
    //@-others
};
//@+node:gcross.20110510004855.2235: ** Functions
//@+others
//@+node:gcross.20110510004855.2246: *3* computeStateVector
//@+<< Forward declaration of extendStateVectorFragment >>
//@+node:gcross.20110510004855.2268: *4* << Forward declaration of extendStateVectorFragment >>
StateVectorFragment extendStateVectorFragment(
      StateVectorFragment const& old_fragment
    , StateSiteAny const& state_site
);
//@-<< Forward declaration of extendStateVectorFragment >>

//! Computes a flat state vector from a matrx product state represented by a list of state site tensors.
/*!

\note Since this function necesarily has exponential running time, if you only need a few components of this vector then you should consider computeStateVectorComponent().

\tparam StateSiteRange the type of the list, which must satisfy the Boost single pass range concept with the value type \c StateSiteAny \c const.
\param state_sites the list of state site tensors
\returns the state vector
*/
template<typename StateSiteRange> StateVector computeStateVector(StateSiteRange const& state_sites) {
    BOOST_CONCEPT_ASSERT((SinglePassRangeConcept<StateSiteRange const>));
    StateVectorFragment current_fragment(make_trivial);
    BOOST_FOREACH(StateSiteAny const& state_site, state_sites) {
        StateVectorFragment next_fragment =
            extendStateVectorFragment(
                 current_fragment
                ,state_site
            );
        current_fragment = boost::move(next_fragment);
    }
    return current_fragment;
}
//@+node:gcross.20110510004855.2248: *3* computeStateVectorComponent
//! Computes a single component of a quantum state.
/*!
\note If you need the entire state vector then it is more efficient to call computeStateVector().

\tparam StateSiteRange the type of the list, which must satisfy the Boost single pass range concept with the value type \c StateSiteAny \c const.
\param state_sites the list of state site tensors
\param component the index of the desired component (in the flat vector representation of the state)
\returns the amplitude of the requested component
*/
template<typename StateSiteRange> complex<double> computeStateVectorComponent(StateSiteRange const& state_sites, unsigned long long const component) {
    using namespace boost;
    return computeStateComponent(state_sites,flatIndexToTensorIndex(state_sites | transformed(bind(&StateSiteAny::physicalDimension,_1,as_unsigned_integer)),component));
}
//@+node:gcross.20110510004855.2247: *3* computeStateVectorLength
//! Computes the number of components in the flat vector representation of the state.
/*!
\tparam StateSiteRange the type of the list, which must satisfy the Boost single pass range concept with the value type \c StateSiteAny \c const.
\param state_sites the list of state site tensors
\returns the number of components in the flat vector representation of the state
*/
template<typename StateSiteRange> unsigned long long computeStateVectorLength(StateSiteRange const& state_sites) {
    BOOST_CONCEPT_ASSERT((SinglePassRangeConcept<StateSiteRange const>));
    unsigned long long length = 1;
    BOOST_FOREACH(StateSiteAny const& state_site, state_sites) {
        length *= state_site.physicalDimension(as_unsigned_integer);
    }
    return length;
}
//@+node:gcross.20110510004855.2255: *3* extendStateVectorFragment
//! Extends a state vector fragement to include another state site tensor.
/*!
\image html extendStateVectorFragment.png
\image latex extendStateVectorFragment.eps

\param old_fragment the current state vector fragment (F')
\param state_site a state site tensor (S)
\returns a new state vector fragment that includes the given state site tensor (F')
*/
StateVectorFragment extendStateVectorFragment(
      StateVectorFragment const& old_fragment
    , StateSiteAny const& state_site
);
//@+node:gcross.20110510004855.2274: *3* flatIndexToTensorIndex
//! Converts an integral index of the flat representation of a tensor into the corresponding multi-index of the multi-dimensional representation of a tensor.
/*!
\note The conversion assumes that the tensor is stored in row-major order --- that is, an increment in the first entry of the multi index causes the greatest increase in the flat index and an increment in the last entry of the multi-index causes the least increase in the flast index.

\tparam DimensionRange the type of the list of the dimensions
\param dimensions the dimension of the tensor
\param flat_index the index into the flattened tensor
\return the multi-dimensional index referring to the same element in the original multi-dimensional tensor as \c flat_index had referred to in the flattened one-dimensional representation of that tensor.
*/
template<typename DimensionRange> vector<unsigned int> flatIndexToTensorIndex(DimensionRange const& dimensions, unsigned long long flat_index) {
    vector<unsigned int> tensor_index;
    tensor_index.reserve(dimensions.size());
    BOOST_FOREACH(unsigned int const dimension, dimensions | reversed) {
        tensor_index.push_back(flat_index % dimension);
        flat_index /= dimension;
    }
    assert(flat_index == 0);
    reverse(tensor_index);
    return boost::move(tensor_index);
}
//@+node:gcross.20110510004855.2275: *3* tensorIndexToFlatIndex
//! Converts a multi-index of the multi-dimensional representation of a tensor into the corresponding integral index of the flat representation of a tensor.
/*!
\note The conversion assumes that the tensor is stored in row-major order --- that is, an increment in the first entry of the multi index causes the greatest increase in the flat index and an increment in the last entry of the multi-index causes the least increase in the flast index.

\tparam DimensionRange the type of the list of the dimensions
\param dimensions the dimension of the tensor
\param tensor_index the index into the flattened tensor
\return the flat index referring to the same element in the flattened tensor as \c tensor_index had referred to in the original multi--dimensional representation of that tensor.
*/
template<typename DimensionRange> unsigned long long tensorIndexToFlatIndex(DimensionRange const& dimensions, vector<unsigned int> const& tensor_index) {
    assert(dimensions.size() == tensor_index.size());
    unsigned long long flat_index = 0;
    BOOST_FOREACH(unsigned int const i, irange(0u,(unsigned int)dimensions.size())) {
        assert(tensor_index[i] < dimensions[i]);
        flat_index *= dimensions[i];
        flat_index += tensor_index[i];
    }
    return flat_index;
}
//@-others
//@-others

//! @}

}

#endif
//@-leo