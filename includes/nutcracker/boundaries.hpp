/*!
\file boundaries.hpp
\brief Functions related to boundaries.
*/

#ifndef NUTCRACKER_BOUNDARIES_HPP
#define NUTCRACKER_BOUNDARIES_HPP

#include "nutcracker/core.hpp"
#include "nutcracker/tensors.hpp"

namespace Nutcracker {


// Boundary contractors {{{
//! \defgroup BoundaryContractors Boundary contractors

//! @{

// computeExpectationValueAtSite {{{
//! Computes the expectation value at a given site
/*!
\image html expectation.png
\image latex expectation.eps

\param left_boundary the left expectation boundary (L)
\param state_site the state site tensor (S)
\param operator_site the operator site tensor (O)
\param right_boundary the right expectation boundary (R)
\param the expectation value
*/
std::complex<double> computeExpectationValueAtSite(
      Nutcracker::ExpectationBoundary<Left> const& left_boundary
    , Nutcracker::StateSite<Middle> const& state_site
    , Nutcracker::OperatorSite const& operator_site
    , Nutcracker::ExpectationBoundary<Right> const& right_boundary
);
// }}}
// contractExpectationBoundaries {{{
//! Contracts the left and right expectation boundaries to form the final expecation value.
/*!
\image html contractExpectationBoundaries.png
\image latex contractExpectationBoundaries.eps

\param left_boundary the left expectation boundary (L)
\param right_boundary the right expectation boundary (R)
\param the expectation value
*/
std::complex<double> contractExpectationBoundaries(
      Nutcracker::ExpectationBoundary<Left> const& left_boundary
    , Nutcracker::ExpectationBoundary<Right> const& right_boundary
);
// }}}
// contractSOSLeft {{{
//! Contracts the state and operator site tensors into the left boundary.
/*!
\image html contractSOSLeft.png
\image latex contractSOSLeft.eps

\param left_boundary the left expectation boundary (L)
\param state_site the state site tensor (S)
\param operator_site the operator site tensor (O)
\returns the new left expectation boundary (L')
*/
Nutcracker::ExpectationBoundary<Left> contractSOSLeft(
      Nutcracker::ExpectationBoundary<Left> const& old_boundary
    , Nutcracker::StateSite<Left> const& state_site
    , Nutcracker::OperatorSite const& operator_site
);
// }}}
// contractSOSRight {{{
//! Contracts the state and operator site tensors into the right boundary.
/*!
\image html contractSOSRight.png
\image latex contractSOSRight.eps

\param right_boundary the right expectation boundary (R)
\param state_site the state site tensor (S)
\param operator_site the operator site tensor (O)
\returns the new right expectation boundary (R')
*/
Nutcracker::ExpectationBoundary<Right> contractSOSRight(
      Nutcracker::ExpectationBoundary<Right> const& old_boundary
    , Nutcracker::StateSite<Right> const& state_site
    , Nutcracker::OperatorSite const& operator_site
);
// }}}
// contractVSLeft {{{
//! Contracts the state and overlap site tensors into the left boundary.
/*!
\image html contractVSLeft.png
\image latex contractVSLeft.eps

\param left_boundary the left overlap boundary (L)
\param overlap_site the overlap site tensor (V)
\param state_site the state site tensor (S)
\returns the new left overlap boundary (L')
*/
Nutcracker::OverlapBoundary<Left> contractVSLeft(
      Nutcracker::OverlapBoundary<Left> const& old_boundary
    , Nutcracker::OverlapSite<Left> const& overlap_site
    , Nutcracker::StateSite<Left> const& state_site
);
// }}}
// contractVSRight {{{
//! Contracts the state and overlap site tensors into the right boundary.
/*!
\image html contractVSRight.png
\image latex contractVSRight.eps

\param right_boundary the right overlap boundary (R)
\param overlap_site the overlap site tensor (V)
\param state_site the state site tensor (S)
\returns the new right overlap boundary (R')
*/
Nutcracker::OverlapBoundary<Right> contractVSRight(
      Nutcracker::OverlapBoundary<Right> const& old_boundary
    , Nutcracker::OverlapSite<Right> const& overlap_site
    , Nutcracker::StateSite<Right> const& state_site
);
// }}}

namespace Unsafe { // {{{

// contractSOSLeft {{{
//! Unsafe version of Nutcracker::contractSOSLeft that ignores the site tensor normalization.
ExpectationBoundary<Left> contractSOSLeft(
      ExpectationBoundary<Left> const& old_boundary
    , StateSiteAny const& state_site
    , OperatorSite const& operator_site
);
// }}}

// contractSOSRight {{{
//! Unsafe version of Nutcracker::contractSOSRight that ignores the site tensor normalization.
ExpectationBoundary<Right> contractSOSRight(
      ExpectationBoundary<Right> const& old_boundary
    , StateSiteAny const& state_site
    , OperatorSite const& operator_site
);
// }}}

// contractVSLeft {{{
//! Unsafe version of Nutcracker::contractVSLeft that ignores the site tensor normalization.
OverlapBoundary<Left> contractVSLeft(
      OverlapBoundary<Left> const& old_boundary
    , OverlapSiteAny const& overlap_site
    , StateSiteAny const& state_site
);
// }}}

} // }}}

// contract utility structures {{{
//! Type function that retrieves the boundary contractors associated with \c side.
/*!
\tparam side the side of the boundary being contracted
*/
template<typename side> struct contract {};

//! Left boundary contractors
template<> struct contract<Left> { // {{{
    // SOS {{{
    //! Alias for contractSOSLeft().
    static Nutcracker::ExpectationBoundary<Left> SOS(
          Nutcracker::ExpectationBoundary<Left> const& old_boundary
        , Nutcracker::StateSite<Left> const& state_site
        , Nutcracker::OperatorSite const& operator_site
    ) { return contractSOSLeft(old_boundary,state_site,operator_site); }
    // }}}
    // SOS_absorb {{{
    static Nutcracker::ExpectationBoundary<Left> SOS_absorb(
          Nutcracker::ExpectationBoundary<Left> const& old_boundary
        , Nutcracker::StateSite<Middle> const& state_site
        , Nutcracker::OperatorSite const& operator_site
    ) { return Unsafe::contractSOSLeft(old_boundary,state_site,operator_site); }
    // }}}
    // VS {{{
    //! Alias for contractVSLeft().
    static Nutcracker::OverlapBoundary<Left> VS(
          Nutcracker::OverlapBoundary<Left> const& old_boundary
        , Nutcracker::OverlapSite<Left> const& overlap_site
        , Nutcracker::StateSite<Left> const& state_site
    ) { return contractVSLeft(old_boundary,overlap_site,state_site); }
    // }}}
};
// }}}

//! Right boundary contractors
template<> struct contract<Right> { // {{{
    // SOS {{{
    //! Alias for contractSOSRight().
    static Nutcracker::ExpectationBoundary<Right> SOS(
          Nutcracker::ExpectationBoundary<Right> const& old_boundary
        , Nutcracker::StateSite<Right> const& state_site
        , Nutcracker::OperatorSite const& operator_site
    ) { return contractSOSRight(old_boundary,state_site,operator_site); }
    // }}}
    // SOS_absorb {{{
    static Nutcracker::ExpectationBoundary<Right> SOS_absorb(
          Nutcracker::ExpectationBoundary<Right> const& old_boundary
        , Nutcracker::StateSite<Middle> const& state_site
        , Nutcracker::OperatorSite const& operator_site
    ) { return Unsafe::contractSOSRight(old_boundary,state_site,operator_site); }
    // }}}
    // VS {{{
    //! Alias for contractVSRight().
    static Nutcracker::OverlapBoundary<Right> VS(
          Nutcracker::OverlapBoundary<Right> const& old_boundary
        , Nutcracker::OverlapSite<Right> const& overlap_site
        , Nutcracker::StateSite<Right> const& state_site
    ) { return contractVSRight(old_boundary,overlap_site,state_site); }
    // }}}
};
// }}}
// }}}

//! @}
// }}}

// Boundary constructors {{{
//! \defgroup BoundaryConstructors Boundary constructors
//! {@

// constructExpectationBoundary {{{
// Helper template function {{{
template<typename side> struct construct_exp_boundary {};
template<> struct construct_exp_boundary<Left> { // {{{
    static void call(
        uint32_t const b, uint32_t const c,
        complex<double> const* state_boundary,
        complex<double> const* operator_boundary,
        complex<double>* left_expectation_boundary
    ) {
        return Core::construct_left_exp_boundary(
            b,c,
            state_boundary,
            operator_boundary,
            left_expectation_boundary
        );
    }
}; // }}}
template<> struct construct_exp_boundary<Right> { // {{{
    static void call(
        uint32_t const b, uint32_t const c,
        complex<double> const* state_boundary,
        complex<double> const* operator_boundary,
        complex<double>* right_expectation_boundary
    ) {
        return Core::construct_right_exp_boundary(
            b,c,
            state_boundary,
            operator_boundary,
            right_expectation_boundary
        );
    }
}; // }}}
// }}}
template<typename side> ExpectationBoundary<side> constructExpectationBoundary( // {{{
    StateBoundary<side> const& state_boundary,
    OperatorBoundary<side> const& operator_boundary
) {
    ExpectationBoundary<side> expectation_boundary(
        operator_boundary.operatorDimension(as_dimension),
        state_boundary.stateDimension(as_dimension)
    );

    construct_exp_boundary<side>::call(
        state_boundary.stateDimension(),operator_boundary.operatorDimension(),
        state_boundary,
        operator_boundary,
        expectation_boundary
    );

    return boost::move(expectation_boundary);
} // }}}
//! @}
// }}}

}

#endif
