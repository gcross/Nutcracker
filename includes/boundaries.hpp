//@+leo-ver=5-thin
//@+node:gcross.20110214183844.1813: * @file boundaries.hpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110220182654.2006: ** << License >>
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
//@+node:gcross.20110509141853.2168: ** << Documentation >>
/*!
\file boundaries.hpp
\brief Boundary-related functions
*/
//@-<< Documentation >>

#ifndef NUTCRACKER_BOUNDARIES_HPP
#define NUTCRACKER_BOUNDARIES_HPP

//@+<< Includes >>
//@+node:gcross.20110214155808.1853: ** << Includes >>
#include "tensors.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110214155808.1854: ** << Usings >>
//@-<< Usings >>

//! \defgroup BoundaryContractors Boundary contractors

//! @{

//@+others
//@+node:gcross.20110214155808.1856: ** Functions
//@+others
//@+node:gcross.20110509141853.2171: *3* computeExpectationValueAtSite
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
//@+node:gcross.20110509141853.2173: *3* contractExpectationBoundaries
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
//@+node:gcross.20110509141853.2172: *3* contractSOSLeft
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
//@+node:gcross.20110509141853.2176: *3* contractSOSRight
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
//@+node:gcross.20110509141853.2174: *3* contractSSLeft
//! Contracts the state and overlap site tensors into the left boundary.
/*!
\image html contractSSLeft.png
\image latex contractSSLeft.eps

\param left_boundary the left overlap boundary (L)
\param overlap_site the overlap site tensor (V)
\param state_site the state site tensor (S)
\returns the new left overlap boundary (L')
*/
Nutcracker::OverlapBoundary<Left> contractSSLeft(
      Nutcracker::OverlapBoundary<Left> const& old_boundary
    , Nutcracker::OverlapSite<Left> const& overlap_site
    , Nutcracker::StateSite<Left> const& state_site
);
//@+node:gcross.20110509141853.2175: *3* contractSSRight
//! Contracts the state and overlap site tensors into the right boundary.
/*!
\image html contractSSRight.png
\image latex contractSSRight.eps

\param right_boundary the right overlap boundary (R)
\param overlap_site the overlap site tensor (V)
\param state_site the state site tensor (S)
\returns the new right overlap boundary (R')
*/
Nutcracker::OverlapBoundary<Right> contractSSRight(
      Nutcracker::OverlapBoundary<Right> const& old_boundary
    , Nutcracker::OverlapSite<Right> const& overlap_site
    , Nutcracker::StateSite<Right> const& state_site
);
//@-others

//@+<< Unsafe >>
//@+node:gcross.20110215235924.2012: *3* << Unsafe >>
namespace Unsafe {

//! Unsafe version of Nutcracker::contractSOSLeft that ignores the site tensor normalization.
ExpectationBoundary<Left> contractSOSLeft(
      ExpectationBoundary<Left> const& old_boundary
    , StateSiteAny const& state_site
    , OperatorSite const& operator_site
);

//! Unsafe version of Nutcracker::contractSSLeft that ignores the site tensor normalization.
OverlapBoundary<Left> contractSSLeft(
      OverlapBoundary<Left> const& old_boundary
    , OverlapSiteAny const& overlap_site
    , StateSiteAny const& state_site
);

}
//@-<< Unsafe >>









//@+node:gcross.20110214155808.1858: ** struct contract
//! Type function that retrieves the boundary contractors associated with \c side.
/*!
\tparam side the side of the boundary being contracted
*/
template<typename side> struct contract {};

//! Left boundary contractors
template<> struct contract<Left> {

    //! Alias for contractSOSLeft().
    static Nutcracker::ExpectationBoundary<Left> SOS(
          Nutcracker::ExpectationBoundary<Left> const& old_boundary
        , Nutcracker::StateSite<Left> const& state_site
        , Nutcracker::OperatorSite const& operator_site
    ) { return contractSOSLeft(old_boundary,state_site,operator_site); }

    //! Alias for contractSSLeft().
    static Nutcracker::OverlapBoundary<Left> SS(
          Nutcracker::OverlapBoundary<Left> const& old_boundary
        , Nutcracker::OverlapSite<Left> const& overlap_site
        , Nutcracker::StateSite<Left> const& state_site
    ) { return contractSSLeft(old_boundary,overlap_site,state_site); }
};

//! Right boundary contractors
template<> struct contract<Right> {

    //! Alias for contractSOSRight().
    static Nutcracker::ExpectationBoundary<Right> SOS(
          Nutcracker::ExpectationBoundary<Right> const& old_boundary
        , Nutcracker::StateSite<Right> const& state_site
        , Nutcracker::OperatorSite const& operator_site
    ) { return contractSOSRight(old_boundary,state_site,operator_site); }

    //! Alias for contractSSRight().
    static Nutcracker::OverlapBoundary<Right> SS(
          Nutcracker::OverlapBoundary<Right> const& old_boundary
        , Nutcracker::OverlapSite<Right> const& overlap_site
        , Nutcracker::StateSite<Right> const& state_site
    ) { return contractSSRight(old_boundary,overlap_site,state_site); }
};
//@-others

//! @}

}

#endif
//@-leo
