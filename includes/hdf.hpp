//@+leo-ver=5-thin
//@+node:gcross.20110511141322.2199: * @file hdf.hpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110511141322.2202: ** << License >>
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
//@+node:gcross.20110511141322.2204: ** << Documentation >>
/*!
\file hdf5.hpp
\brief HDF serialization functions
*/
//@-<< Documentation >>

#ifndef NUTCRACKER_HDF_HPP
#define NUTCRACKER_HDF_HPP

//@+<< Includes >>
//@+node:gcross.20110511141322.2205: ** << Includes >>
#include <hdf++/container.hpp>
#include <hdf++/dataset.hpp>
#include <hdf++/location.hpp>
#include <hdf++/group_array.hpp>
#include <vector>

#include "states.hpp"
#include "tensors.hpp"
#include "utilities.hpp"
//@-<< Includes >>

namespace Nutcracker { namespace HDF {

//@+<< Usings >>
//@+node:gcross.20110511141322.2206: ** << Usings >>
using ::HDF::createAt;
using ::HDF::Dataset;
using ::HDF::Group;
using ::HDF::GroupArray;
using ::HDF::Location;
//@-<< Usings >>

//! \defgroup HDF HDF serialization
//! @{

//@+others
//@+node:gcross.20110511150727.2222: ** Exceptions
//@+node:gcross.20110511190907.2313: *3* InconsistentTensorDimensions
struct InconsistentTensorDimensions : public Exception {
    InconsistentTensorDimensions() : Exception("The tensor dimensions are inconsistent.") {}
};
//@+node:gcross.20110511190907.2310: *3* WrongTensorNormalizationException
struct WrongTensorNormalizationException : public Exception {
    WrongTensorNormalizationException(optional<string> const& expected_normalization, optional<string> const& actual_normalization)
        : Exception(
            (format("Expected a tensor with %1% normalization but encountered a tensor with %2% normalization.")
                % (expected_normalization ? *expected_normalization : "unspecified")
                % (actual_normalization   ? *actual_normalization    : "unspecified")
            ).str())
    {}
};
//@+node:gcross.20110511190907.2271: *3* WrongTensorRankException
struct WrongTensorRankException : public Exception {
    WrongTensorRankException(unsigned int const expected_rank, unsigned int const actual_rank)
        : Exception((format("Expected a tensor with rank %1% but encountered a tensor with rank %2%.") % expected_rank % actual_rank).str())
    {}
};
//@-others

//! @}

} }

//@+<< Outside namespace >>
//@+node:gcross.20110524225044.2431: ** << Outside namespace >>
//@+others
//@+node:gcross.20110511190907.2294: *3* I/O Operators
//@+others
//@+node:gcross.20110511190907.2299: *4* OperatorSite
//! Write an object site tensor to an HDF object.
/*!
\param location the location of the object to write
\param operator_site_tensor the operator site tensor to write
\returns the id of the object that was created
*/
HDF::Group operator<<(HDF::Location const& location, Nutcracker::OperatorSite const& operator_site_tensor);

//! Read an object site tensor from an HDF object.
/*!
\param location the location of the object to write
\param operator_site_tensor the operator site tensor to be overrwritten with the read data
\returns the id of the object that was read
*/
HDF::Group operator>>(HDF::Location const& location, Nutcracker::OperatorSite& operator_site_tensor);
//@+node:gcross.20110511190907.2295: *4* StateSite
//@+node:gcross.20110511190907.2296: *5* <<
//! Write a state site tensor to an HDF object.
/*!
\note If the side tag is Left or Right then the "normalization" attribute will be set to respectively "left" or "right".

\tparam side the normalization of the state site tensor
\param location the location of the object to write
\param state_site_tensor the state site tensor to write
\returns the id of the object that was created
*/
template<typename side> HDF::Dataset operator<< (HDF::Location const& location, Nutcracker::StateSite<side> const& state_site_tensor) {
    ::HDF::Dataset dataset(
        createAt(location),
        HDF::rangeOf(state_site_tensor.dataDimensions()),
        state_site_tensor.begin()
    );
    dataset["normalization"] = Nutcracker::normalizationOf<side>::value;
    return dataset;
}
//@+node:gcross.20110511190907.2297: *5* >>
//! Read a state site tensor from an HDF object.
/*!
\note If the side tag is Left or Right then the "normalization" attribute of the object must be respectively "left" or "right".

\tparam side the normalization of the state site tensor
\param location the location of the object to write
\param state_site_tensor the state site tensor to be overritten with the read data
\returns the id of the object that was read
*/
template<typename side> HDF::Dataset operator>> (HDF::Location const& location, Nutcracker::StateSite<side>& state_site_tensor) {
    using namespace Nutcracker;
    using boost::optional;
    using std::string;

    ::HDF::Dataset dataset(location);

    optional<string> const& expected_normalization = Nutcracker::normalizationOf<side>::value;
    if(expected_normalization) {
        optional<string> actual_normalization = dataset["normalization"];
        if(expected_normalization != actual_normalization) {
            throw Nutcracker::HDF::WrongTensorNormalizationException(expected_normalization,actual_normalization);
        }
    }

    std::vector<hsize_t> dimensions = dataset.dimensionsWithAssertedRank(3);

    state_site_tensor =
        StateSite<side>(
            PhysicalDimension(dimensions[0]),
            LeftDimension(dimensions[1]),
            RightDimension(dimensions[2])
        );

    dataset.read(state_site_tensor.begin());

    return dataset;
}
//@+node:gcross.20110511190907.3510: *4* State
HDF::GroupArray operator<<(HDF::Location const& location, Nutcracker::State const& state);
HDF::GroupArray operator>>(HDF::Location const& location, Nutcracker::State& state);
//@+node:gcross.20110511190907.3573: *4* Operator
HDF::Container operator<<(HDF::Location const& location, Nutcracker::Operator const& operator_sites);
void operator>>(HDF::Location const& location, Nutcracker::Operator& operator_sites);
//@-others
//@-others
//@-<< Outside namespace >>

#endif
//@-leo
