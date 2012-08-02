//@+leo-ver=5-thin
//@+node:gcross.20110511141322.2199: * @file hdf.hpp
//@@language cplusplus
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

#include "nutcracker/states.hpp"
#include "nutcracker/tensors.hpp"
#include "nutcracker/utilities.hpp"
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
struct InconsistentTensorDimensions : public std::runtime_error {
    InconsistentTensorDimensions() : std::runtime_error("The tensor dimensions are inconsistent.") {}
};
//@+node:gcross.20110511190907.2271: *3* WrongTensorRankException
struct WrongTensorRankException : public std::runtime_error {
    WrongTensorRankException(unsigned int const expected_rank, unsigned int const actual_rank)
        : std::runtime_error((format("Expected a tensor with rank %1% but encountered a tensor with rank %2%.") % expected_rank % actual_rank).str())
    {}
};
//@-others

//! @}

} }

//@+<< Outside namespace >>
//@+node:gcross.20110524225044.2431: ** << Outside namespace >>
namespace HDF {
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
            throw Nutcracker::WrongTensorNormalizationException(expected_normalization,actual_normalization);
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
}
//@-<< Outside namespace >>

#endif
//@-leo
