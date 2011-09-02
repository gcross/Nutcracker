//@+leo-ver=5-thin
//@+node:gcross.20110901221152.2671: * @file protobuf.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110901221152.2672: ** << License >>
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

//@+<< Includes >>
//@+node:gcross.20110901221152.2673: ** << Includes >>
#include "protobuf.hpp"
#include "states.hpp"
//@-<< Includes >>

//@+<< Usings >>
//@+node:gcross.20110901221152.2674: ** << Usings >>
using boost::container::vector;
using boost::optional;
//@-<< Usings >>

//@+others
//@+node:gcross.20110901221152.2675: ** I/O Operators
//@+node:gcross.20110902105950.2696: *3* OperatorSite
void operator<<(Nutcracker::Protobuf::OperatorSite& buffer, Nutcracker::OperatorSite const& tensor) {
    using namespace Nutcracker;
    buffer.set_number_of_matrices(tensor.numberOfMatrices());
    buffer.set_physical_dimension(tensor.physicalDimension());
    buffer.set_left_dimension(tensor.leftDimension());
    buffer.set_right_dimension(tensor.rightDimension());
    google::protobuf::RepeatedField<double>& matrix_data = *buffer.mutable_matrix_data();
    matrix_data.Clear();
    matrix_data.Reserve(2*tensor.size());
    BOOST_FOREACH(complex<double> const& x, tensor) {
        (*matrix_data.AddAlreadyReserved()) = x.real();
        (*matrix_data.AddAlreadyReserved()) = x.imag();
    }
    google::protobuf::RepeatedField<uint32_t>& index_data = *buffer.mutable_index_data();
    index_data.Clear();
    index_data.Reserve(2*tensor.size());
    uint32_t const *ptr = static_cast<uint32_t const*>(tensor), *end = ptr + 2*tensor.numberOfMatrices();
    while(ptr != end) {
        (*index_data.Add()) = *ptr++;
    }
}

void operator>>(Nutcracker::Protobuf::OperatorSite const& buffer, Nutcracker::OperatorSite& tensor) {
    using namespace Nutcracker;
    unsigned int const number_of_matrices = buffer.number_of_matrices();
    PhysicalDimension const physical_dimension(buffer.physical_dimension());
    LeftDimension     const left_dimension    (buffer.left_dimension());
    RightDimension    const right_dimension   (buffer.right_dimension());
    OperatorSite operator_site_tensor(number_of_matrices,physical_dimension,left_dimension,right_dimension);
    google::protobuf::RepeatedField<double> const& matrix_data = buffer.matrix_data();
    assert(operator_site_tensor.size()*2 == (unsigned int)matrix_data.size());
    google::protobuf::RepeatedField<double>::const_iterator iter = matrix_data.begin();
    BOOST_FOREACH(std::complex<double>& x, operator_site_tensor) {
        x.real() = *(iter++);
        x.imag() = *(iter++);
    }
    boost::copy(buffer.index_data(),static_cast<uint32_t*>(operator_site_tensor));
    tensor = boost::move(operator_site_tensor);
}
//@+node:gcross.20110902105950.2689: *3* State
void operator<<(Nutcracker::Protobuf::State& buffer, Nutcracker::State const& state) {
    using namespace Nutcracker;
    buffer.clear_sites();
    static_cast<Protobuf::StateSite&>(*buffer.add_sites()) << static_cast<StateSite<Middle> const&>(state.getFirstSite());
    BOOST_FOREACH(StateSite<Right> const& state_site, state.getRestSites()) {
        (*buffer.add_sites()) << state_site;
    }
}

void operator>>(Nutcracker::Protobuf::State const& buffer, Nutcracker::State& tensor) {
    using namespace Nutcracker;
    StateSite<Middle> first_site;
    vector<StateSite<Right> > rest_sites(buffer.sites_size()-1);
    buffer.sites(0) >> first_site;
    BOOST_FOREACH(unsigned int const index, irange<unsigned int>(0u,rest_sites.size())) {
        buffer.sites(index+1) >> rest_sites[index];
    }
    tensor = State(boost::move(first_site),boost::move(rest_sites));
}
//@-others
//@-leo
