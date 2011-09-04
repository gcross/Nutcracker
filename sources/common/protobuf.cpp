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
#include <boost/make_shared.hpp>
#include <boost/range/adaptor/indirected.hpp>
#include <boost/shared_ptr.hpp>
#include <iterator>
#include <fstream>

#include "io.hpp"
#include "protobuf.hpp"
#include "states.hpp"
//@-<< Includes >>

//@+<< Usings >>
//@+node:gcross.20110901221152.2674: ** << Usings >>
using boost::adaptors::indirected;
using boost::container::vector;
using boost::optional;
using boost::make_shared;
using boost::shared_ptr;
using boost::signals::trackable;

using std::cin;
using std::cout;
using std::ifstream;
using std::ofstream;
//@-<< Usings >>

namespace Nutcracker { namespace Protobuf {

//@+others
//@+node:gcross.20110901221152.2675: ** I/O Operators
//@+others
//@+node:gcross.20110902105950.2704: *3* Operator
void operator<<(OperatorBuffer& buffer, Operator const& op) {
    vector<shared_ptr<OperatorSite const> > unique_operator_sites;
    vector<unsigned int> sequence;
    deconstructOperatorTo(op,unique_operator_sites,sequence);
    buffer.clear_sites();
    buffer.mutable_sites()->Reserve(unique_operator_sites.size());
    BOOST_FOREACH(OperatorSite const& operator_site, unique_operator_sites | indirected) {
        (*buffer.add_sites()) << operator_site;
    }
    buffer.clear_sequence();
    buffer.mutable_sequence()->Reserve(sequence.size());
    BOOST_FOREACH(unsigned int const id, sequence) {
        buffer.add_sequence(id);
    }
}

void operator>>(OperatorBuffer const& buffer, Operator& op) {
    vector<shared_ptr<OperatorSite const> > unique_operator_sites;
    vector<unsigned int> sequence;
    unique_operator_sites.reserve(buffer.sites_size());
    BOOST_FOREACH(OperatorSiteBuffer const& operator_site, buffer.sites()) {
        shared_ptr<OperatorSite> operator_site_ptr = make_shared<OperatorSite>();
        operator_site >> *operator_site_ptr;
        unique_operator_sites.emplace_back(operator_site_ptr);
    }
    sequence.reserve(buffer.sequence_size());
    boost::copy(buffer.sequence(),std::back_inserter(sequence));
    op = constructOperatorFrom(unique_operator_sites,sequence);
}
//@+node:gcross.20110902105950.2696: *3* OperatorSite
void operator<<(OperatorSiteBuffer& buffer, OperatorSite const& tensor) {
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

void operator>>(OperatorSiteBuffer const& buffer, OperatorSite& tensor) {
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
struct Protobuf_State_Chain_Callback {
    StateBuffer& buffer;
    Protobuf_State_Chain_Callback(StateBuffer& buffer) : buffer(buffer) {}
    template<typename RestSites> void operator()(StateSite<Middle> const& first_site, RestSites const& rest_sites) {
        setState(buffer,first_site,rest_sites);
    }
};

void operator<<(StateBuffer& buffer, Chain const& chain) {
    Protobuf_State_Chain_Callback callback(buffer);
    chain.callWithStateSites(callback);
}

void operator<<(StateBuffer& buffer, State const& state) {
    setState(buffer,state.getFirstSite(),state.getRestSites());
}

void operator>>(StateBuffer const& buffer, State& tensor) {
    StateSite<Middle> first_site;
    vector<StateSite<Right> > rest_sites(buffer.sites_size()-1);
    buffer.sites(0) >> first_site;
    BOOST_FOREACH(unsigned int const index, irange<unsigned int>(0u,rest_sites.size())) {
        buffer.sites(index+1) >> rest_sites[index];
    }
    tensor = State(boost::move(first_site),boost::move(rest_sites));
}
//@-others
//@+node:gcross.20110903120540.2684: ** Formats
//@+others
//@+node:gcross.20110903120540.2685: *3* Input
static Operator readOperator(optional<string> const& maybe_filename, optional<string> const& maybe_location) {
    assert(!maybe_location);
    OperatorBuffer buffer;
    if(maybe_filename) {
        ifstream in(maybe_filename->c_str());
        buffer.ParseFromIstream(&in);
    } else {
        buffer.ParseFromIstream(&cin);
    }

    Operator op;
    buffer >> op;
    return boost::move(op);
}
//@+node:gcross.20110903120540.2686: *3* Output
struct Outputter : public Destructable, public trackable {
    Chain const& chain;
    optional<string> const maybe_filename;
    SimulationResultsBuffer buffer;

    Outputter(
        optional<string> const& maybe_filename
      , bool output_states
      , bool overwrite
      , Chain& chain
    )
      : chain(chain)
      , maybe_filename(maybe_filename)
    {
        buffer.set_sanity_check_threshold(chain.sanity_check_threshold);
        buffer.set_sweep_convergence_threshold(chain.sweep_convergence_threshold);
        buffer.set_chain_convergence_threshold(chain.chain_convergence_threshold);
        buffer.set_site_convergence_threshold(chain.site_convergence_threshold);

        chain.signalChainOptimized.connect(boost::bind(&Outputter::postSolution,this));
    }

    virtual ~Outputter() {
        if(maybe_filename) {
            ofstream out(maybe_filename->c_str(),ofstream::binary);
            buffer.SerializeToOstream(&out);
            out.flush();
        } else {
            buffer.SerializeToOstream(&cout);
        }
    }

    void postSolution() {
        SolutionBuffer& solution = *buffer.add_solutions();
        solution.set_eigenvalue(chain.getEnergy());
        StateBuffer& state = *solution.mutable_eigenvector();
        state << chain;
    }
};

auto_ptr<Destructable const> connectToChain(
    optional<string> const& maybe_filename
  , optional<string> const& maybe_location
  , bool output_states
  , bool overwrite
  , Chain& chain
) {
    assert(!maybe_location);
    return auto_ptr<Destructable const>(new Outputter(maybe_filename,output_states,overwrite,chain));
}
//@-others

void installFormat() {
    static InputFormat protobuf_input_format("protobuf","Google Protocol Buffers format",true,false,list_of("prb"),readOperator);
    static OutputFormat protobuf_output_format("protobuf","Google Protocol Buffers format",true,false,list_of("prb"),true,connectToChain);
}
//@-others

} }
//@-leo
