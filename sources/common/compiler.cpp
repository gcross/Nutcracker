//@+leo-ver=5-thin
//@+node:gcross.20110805222031.2351: * @file compiler.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110805222031.2352: ** << License >>
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
//@+node:gcross.20110805222031.2353: ** << Includes >>
#include <boost/container/set.hpp>
#include <boost/lambda/bind.hpp>
#include <boost/make_shared.hpp>
#include <boost/range/algorithm/for_each.hpp>
#include <boost/range/algorithm/fill.hpp>
#include <boost/range/algorithm/sort.hpp>
#include <boost/range/algorithm/transform.hpp>
#include <boost/smart_ptr.hpp>
#include <boost/unordered_set.hpp>

#include "compiler.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110805222031.2354: ** << Usings >>
using boost::container::map;
using boost::container::set;
using boost::fill;
namespace lambda = boost::lambda;
using boost::make_shared;
using boost::make_tuple;
using boost::irange;
using boost::scoped_array;
using boost::sort;
using boost::transform;
using boost::unordered_set;

using std::iterator_traits;
using std::make_pair;
//@-<< Usings >>

//@+others
//@+node:gcross.20110805222031.2355: ** Classes
//@+node:gcross.20110805222031.2356: *3* OperatorBuilder
//@+node:gcross.20110822214054.2515: *4* add(X)ExternalField
OperatorBuilder& OperatorBuilder::addLocalExternalField(unsigned int site_number, unsigned int field_matrix_id, complex<double> scale_factor) {
    return connect(site_number,getStartSignal(),getEndSignal(),field_matrix_id,scale_factor);
}

OperatorBuilder& OperatorBuilder::addGlobalExternalField(unsigned int field_matrix_id, complex<double> scale_factor) {
    BOOST_FOREACH(unsigned int const site_number, irange(0u,numberOfSites())) {
        addLocalExternalField(site_number,field_matrix_id,scale_factor);
    }
    return *this;
}
//@+node:gcross.20110822214054.2520: *4* add(X)NeighborCouplingField
OperatorBuilder& OperatorBuilder::addLocalNeighborCouplingField(unsigned int left_site_number, unsigned int left_field_matrix_id, unsigned int right_field_matrix_id, complex<double> scale_factor) {
    unsigned int const signal = allocateSignal();
    connect(left_site_number,getStartSignal(),signal,left_field_matrix_id,scale_factor);
    return connect(left_site_number+1,signal,getEndSignal(),right_field_matrix_id);
}

OperatorBuilder& OperatorBuilder::addGlobalNeighborCouplingField(unsigned int left_field_matrix_id, unsigned int right_field_matrix_id, complex<double> scale_factor) {
    unsigned int const signal = allocateSignal();
    BOOST_FOREACH(unsigned int const site_number, irange(0u,numberOfSites())) {
        connect(site_number,getStartSignal(),signal,left_field_matrix_id,scale_factor);
        connect(site_number,signal,getEndSignal(),right_field_matrix_id);
    }
    return *this;
}
//@+node:gcross.20110828205143.2631: *4* compile
Operator OperatorBuilder::compile(bool optimize, bool add_start_and_end_loops) {
    OperatorSpecification source = generateSpecification(add_start_and_end_loops);
    if(optimize) source.optimize();
    return source.compile();
}
//@+node:gcross.20110828205143.2632: *4* generateSpecification
OperatorSpecification OperatorBuilder::generateSpecification(bool add_start_and_end_loops) {
    OperatorSpecification specification = Base::generateSpecification();
    if(add_start_and_end_loops) {
        BOOST_FOREACH(unsigned int const site_number, irange((size_t)0u,connections.size())) {
            specification.connect(site_number,specification.getStartSignal(),specification.getStartSignal(),specification.lookupIdOfIdentityWithDimension(sites[site_number]));
            specification.connect(site_number,specification.getEndSignal(),  specification.getEndSignal(),  specification.lookupIdOfIdentityWithDimension(sites[site_number]));
        }
    }
    return boost::move(specification);
}
//@+node:gcross.20110828205143.2628: *3* StateBuilder
//@+node:gcross.20110828205143.2639: *4* addWState
StateBuilder& StateBuilder::addWState(unsigned int common_observation,unsigned int special_observation,complex<double> amplitude) {
    complex<double> coefficient = amplitude/c(numberOfSites(),0);
    if(numberOfSites() == 1u) {
        connect(0u,getStartSignal(),getEndSignal(),lookupIdOfObservation(special_observation,sites[0u]),coefficient);
        return *this;;
    }
    unsigned int const
          signal_1 = allocateSignal()
        , signal_2 = allocateSignal()
        , last_site_number = numberOfSites()-1
        ;
    connect(0u,getStartSignal(),signal_1,lookupIdOfObservation(common_observation,sites[0u]));
    connect(0u,getStartSignal(),signal_2,lookupIdOfObservation(special_observation,sites[0u]),coefficient);
    connect(last_site_number,signal_1,getEndSignal(),lookupIdOfObservation(special_observation,sites[last_site_number]),coefficient);
    connect(last_site_number,signal_2,getEndSignal(),lookupIdOfObservation(common_observation,sites[last_site_number]));
    BOOST_FOREACH(unsigned int const site_number, irange(1u,last_site_number)) {
        connect(site_number,signal_1,signal_1,lookupIdOfObservation(common_observation,sites[site_number]));
        connect(site_number,signal_1,signal_2,lookupIdOfObservation(special_observation,sites[site_number]),coefficient);
        connect(site_number,signal_2,signal_2,lookupIdOfObservation(common_observation,sites[site_number]));
    }
    return *this;
}
//@+node:gcross.20110828205143.2634: *4* compile
State StateBuilder::compile(bool optimize) {
    StateSpecification source = generateSpecification();
    if(optimize) source.optimize();
    return source.compile();
}
//@+node:gcross.20110828205143.2636: *4* generateSpecification
StateSpecification StateBuilder::generateSpecification() {
    return Base::generateSpecification();
}
//@+node:gcross.20110805222031.4644: *3* SignalTable
//@+node:gcross.20110805222031.4645: *4* (constructors)
SignalTable::SignalTable()
  : next_free_signal(3)
{}
//@+node:gcross.20110805222031.2378: *4* allocateSignal
unsigned int SignalTable::allocateSignal() {
    return next_free_signal++;
}
//@+node:gcross.20110817110920.2484: *4* reserveSignalsBelow
void SignalTable::reserveSignalsBelow(unsigned int exclusive_upper_bound) {
    next_free_signal = std::max(next_free_signal,exclusive_upper_bound);
}
//@-others

}
//@-leo
