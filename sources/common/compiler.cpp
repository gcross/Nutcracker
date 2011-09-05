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
