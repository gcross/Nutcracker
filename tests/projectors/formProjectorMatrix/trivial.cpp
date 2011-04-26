//@+leo-ver=5-thin
//@+node:gcross.20110307093706.2986: * @file trivial.cpp
//@@language cplusplus
//@+<< License >>
//@+node:gcross.20110307093706.2987: ** << License >>
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
//@+node:gcross.20110307093706.2988: ** << Includes >>
#include <boost/assign/list_of.hpp>
#include <boost/range/adaptor/indirected.hpp>

#include "projectors.hpp"

#include "test_utils.hpp"

using namespace Nutcracker;

using boost::adaptors::indirected;
using boost::assign::list_of;
//@-<< Includes >>
int main() {
    ProjectorMatrix const projector_matrix(
        formProjectorMatrix(
             list_of(&OverlapBoundary<Left>::trivial) | indirected
            ,list_of(&OverlapBoundary<Right>::trivial) | indirected
            ,list_of(&OverlapSite<Middle>::trivial) | indirected
        )
    );
    ASSERT_EQUAL_TO(1u,projector_matrix.numberOfProjectors());
    ASSERT_EQUAL_TO(1u,projector_matrix.projectorLength());
    ASSERT_EQUAL_TO(1u,projector_matrix.numberOfReflectors());
    ASSERT_EQUAL_TO(0u,projector_matrix.orthogonalSubspaceDimension());

    return 0;
}
//@-leo
