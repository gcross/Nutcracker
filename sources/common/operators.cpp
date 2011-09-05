//@+leo-ver=5-thin
//@+node:gcross.20110206185121.1777: * @file operators.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110220182654.2032: ** << License >>
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
//@+node:gcross.20110206185121.1778: ** << Includes >>
#include <boost/make_shared.hpp>
#include <boost/range/algorithm/copy.hpp>
#include <boost/tuple/tuple.hpp>
#include <complex>
#include <iterator>
#include <stdint.h>
#include <utility>

#include "nutcracker/operators.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110206185121.1779: ** << Usings >>
using boost::irange;
using boost::make_shared;
using boost::make_tuple;
using boost::tuple;

using std::make_pair;
using std::ostream_iterator;
//@-<< Usings >>

//@+others
//@+node:gcross.20110206185121.1783: ** Functions
//@+node:gcross.20110207115918.1781: *3* constructExternalFieldOperator
Operator constructExternalFieldOperator(
      unsigned int const number_of_sites
    , MatrixConstPtr const& matrix
) {
    assert(number_of_sites > 0);
    PhysicalDimension const physical_dimension(matrix->size1());
    assert(*physical_dimension == matrix->size2());
    Operator operator_sites;
    operator_sites.reserve(number_of_sites);
    if(number_of_sites == 1) {
        operator_sites.emplace_back(new OperatorSite(
            constructOperatorSite(
                 physical_dimension
                ,LeftDimension(1)
                ,RightDimension(1)
                ,list_of(OperatorSiteLink(1,1,matrix))
            )
        ));
    } else {
        MatrixConstPtr I = identityMatrix(*physical_dimension);
        operator_sites.emplace_back(new OperatorSite(
            constructOperatorSite(
                 physical_dimension
                ,LeftDimension(1)
                ,RightDimension(2)
                ,list_of
                    (OperatorSiteLink(1,1,I))
                    (OperatorSiteLink(1,2,matrix))
            )
        ));
        shared_ptr<OperatorSite const> const middle(new OperatorSite(
            constructOperatorSite(
                 physical_dimension
                ,LeftDimension(2)
                ,RightDimension(2)
                ,list_of
                    (OperatorSiteLink(1,1,I))
                    (OperatorSiteLink(1,2,matrix))
                    (OperatorSiteLink(2,2,I))
            )
        ));
        REPEAT(number_of_sites-2) {
            operator_sites.emplace_back(middle);
        }
        operator_sites.emplace_back(new OperatorSite(
            constructOperatorSite(
                 physical_dimension
                ,LeftDimension(2)
                ,RightDimension(1)
                ,list_of
                    (OperatorSiteLink(1,1,matrix))
                    (OperatorSiteLink(2,1,I))
            )
        ));
    }
    return boost::move(operator_sites);
}
//@+node:gcross.20110206185121.1784: *3* constructOperatorSite
OperatorSite constructOperatorSite(
      PhysicalDimension const physical_dimension
    , LeftDimension const left_dimension
    , RightDimension const right_dimension
    , vector<OperatorSiteLink> const& links
) {
    assert(links.size() > 0);

    OperatorSite operator_site
        (links.size()
        ,physical_dimension
        ,left_dimension
        ,right_dimension
        );
    uint32_t* index_data = operator_site;
    complex<double>* matrix_data = operator_site;

    BOOST_FOREACH(
         OperatorSiteLink const& link
        ,links
    ) {
        *index_data++ = link.from;
        *index_data++ = link.to;
        assert(link.label->size1() == *physical_dimension);
        assert(link.label->size2() == *physical_dimension);
        matrix_data = copy(link.label->data(),matrix_data);
    }

    return boost::move(operator_site);
}
//@+node:gcross.20110208195213.1789: *3* constructTransverseIsingModelOperator
Operator constructTransverseIsingModelOperator(
      unsigned int const number_of_sites
    , double spin_coupling_strength
) {
    using namespace Pauli;
    assert(number_of_sites > 1);
    MatrixConstPtr const
          &X1 = X
        ,  X2 = make_shared<Matrix const>(spin_coupling_strength*(*X))
        ;
    Operator operator_sites;
    operator_sites.reserve(number_of_sites);
    operator_sites.emplace_back(new OperatorSite(
        constructOperatorSite(
             PhysicalDimension(2)
            ,LeftDimension(1)
            ,RightDimension(3)
            ,list_of
                (OperatorSiteLink(1,1,I ))
                (OperatorSiteLink(1,2,X1))
                (OperatorSiteLink(1,3,Z ))
        )
    ));
    shared_ptr<OperatorSite const> const middle(new OperatorSite(
        constructOperatorSite(
             PhysicalDimension(2)
            ,LeftDimension(3)
            ,RightDimension(3)
            ,list_of
                (OperatorSiteLink(1,1,I ))
                (OperatorSiteLink(1,2,X1))
                (OperatorSiteLink(2,3,X2))
                (OperatorSiteLink(1,3,Z ))
                (OperatorSiteLink(3,3,I ))
        )
    ));
    REPEAT(number_of_sites-2) {
        operator_sites.emplace_back(middle);
    }
    operator_sites.emplace_back(new OperatorSite(
        constructOperatorSite(
             PhysicalDimension(2)
            ,LeftDimension(3)
            ,RightDimension(1)
            ,list_of
                (OperatorSiteLink(1,1,Z ))
                (OperatorSiteLink(2,1,X2))
                (OperatorSiteLink(3,1,I ))
        )
    ));
    return boost::move(operator_sites);
}
//@+node:gcross.20110217175626.1939: *3* extractPhysicalDimensions
vector<unsigned int> extractPhysicalDimensions(Operator const& operator_sites) {
    vector<unsigned int> physical_dimensions;
    physical_dimensions.reserve(operator_sites.size()+1);
    BOOST_FOREACH(shared_ptr<OperatorSite const> const& operator_site, operator_sites) {
        physical_dimensions.push_back(operator_site->physicalDimension());
    }
    return boost::move(physical_dimensions);
}
//@-others

}
//@-leo
