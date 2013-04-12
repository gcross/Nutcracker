#include <boost/make_shared.hpp>
#include <boost/range/algorithm/copy.hpp>
#include <boost/tuple/tuple.hpp>
#include <complex>
#include <iterator>
#include <stdint.h>
#include <utility>

#include "nutcracker/operators.hpp"

namespace Nutcracker {

using boost::irange;
using boost::make_shared;
using boost::make_tuple;
using boost::tuple;

using std::make_pair;
using std::ostream_iterator;

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
vector<unsigned int> extractPhysicalDimensions(Operator const& operator_sites) {
    vector<unsigned int> physical_dimensions;
    physical_dimensions.reserve(operator_sites.size()+1);
    BOOST_FOREACH(shared_ptr<OperatorSite const> const& operator_site, operator_sites) {
        physical_dimensions.push_back(operator_site->physicalDimension());
    }
    return boost::move(physical_dimensions);
}

}
