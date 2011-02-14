//@+leo-ver=5-thin
//@+node:gcross.20110214164734.1953: * @thin connectors.hpp
//@@language cplusplus

#ifndef NUTCRACKER_CONNECTORS_HPP
#define NUTCRACKER_CONNECTORS_HPP

//@+<< Includes >>
//@+node:gcross.20110214164734.1954: ** << Includes >>
#include "boundaries.hpp"
#include "states.hpp"
#include "operators.hpp"
#include "projectors.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110214164734.1955: ** << Usings >>
//@-<< Usings >>

//@+others
//@+node:gcross.20110214164734.1998: ** Exceptions
//@+node:gcross.20110214164734.2000: *3* DimensionMismatch
struct DimensionMismatch : public Exception {
    DimensionMismatch(
          const char* n1
        , unsigned int const d1
        , const char* n2
        , unsigned int const d2
    ) : Exception((format("%1% dimension (%2%) does not match %3% dimension (%4%)") % n1 % d1 % n2 % d2).str())
    { }
};
//@+node:gcross.20110214164734.2004: ** Functions
//@+node:gcross.20110214164734.2005: *3* function connectDimensions
inline unsigned int connectDimensions(
      const char* n1
    , unsigned int const d1
    , const char* n2
    , unsigned int const d2
) {
    if(d1 != d2) throw DimensionMismatch(n1,d1,n2,d2);
    return d1;
}
//@+node:gcross.20110214164734.1977: ** Connectors
//@+node:gcross.20110214164734.1978: *3* ExpectationBoundary<Left> | OperatorSite
inline unsigned int operator||(
      ExpectationBoundary<Left> const& expectation_boundary
    , OperatorSite const& operator_site
) {
    return connectDimensions(
         "left expectation boundary state"
        ,expectation_boundary.operatorDimension(as_unsigned_integer)
        ,"operator site left"
        ,operator_site.leftDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110214164734.1979: *3* ExpectationBoundary<Left> | StateSite<Middle>
inline unsigned int operator||(
      ExpectationBoundary<Left> const& expectation_boundary
    , StateSite<Middle> const& state_site
) {
    return connectDimensions(
         "left expectation boundary state"
        ,expectation_boundary.stateDimension(as_unsigned_integer)
        ,"middle state site left"
        ,state_site.leftDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110214164734.1980: *3* ExpectationBoundary<Left> | StateSite<Left>
inline unsigned int operator||(
      ExpectationBoundary<Left> const& expectation_boundary
    , StateSite<Left> const& state_site
) {
    return connectDimensions(
         "left expectation boundary state"
        ,expectation_boundary.stateDimension(as_unsigned_integer)
        ,"middle state site left"
        ,state_site.leftDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110214164734.1981: *3* OperatorSite | ExpectationBoundary<Right>
inline unsigned int operator||(
      OperatorSite const& operator_site
    , ExpectationBoundary<Right> const& expectation_boundary
) {
    return connectDimensions(
         "operator site right"
        ,operator_site.rightDimension(as_unsigned_integer)
        ,"right expectation boundary state"
        ,expectation_boundary.operatorDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110214164734.1982: *3* OperatorSite | StateSite<Middle>
template<typename side> inline unsigned int operator||(
      OperatorSite const& operator_site
    , StateSite<side> const& state_site
) {
    return connectDimensions(
         "operator site physical"
        ,operator_site.physicalDimension(as_unsigned_integer)
        ,"middle state site physical"
        ,state_site.physicalDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110214164734.1983: *3* OverlapBoundary<Left> | OverlapSite<Left>
inline unsigned int operator||(
      OverlapBoundary<Left> const& overlap_boundary
    , OverlapSite<Left> const& overlap_site
) {
    return connectDimensions(
         "left overlap boundary overlap"
        ,overlap_boundary.overlapDimension(as_unsigned_integer)
        ,"left overlap site left"
        ,overlap_site.leftDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110214164734.1984: *3* OverlapBoundary<Left> | OverlapSite<Middle>
inline unsigned int operator||(
      OverlapBoundary<Left> const& overlap_boundary
    , OverlapSite<Middle> const& overlap_site
) {
    return connectDimensions(
         "left overlap boundary overlap"
        ,overlap_boundary.overlapDimension(as_unsigned_integer)
        ,"middle overlap site left"
        ,overlap_site.leftDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110214164734.1985: *3* OverlapBoundary<Left> | StateSite<Left>
inline unsigned int operator||(
      OverlapBoundary<Left> const& overlap_boundary
    , StateSite<Left> const& state_site
) {
    return connectDimensions(
         "left overlap boundary state"
        ,overlap_boundary.stateDimension(as_unsigned_integer)
        ,"left state site left"
        ,state_site.leftDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110214164734.1986: *3* OverlapBoundary<Left> | StateSite<Middle>
inline unsigned int operator||(
      OverlapBoundary<Left> const& overlap_boundary
    , StateSite<Middle> const& state_site
) {
    return connectDimensions(
         "left overlap boundary state"
        ,overlap_boundary.stateDimension(as_unsigned_integer)
        ,"middle state site left"
        ,state_site.leftDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110214164734.1987: *3* OverlapSite<*> | StateSite<*>
template<typename side> inline unsigned int operator||(
      OverlapSite<side> const& overlap_site
    , StateSite<side> const& state_site
) {
    return connectDimensions(
         "overlap site physical"
        ,overlap_site.physicalDimension(as_unsigned_integer)
        ,"state site physical"
        ,state_site.physicalDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110214164734.1988: *3* OverlapSite<Middle> | OverlapBoundary<Right>
inline unsigned int operator||(
      OverlapSite<Middle> const& overlap_site
    , OverlapBoundary<Right> const& overlap_boundary
) {
    return connectDimensions(
         "middle overlap site right"
        ,overlap_site.rightDimension(as_unsigned_integer)
        ,"right overlap boundary overlap"
        ,overlap_boundary.overlapDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110214164734.1989: *3* OverlapSite<Right> | OverlapBoundary<Right>
inline unsigned int operator||(
      OverlapSite<Right> const& overlap_site
    , OverlapBoundary<Right> const& overlap_boundary
) {
    return connectDimensions(
         "right overlap site right"
        ,overlap_site.rightDimension(as_unsigned_integer)
        ,"right overlap boundary overlap"
        ,overlap_boundary.overlapDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110214164734.1990: *3* ProjectorMatrix | StateSite<Middle>
inline unsigned int operator||(
      ProjectorMatrix const& projector_matrix
    , StateSite<Middle> const& state_site
) {
    return connectDimensions(
         "state site size"
        ,state_site.size()
        ,"projector length"
        ,projector_matrix.projectorLength()
    );
}
//@+node:gcross.20110214164734.1991: *3* StateSite<Left> | StateSite<Middle>
inline unsigned int operator||(
      StateSite<Left> const& state_site_1
    , StateSite<Middle> const& state_site_2
) {
    return connectDimensions(
         "left state site right"
        ,state_site_1.rightDimension(as_unsigned_integer)
        ,"middle state site left"
        ,state_site_2.leftDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110214164734.1992: *3* StateSite<Middle> | ExpectationBoundary<Left>
inline unsigned int operator||(
      StateSite<Middle> const& state_site
    , ExpectationBoundary<Right> const& expectation_boundary
) {
    return connectDimensions(
         "middle state site right"
        ,state_site.rightDimension(as_unsigned_integer)
        ,"right expectation boundary state"
        ,expectation_boundary.stateDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110214164734.1993: *3* StateSite<Middle> | OverlapBoundary<Right>
inline unsigned int operator||(
      StateSite<Middle> const& state_site
    , OverlapBoundary<Right> const& overlap_boundary
) {
    return connectDimensions(
         "middle state site right"
        ,state_site.rightDimension(as_unsigned_integer)
        ,"right overlap boundary state"
        ,overlap_boundary.stateDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110214164734.1994: *3* StateSite<Middle> | StateSite<Right>
inline unsigned int operator||(
      StateSite<Middle> const& state_site_1
    , StateSite<Right> const& state_site_2
) {
    return connectDimensions(
         "middle state site right"
        ,state_site_1.rightDimension(as_unsigned_integer)
        ,"right state site left"
        ,state_site_2.leftDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110214164734.1995: *3* StateSite<Right> | ExpectationBoundary<Right>
inline unsigned int operator||(
      StateSite<Right> const& state_site
    , ExpectationBoundary<Right> const& expectation_boundary
) {
    return connectDimensions(
         "right state site right"
        ,state_site.rightDimension(as_unsigned_integer)
        ,"right overlap boundary state"
        ,expectation_boundary.stateDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110214164734.1996: *3* StateSite<Right> | OverlapBoundary<Right>
inline unsigned int operator||(
      StateSite<Right> const& state_site
    , OverlapBoundary<Right> const& overlap_boundary
) {
    return connectDimensions(
         "right state site right"
        ,state_site.rightDimension(as_unsigned_integer)
        ,"right overlap boundary state"
        ,overlap_boundary.stateDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110214164734.1997: *3* StateSite<Right> | StateSite<Right>
inline unsigned int operator||(
      StateSite<Right> const& state_site_1
    , StateSite<Right> const& state_site_2
) {
    return connectDimensions(
         "right state site right"
        ,state_site_1.rightDimension(as_unsigned_integer)
        ,"right state site left"
        ,state_site_2.leftDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110214164734.2001: *3* StateVectorFragment | StateSiteAny
inline unsigned int operator||(
      StateVectorFragment const& fragment
    , StateSiteAny const& state_site
) {
    return connectDimensions(
         "fragment right"
        ,fragment.rightDimension(as_unsigned_integer)
        ,"state site left"
        ,state_site.leftDimension(as_unsigned_integer)
    );
}
//@-others

}

#endif
//@-leo
