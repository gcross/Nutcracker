//@+leo-ver=5-thin
//@+node:gcross.20110213161858.1810: * @thin states.hpp
//@@language cplusplus

#ifndef NUTCRACKER_STATES_HPP
#define NUTCRACKER_STATES_HPP

//@+<< Includes >>
//@+node:gcross.20110213161858.1811: ** << Includes >>
#include <boost/container/vector.hpp>

#include "tensors.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110213161858.1812: ** << Usings >>
using boost::container::vector;
//@-<< Usings >>

//@+others
//@+node:gcross.20110214155808.1966: ** Exceptions
//@+node:gcross.20110214155808.1967: *3* NormalizationError
struct NormalizationError : public Exception {
    int const info;
    NormalizationError(int info);
};
//@+node:gcross.20110214155808.1968: *3* NotEnoughDegreesOfFreedomToNormalizeError
struct NotEnoughDegreesOfFreedomToNormalizeError : public Exception {
    string n1, n2, n3;
    unsigned int d1, d2, d3;
    NotEnoughDegreesOfFreedomToNormalizeError(
         string const& n1
        ,unsigned int const d1
        ,string const& n2
        ,unsigned int const d2
        ,string const& n3
        ,unsigned int const d3
    ) : Exception((
            format("Not enough degrees of freedom to normalize (%1% (%2%) > %3% (%4%) * %5% (%6%))")
                % n1
                % d1
                % n2
                % d2
                % n3
                % d3
        ).str())
      , n1(n1)
      , n2(n2)
      , n3(n3)
      , d1(d1)
      , d2(d2)
      , d3(d3)
    { }
    virtual ~NotEnoughDegreesOfFreedomToNormalizeError() throw() {}
};
//@+node:gcross.20110214164734.1928: ** Tensors
//@+node:gcross.20110214164734.1930: *3* StateSiteAny
class StateSiteAny : public SiteBaseTensor {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(StateSiteAny)
protected:
    StateSiteAny() {}

    StateSiteAny(BOOST_RV_REF(StateSiteAny) other)
      : SiteBaseTensor(boost::move(static_cast<SiteBaseTensor&>(other)))
    { }

    StateSiteAny(
          PhysicalDimension const physical_dimension
        , LeftDimension const left_dimension
        , RightDimension const right_dimension
    ) : SiteBaseTensor(
             physical_dimension
            ,left_dimension
            ,right_dimension
            ,(*physical_dimension)*(*left_dimension)*(*right_dimension)
        )
    { }

    StateSiteAny(
          CopyFrom<StateSiteAny const> const other_site
    ) : SiteBaseTensor(other_site)
    { }

    StateSiteAny(
          DimensionsOf<StateSiteAny const> const other_site
    ) : SiteBaseTensor(other_site)
    { }

    template<typename G> StateSiteAny(
          PhysicalDimension const physical_dimension
        , LeftDimension const left_dimension
        , RightDimension const right_dimension
        , FillWithGenerator<G> const generator
    ) : SiteBaseTensor(
             physical_dimension
            ,left_dimension
            ,right_dimension
            ,(*physical_dimension)*(*left_dimension)*(*right_dimension)
            ,generator
        )
    { }

    template<typename Range> StateSiteAny(
          LeftDimension const left_dimension
        , RightDimension const right_dimension
        , FillWithRange<Range> const init
    ) : SiteBaseTensor(
             PhysicalDimension(init->size()/((*left_dimension)*(*right_dimension)))
            ,left_dimension
            ,right_dimension
            ,init
        )
    { }

    StateSiteAny(MakeTrivial const make_trivial) : SiteBaseTensor(make_trivial) {}

    void operator=(BOOST_RV_REF(StateSiteAny) other) {
        SiteBaseTensor::operator=(boost::move(static_cast<SiteBaseTensor&>(other)));
    }

    void swap(StateSiteAny& other) {
        SiteBaseTensor::swap(other);
    }
};
//@+node:gcross.20110214164734.1931: *3* StateSite
template<typename side> class StateSite : public StateSiteAny {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(StateSite)
public:
    StateSite() {}

    StateSite(BOOST_RV_REF(StateSite) other)
      : StateSiteAny(boost::move(static_cast<StateSiteAny&>(other)))
    { }

    StateSite(
          PhysicalDimension const physical_dimension
        , LeftDimension const left_dimension
        , RightDimension const right_dimension
    ) : StateSiteAny(
             physical_dimension
            ,left_dimension
            ,right_dimension
        )
    { }

    StateSite(
          CopyFrom<StateSiteAny const> const other_site
    ) : StateSiteAny(other_site)
    { }

    StateSite(
          DimensionsOf<StateSiteAny const> const other_site
    ) : StateSiteAny(other_site)
    { }

    template<typename G> StateSite(
          PhysicalDimension const physical_dimension
        , LeftDimension const left_dimension
        , RightDimension const right_dimension
        , FillWithGenerator<G> const generator
    ) : StateSiteAny(
             physical_dimension
            ,left_dimension
            ,right_dimension
            ,generator
        )
    { }

    template<typename Range> StateSite(
          LeftDimension const left_dimension
        , RightDimension const right_dimension
        , FillWithRange<Range> const init
    ) : StateSiteAny(
             left_dimension
            ,right_dimension
            ,init
        )
    { }

    StateSite(MakeTrivial const make_trivial) : StateSiteAny(make_trivial) {}

    StateSite& operator=(BOOST_RV_REF(StateSite) other) {
        if(this == &other) return *this;
        StateSiteAny::operator=(boost::move(static_cast<StateSiteAny&>(other)));
        return *this;
    }

    void swap(StateSite& other) {
        if(this == &other) return;
        StateSiteAny::swap(other);
    }

    double norm() const { return BaseTensor::norm(); }

    static StateSite const trivial;
};

template<typename side> StateSite<side> const StateSite<side>::trivial(make_trivial);
//@+node:gcross.20110214164734.1932: *3* StateVectorFragment
class StateVectorFragment : public BaseTensor {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(StateVectorFragment)
    PhysicalDimension physical_dimension;
    RightDimension right_dimension;
public:
    StateVectorFragment() {}

    StateVectorFragment(BOOST_RV_REF(StateVectorFragment) other)
      : BaseTensor(boost::move(static_cast<BaseTensor&>(other)))
      , physical_dimension(boost::move(other.physical_dimension))
      , right_dimension(boost::move(other.right_dimension))
    { }

    StateVectorFragment(
          PhysicalDimension const physical_dimension
        , RightDimension const right_dimension
    ) : BaseTensor((*physical_dimension)*(*right_dimension))
      , physical_dimension(physical_dimension)
      , right_dimension(right_dimension)
    { }

    template<typename G> StateVectorFragment(
          PhysicalDimension const physical_dimension
        , RightDimension const right_dimension
        , FillWithGenerator<G> const generator
    ) : BaseTensor((*physical_dimension)*(*right_dimension),generator)
      , physical_dimension(physical_dimension)
      , right_dimension(right_dimension)
    { }

    template<typename Range> StateVectorFragment(
          PhysicalDimension const physical_dimension
        , FillWithRange<Range> const init
    ) : BaseTensor(init)
      , physical_dimension(physical_dimension)
      , right_dimension(size()/(*physical_dimension))
    { }

    StateVectorFragment(
          MakeTrivial const make_trivial
    ) : BaseTensor(make_trivial)
      , physical_dimension(1)
      , right_dimension(1)
    { }

    StateVectorFragment& operator=(BOOST_RV_REF(StateVectorFragment) other) {
        if(this == &other) return *this;
        BaseTensor::operator=(boost::move(static_cast<BaseTensor&>(other)));
        physical_dimension = boost::move(other.physical_dimension);
        right_dimension = boost::move(other.right_dimension);
        return *this;
    }

    operator StateVector() const {
        assert(rightDimension(as_unsigned_integer) == 1);
        StateVector v(size());
        copy(*this,v.begin());
        return v;
    }

    PhysicalDimension physicalDimension() const { return physical_dimension; }
    unsigned int physicalDimension(AsUnsignedInteger _) const { return *physical_dimension; }

    RightDimension rightDimension() const { return right_dimension; }
    unsigned int rightDimension(AsUnsignedInteger _) const { return *right_dimension; }

    static StateVectorFragment const trivial;
};
//@+node:gcross.20110214164734.1929: *3* OverlapSite
template<typename side> class OverlapSite : public SiteBaseTensor {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(OverlapSite)
public:
    OverlapSite() {}

    OverlapSite(BOOST_RV_REF(OverlapSite) other)
      : SiteBaseTensor(boost::move(static_cast<SiteBaseTensor&>(other)))
    { }

    OverlapSite(
          RightDimension const right_dimension
        , PhysicalDimension const physical_dimension
        , LeftDimension const left_dimension
    ) : SiteBaseTensor(
             physical_dimension
            ,left_dimension
            ,right_dimension
            ,(*physical_dimension)*(*left_dimension)*(*right_dimension)
        )
    { }

    template<typename other_side> OverlapSite(
          CopyFrom<OverlapSite<other_side> const> const other_site
    ) : SiteBaseTensor(other_site)
    { }

    template<typename other_side> OverlapSite(
          DimensionsOf<StateSite<other_side> const> const other_site
    ) : SiteBaseTensor(other_site)
    { }

    template<typename G> OverlapSite(
          RightDimension const right_dimension
        , PhysicalDimension const physical_dimension
        , LeftDimension const left_dimension
        , FillWithGenerator<G> const generator
    ) : SiteBaseTensor(
             physical_dimension
            ,left_dimension
            ,right_dimension
            ,(*physical_dimension)*(*left_dimension)*(*right_dimension)
            ,generator
        )
    { }

    template<typename Range> OverlapSite(
          RightDimension const right_dimension
        , LeftDimension const left_dimension
        , FillWithRange<Range> const init
    ) : SiteBaseTensor(
             PhysicalDimension(init->size()/((*left_dimension)*(*right_dimension)))
            ,left_dimension
            ,right_dimension
            ,init
        )
    { }

    OverlapSite(MakeTrivial const make_trivial) : SiteBaseTensor(make_trivial) {}

    OverlapSite& operator=(BOOST_RV_REF(OverlapSite) other) {
        if(this == &other) return *this;
        SiteBaseTensor::operator=(boost::move(static_cast<SiteBaseTensor&>(other)));
        return *this;
    }

    void swap(OverlapSite& other) {
        if(this == &other) return;
        SiteBaseTensor::swap(other);
    }

    static OverlapSite const trivial;
};

template<typename side> OverlapSite<side> const OverlapSite<side>::trivial(make_trivial);
//@+node:gcross.20110213233103.2762: ** Classes
//@+node:gcross.20110213233103.2763: *3* IncreaseDimensionBetweenResult
template<typename side1,typename side2> class IncreaseDimensionBetweenResult {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(IncreaseDimensionBetweenResult)
public:
    StateSite<side1> state_site_1;
    StateSite<side2> state_site_2;

    IncreaseDimensionBetweenResult(BOOST_RV_REF(IncreaseDimensionBetweenResult) other)
      : state_site_1(boost::move(other.state_site_1))
      , state_site_2(boost::move(other.state_site_2))
    {}

    IncreaseDimensionBetweenResult(
          BOOST_RV_REF(StateSite<side1>) state_site_1
        , BOOST_RV_REF(StateSite<side2>) state_site_2
    ) : state_site_1(state_site_1)
      , state_site_2(state_site_2)
    {}
};
//@+node:gcross.20110213233103.2764: *3* MoveSiteCursorResult
template<typename side> class MoveSiteCursorResult {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(MoveSiteCursorResult)
    typedef typename Other<side>::value other_side;
public:
    StateSite<Middle> middle_state_site;
    StateSite<other_side> other_side_state_site;

    MoveSiteCursorResult(BOOST_RV_REF(MoveSiteCursorResult) other)
      : middle_state_site(boost::move(other.middle_state_site))
      , other_side_state_site(boost::move(other.other_side_state_site))
    {}

    MoveSiteCursorResult(
          BOOST_RV_REF(StateSite<Middle>) middle_state_site
        , BOOST_RV_REF(StateSite<other_side>) other_side_state_site
    ) : middle_state_site(middle_state_site)
      , other_side_state_site(other_side_state_site)
    {}
};
//@+node:gcross.20110214164734.2011: *3* OverlapSitesFromStateSitesAndNormalizeResult
class OverlapSitesFromStateSitesAndNormalizeResult {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(OverlapSitesFromStateSitesAndNormalizeResult)
public:
    OverlapSite<Left> left_overlap_site_from_middle_state_site;
    OverlapSite<Middle> middle_overlap_site_from_middle_state_site;
    StateSite<Middle> middle_state_site_from_right_state_site;
    OverlapSite<Right> right_overlap_site_from_right_state_site;

    OverlapSitesFromStateSitesAndNormalizeResult(
          BOOST_RV_REF(OverlapSitesFromStateSitesAndNormalizeResult) other
    ) : left_overlap_site_from_middle_state_site(boost::move(other.left_overlap_site_from_middle_state_site))
      , middle_overlap_site_from_middle_state_site(boost::move(other.middle_overlap_site_from_middle_state_site))
      , middle_state_site_from_right_state_site(boost::move(other.middle_state_site_from_right_state_site))
      , right_overlap_site_from_right_state_site(boost::move(other.right_overlap_site_from_right_state_site))
    {}

    OverlapSitesFromStateSitesAndNormalizeResult(
          BOOST_RV_REF(OverlapSite<Left>) left_overlap_site_from_middle_state_site
        , BOOST_RV_REF(OverlapSite<Middle>) middle_overlap_site_from_middle_state_site
        , BOOST_RV_REF(StateSite<Middle>) middle_state_site_from_right_state_site
        , BOOST_RV_REF(OverlapSite<Right>) right_overlap_site_from_right_state_site
    ) : left_overlap_site_from_middle_state_site(left_overlap_site_from_middle_state_site)
      , middle_overlap_site_from_middle_state_site(middle_overlap_site_from_middle_state_site)
      , middle_state_site_from_right_state_site(middle_state_site_from_right_state_site)
      , right_overlap_site_from_right_state_site(right_overlap_site_from_right_state_site)
    {}
};
//@+node:gcross.20110213161858.1813: ** Functions
StateVector computeStateVector(vector<StateSiteAny const*> const& state_sites);

OverlapSite<Middle> computeOverlapSiteFromStateSite(StateSite<Middle> const& state_site);

OverlapSitesFromStateSitesAndNormalizeResult computeOverlapSitesFromStateSitesAndNormalize(
      StateSite<Middle> const& middle_state_site
     ,StateSite<Right> const& right_state_site
);

StateVectorFragment extendStateVectorFragment(
      StateVectorFragment const& old_fragment
    , StateSiteAny const& state_site
);

IncreaseDimensionBetweenResult<Right,Right> increaseDimensionBetweenRightRight(
      unsigned int new_dimension
    , StateSite<Right> const& old_site_1
    , StateSite<Right> const& old_site_2
);

IncreaseDimensionBetweenResult<Middle,Right> increaseDimensionBetweenMiddleRight(
      unsigned int new_dimension
    , StateSite<Middle> const& old_site_1
    , StateSite<Right> const& old_site_2
);

MoveSiteCursorResult<Left> moveSiteCursorLeft(
      StateSite<Middle> const& old_state_site_2
    , StateSite<Left> const& old_state_site_1
);

MoveSiteCursorResult<Right> moveSiteCursorRight(
      StateSite<Middle> const& old_state_site_1
    , StateSite<Right> const& old_state_site_2
);

StateSite<Middle> randomStateSiteMiddle(
      const PhysicalDimension physical_dimension
    , const LeftDimension left_dimension
    , const RightDimension right_dimension
);

StateSite<Right> randomStateSiteRight(
      const PhysicalDimension physical_dimension
    , const LeftDimension left_dimension
    , const RightDimension right_dimension
);
//@+node:gcross.20110213233103.2755: ** struct moveSiteCursor
template<typename side> struct moveSiteCursor { };

template<> struct moveSiteCursor<Left> {
    static MoveSiteCursorResult<Left> from(
          StateSite<Middle> const& old_middle_state_site
	   , StateSite<Left> const& old_left_state_site
    ) { return moveSiteCursorLeft(old_middle_state_site,old_left_state_site); }
};

template<> struct moveSiteCursor<Right> {
    static MoveSiteCursorResult<Right> from(
          StateSite<Middle> const& old_middle_state_site
	   , StateSite<Right> const& old_right_state_site
    ) { return moveSiteCursorRight(old_middle_state_site,old_right_state_site); }
};
//@-others

}

#endif
//@-leo
