//@+leo-ver=5-thin
//@+node:gcross.20110206092738.1736: * @thin test_utils.cpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110206092738.1737: ** << Includes >>
#include <boost/range/algorithm/generate.hpp>
#include <boost/range/irange.hpp>
#include <boost/numeric/ublas/hermitian.hpp>

#include "utilities.hpp"

#include "test_utils.hpp"

using boost::irange;
using boost::generate;
using boost::variate_generator;
//@-<< Includes >>

//@+others
//@+node:gcross.20110206185121.1769: ** Typedefs
typedef boost::numeric::ublas::hermitian_matrix<std::complex<double> > HermitianMatrix;
//@+node:gcross.20110206092738.1740: ** Generators
//@+node:gcross.20110206092738.1741: *3* ComplexDoubleGenerator
struct ComplexDoubleGenerator {
    RNG& rng;
    ComplexDoubleGenerator(RNG& rng) : rng(rng) {}
    complex<double> operator()() { return c(rng.randomDouble(),rng.randomDouble()); }
};
//@+node:gcross.20110206092738.1749: *3* HermitianMatrixGenerator
struct HermitianMatrixGenerator {
    RNG& rng;
    HermitianMatrix m;
    unsigned int i, j;
    HermitianMatrixGenerator(RNG& rng,unsigned int const size)
      : rng(rng)
      , m(size,size)
      , i(0)
      , j(0)
    {
        resetMatrix();
    }
    void resetMatrix() {
        generate(m.data(),rng.randomComplexDouble);
        BOOST_FOREACH(size_t const k, irange((size_t)0,m.size1())) {
            m(k,k) = static_cast<complex<double> >(m(k,k)).real();
        }
    }

    complex<double> operator()() {
        if(j == m.size2()) {
            if(++i == m.size1()) {
                resetMatrix();
                i = 0;
            }
            j = 0;
        }
        return m(i,j++);
    }
};
//@+node:gcross.20110206092738.1742: *3* IntegerGenerator
struct IntegerGenerator {
    uniform_smallint<unsigned int> smallint;
    variate_generator<taus88,uniform_smallint<unsigned int> > randomInteger;
    IntegerGenerator(
          RNG& rng
        , unsigned int const lo
        , unsigned int const hi
    ) : smallint(lo,hi)
      , randomInteger(rng.generator,smallint)
    {}
    uint32_t operator()() { return (uint32_t)randomInteger(); }
};
//@+node:gcross.20110206092738.1744: *3* IndexGeneraor
struct IndexGenerator {
    bool left;
    IntegerGenerator left_index_generator, right_index_generator;
    IndexGenerator(
          RNG& rng
        , LeftDimension const left_index_bound
        , RightDimension const right_index_bound
    ) : left(false)
      , left_index_generator(rng,1,*left_index_bound)
      , right_index_generator(rng,1,*right_index_bound)
    {}
    uint32_t operator()() {
        left = !left;
        return left ? left_index_generator() : right_index_generator();
    }
};
//@+node:gcross.20110206092738.1738: ** struct RNG
//@+node:gcross.20110206092738.1739: *3* (constructors)
RNG::RNG()
  : normal(0,1)
  , smallint(1,10)
  , randomDouble(variate_generator<taus88,normal_distribution<double> >(generator,normal))
  , randomInteger(variate_generator<taus88,uniform_smallint<unsigned int> >(generator,smallint))
  , randomComplexDouble(ComplexDoubleGenerator(*this))
{}
//@+node:gcross.20110206092738.1747: *3* operator()
unsigned int RNG::operator()(unsigned int lo, unsigned int hi) {
    return generateRandomIntegers(lo,hi)();
}
//@+node:gcross.20110206092738.1745: *3* generateRandomIndices
function<uint32_t()> RNG::generateRandomIndices(
      LeftDimension const left_index_bound
    , RightDimension const right_index_bound
) {
    return IndexGenerator(*this,left_index_bound,right_index_bound);
}
//@+node:gcross.20110206092738.1743: *3* generateRandomIntegers
function<unsigned int()> RNG::generateRandomIntegers(unsigned int lo, unsigned int hi) {
    return IntegerGenerator(*this,lo,hi);
}
//@+node:gcross.20110206092738.1751: *3* generateRandomHermitianMatrices
function<complex<double>()> RNG::generateRandomHermitianMatrices(unsigned int const size) {
    return HermitianMatrixGenerator(*this,size);
}
//@+node:gcross.20110215135633.1866: *3* randomOperator
Operator RNG::randomOperator(
      optional<unsigned int> maybe_number_of_sites
    , unsigned int const maximum_physical_dimension
    , unsigned int const maximum_bandwidth_dimension

) {
    unsigned int const number_of_sites = maybe_number_of_sites ? *maybe_number_of_sites : randomInteger()+1;
    Operator operator_sites;
    unsigned int left_dimension = 1;
    BOOST_FOREACH(unsigned int const site_number, irange(0u,number_of_sites)) {
        unsigned int const right_dimension
            = site_number == number_of_sites-1
                ? 1
                : (*this)(1,maximum_bandwidth_dimension)
                ;
        operator_sites.push_back(boost::shared_ptr<OperatorSite const>(new OperatorSite(
            randomOperatorSite(
                 PhysicalDimension((*this)(1,maximum_physical_dimension))
                ,LeftDimension(left_dimension)
                ,RightDimension(right_dimension)
            )
        )));
        left_dimension = right_dimension;
    }
    return boost::move(operator_sites);
}
//@+node:gcross.20110206092738.1746: *3* randomOperatorSite
OperatorSite RNG::randomOperatorSite(
      PhysicalDimension const physical_dimension
    , LeftDimension const left_dimension
    , RightDimension const right_dimension
) {
    return OperatorSite(
             (*this)(1,2*(*left_dimension)*(*right_dimension))
            ,physical_dimension
            ,left_dimension
            ,right_dimension
            ,fillWithGenerator(generateRandomIndices(left_dimension,right_dimension))
            ,fillWithGenerator(generateRandomHermitianMatrices(*physical_dimension))
    );
}
//@+node:gcross.20110215135633.1863: *3* randomState
State RNG::randomState() {
    return randomState((*this)(1,5));
}

State RNG::randomState(unsigned int const number_of_sites) {
    assert(number_of_sites > 0);
    return randomState(randomUnsignedIntegerVector(number_of_sites));
}

State RNG::randomState(vector<unsigned int> const& physical_dimensions) {
    unsigned int const number_of_sites = physical_dimensions.size();
    unsigned int left_dimension = number_of_sites == 1 ? 1 : (*this);
    StateSite<Middle> first_state_site(
        randomStateSiteMiddle(
             PhysicalDimension(physical_dimensions[0])
            ,LeftDimension(1)
            ,RightDimension(left_dimension)
        )
    );
    vector<StateSite<Right> > rest_state_sites;
    BOOST_FOREACH(unsigned int const site_number, irange(1u,number_of_sites)) {
        unsigned int const right_dimension
            = site_number == number_of_sites-1
                ? 1
                : randomInteger()
                ;
        rest_state_sites.push_back(
            randomStateSiteRight(
                 PhysicalDimension(physical_dimensions[site_number])
                ,LeftDimension(left_dimension)
                ,RightDimension(right_dimension)
            )
        );
        left_dimension = right_dimension;
    }
    return State(boost::move(first_state_site),boost::move(rest_state_sites));
}
//@+node:gcross.20110217014932.1933: *3* randomUnsignedIntegerVector
vector<unsigned int> RNG::randomUnsignedIntegerVector(unsigned int n, unsigned int lo,unsigned int hi) {
    vector<unsigned int> physical_dimensions(n);
    generate(physical_dimensions,generateRandomIntegers(lo,hi));
    return boost::move(physical_dimensions);
}
//@-others
//@-leo
