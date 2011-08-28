//@+leo-ver=5-thin
//@+node:gcross.20110206092738.1736: * @file test_utils.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110220182654.2064: ** << License >>
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
//@+node:gcross.20110206092738.1737: ** << Includes >>
#include <algorithm>
#include <boost/random/bernoulli_distribution.hpp>
#include <boost/random/normal_distribution.hpp>
#include <boost/random/uniform_smallint.hpp>
#include <boost/range/adaptor/indirected.hpp>
#include <boost/range/adaptor/transformed.hpp>
#include <boost/range/algorithm/equal.hpp>
#include <boost/range/algorithm/generate.hpp>
#include <boost/range/irange.hpp>
#include <boost/numeric/ublas/hermitian.hpp>
#include <illuminate.hpp>

#include "utilities.hpp"

#include "test_utils.hpp"

using boost::adaptors::indirected;
using boost::adaptors::transformed;
using boost::bernoulli_distribution;
using boost::equal;
using boost::filesystem::exists;
using boost::filesystem::remove;
using boost::irange;
using boost::generate;
using boost::normal_distribution;
using boost::uniform_smallint;
using boost::variate_generator;

using std::equal;
using std::max;
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
//@+node:gcross.20110511190907.3793: ** Functions
//@+node:gcross.20110511190907.3794: *3* makeVariateGenerator
template<typename Engine, typename Distribution>
variate_generator<Engine,Distribution>
makeVariateGenerator(
    Engine const& engine
  , Distribution const& distribution
)
{ return variate_generator<Engine,Distribution>(engine,distribution); }
//@+node:gcross.20110511190907.3825: ** Classes
//@+node:gcross.20110206092738.1738: *3* RNG
//@+node:gcross.20110206092738.1739: *4* (constructors)
RNG::RNG()
  : randomBoolean(makeVariateGenerator(generator,bernoulli_distribution<double>()))
  , randomDouble(makeVariateGenerator(generator,normal_distribution<double>(0,1)))
  , randomInteger(makeVariateGenerator(generator,uniform_smallint<unsigned int>(1,10)))
  , randomComplexDouble(ComplexDoubleGenerator(*this))
  , randomLowercaseLetter(makeVariateGenerator(generator,uniform_smallint<unsigned int>('a','z')))
{}
//@+node:gcross.20110206092738.1751: *4* generateRandomHermitianMatrices
function<complex<double>()> RNG::generateRandomHermitianMatrices(unsigned int const size) {
    return HermitianMatrixGenerator(*this,size);
}
//@+node:gcross.20110206092738.1745: *4* generateRandomIndices
function<uint32_t()> RNG::generateRandomIndices(
      LeftDimension const left_index_bound
    , RightDimension const right_index_bound
) {
    return IndexGenerator(*this,left_index_bound,right_index_bound);
}
//@+node:gcross.20110206092738.1743: *4* generateRandomIntegers
function<unsigned int()> RNG::generateRandomIntegers(unsigned int lo, unsigned int hi) {
    return IntegerGenerator(*this,lo,hi);
}
//@+node:gcross.20110206092738.1747: *4* operator()
unsigned int RNG::operator()(unsigned int lo, unsigned int hi) {
    return generateRandomIntegers(lo,hi)();
}
//@+node:gcross.20110818221240.2504: *4* randomMatrix
MatrixPtr RNG::randomMatrix(unsigned int rows, unsigned int cols) {
    MatrixPtr const matrix(new Matrix(rows,cols));
    boost::generate(matrix->data(),randomComplexDouble);
    return matrix;
}
//@+node:gcross.20110215135633.1866: *4* randomOperator
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
//@+node:gcross.20110206092738.1746: *4* randomOperatorSite
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
OperatorSite RNG::randomOperatorSite() {
    return randomOperatorSite(
        PhysicalDimension((*this)(1,5)),
        LeftDimension((*this)(1,5)),
        RightDimension((*this)(1,5))
    );
}
//@+node:gcross.20110818221240.2514: *4* randomSquareMatrix
MatrixPtr RNG::randomSquareMatrix(unsigned int dimension) {
    return randomMatrix(dimension,dimension);
}
//@+node:gcross.20110215135633.1863: *4* randomState
State RNG::randomState() {
    return randomState((*this)(1,5));
}

State RNG::randomState(unsigned int const number_of_sites) {
    assert(number_of_sites > 0);
    return randomState(randomUnsignedIntegerVector(number_of_sites));
}

State RNG::randomState(vector<unsigned int> const& physical_dimensions) {
    unsigned int const number_of_sites = physical_dimensions.size();
    vector<unsigned int> const bandwidth_dimensions = computeBandwidthDimensionSequence((*this)(1,maximumBandwidthDimension(physical_dimensions)),physical_dimensions);
    StateSite<Middle> first_state_site(
        randomStateSiteMiddle(
             PhysicalDimension(physical_dimensions[0])
            ,LeftDimension(bandwidth_dimensions[0])
            ,RightDimension(bandwidth_dimensions[1])
        )
    );
    vector<StateSite<Right> > rest_state_sites;
    BOOST_FOREACH(unsigned int const site_number, irange(1u,number_of_sites)) {
        rest_state_sites.push_back(
            randomStateSiteRight(
                 PhysicalDimension(physical_dimensions[site_number])
                ,LeftDimension(bandwidth_dimensions[site_number])
                ,RightDimension(bandwidth_dimensions[site_number+1])
            )
        );
    }
    return State(boost::move(first_state_site),boost::move(rest_state_sites));
}
//@+node:gcross.20110511190907.3830: *4* randomTemporaryFilepath
TemporaryFilepath RNG::randomTemporaryFilepath(string suffix) const {
    string random_data(5,0);
    generate(random_data,randomLowercaseLetter);
    return TemporaryFilepath(random_data + suffix);
}
//@+node:gcross.20110217014932.1933: *4* randomUnsignedIntegerVector
vector<unsigned int> RNG::randomUnsignedIntegerVector(unsigned int n, unsigned int lo,unsigned int hi) {
    vector<unsigned int> physical_dimensions(n);
    generate(physical_dimensions,generateRandomIntegers(lo,hi));
    return boost::move(physical_dimensions);
}
//@+node:gcross.20110827215622.2614: *4* randomVector
VectorPtr RNG::randomVector() {
    return randomVector(*this);
}

VectorPtr RNG::randomVector(unsigned int size) {
    VectorPtr data = make_shared<Vector>(size);
    generate(*data,randomComplexDouble);
    return data;
}
//@+node:gcross.20110511190907.3826: *3* TemporaryFilepath
//@+node:gcross.20110511190907.3827: *4* (constructors)
TemporaryFilepath::TemporaryFilepath() {}

TemporaryFilepath::TemporaryFilepath(path const& filepath)
  : filepath(filepath)
{ if(exists(filepath)) remove(filepath); }

TemporaryFilepath::TemporaryFilepath(BOOST_RV_REF(TemporaryFilepath) other) {
    if(exists(filepath)) remove(filepath);
    filepath = other.filepath;
    other.filepath.clear();
}
//@+node:gcross.20110511190907.3828: *4* (destructors)
TemporaryFilepath::~TemporaryFilepath() { if(exists(filepath)) remove(filepath); }
//@+node:gcross.20110511190907.3829: *4* operator*/->
path const& TemporaryFilepath::operator*() const { return filepath; }
path const* TemporaryFilepath::operator->() const { return &filepath; }
//@+node:gcross.20110430221653.2182: ** Functions
//@+node:gcross.20110430221653.2184: *3* checkOperatorsEqual
void checkOperatorsEqual(Operator const& operator_1,Operator const& operator_2) {
    ASSERT_EQ(operator_1.size(),operator_2.size());
    BOOST_FOREACH(unsigned int const i, irange(0u,(unsigned int)operator_1.size())) {
        checkOperatorSitesEqual(*operator_1[i],*operator_2[i]);
    }
}
//@+node:gcross.20110818221240.2501: *3* checkOperatorsEquivalent
void checkOperatorsEquivalent(
    Operator const& operator_1,
    Operator const& operator_2,
    RNG& random,
    unsigned int number_of_samples
) {
    ASSERT_EQ(operator_1.size(),operator_2.size())
    vector<unsigned int> physical_dimensions(operator_1.size());
    copy(operator_1 | indirected | transformed(bind(&OperatorSite::physicalDimension,_1)),physical_dimensions.begin());
    REPEAT(number_of_samples) {
        State state = random.randomState(physical_dimensions);
        ASSERT_NEAR_REL(computeExpectationValue(state,operator_1),computeExpectationValue(state,operator_2),1e-13)
    }
}
//@+node:gcross.20110430221653.2183: *3* checkOperatorSitesEqual
void checkOperatorSitesEqual(OperatorSite const& operator_site_1,OperatorSite const& operator_site_2) {
    ASSERT_EQ(operator_site_1.physicalDimension(),operator_site_2.physicalDimension());
    ASSERT_EQ(operator_site_1.leftDimension(),operator_site_2.leftDimension());
    ASSERT_EQ(operator_site_1.rightDimension(),operator_site_2.rightDimension());
    ASSERT_EQ(operator_site_1.numberOfMatrices(),operator_site_2.numberOfMatrices());
    ASSERT_TRUE(equal(operator_site_1,operator_site_2));
    ASSERT_TRUE(equal((uint32_t const*)operator_site_1,((uint32_t const*)operator_site_1)+2*operator_site_1.numberOfMatrices(),(uint32_t const*)operator_site_2));
}
//@+node:gcross.20110511190907.2330: *3* checkSiteTensorsEqual
void checkSiteTensorsEqual(SiteBaseTensor const& site_1,SiteBaseTensor const& site_2) {
    ASSERT_EQ(site_1.physicalDimension(),site_2.physicalDimension());
    ASSERT_EQ(site_1.leftDimension(),site_2.leftDimension());
    ASSERT_EQ(site_1.rightDimension(),site_2.rightDimension());
    ASSERT_TRUE(equal(site_1,site_2));
}
//@+node:gcross.20110511190907.3527: *3* checkStatesEqual
void checkStatesEqual(State const& state_1,State const& state_2) {
    ASSERT_EQ(state_1.numberOfSites(),state_2.numberOfSites());
    for(State::iterator
            state_iter_1 = state_1.begin(),
            state_iter_2 = state_1.begin();
        state_iter_1 != state_1.end();
        ++state_iter_1, ++state_iter_2
    ) checkSiteTensorsEqual(*state_iter_1,*state_iter_2);
}
//@-others
//@-leo
