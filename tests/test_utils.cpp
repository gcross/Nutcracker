// Includes {{{
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

#include "nutcracker/utilities.hpp"

#include "test_utils.hpp"
// }}}

// Usings {{{
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
// }}}

typedef boost::numeric::ublas::hermitian_matrix<std::complex<double> > HermitianMatrix;

// Generators {{{

struct ComplexDoubleGenerator { // {{{
    RNG& rng;
    ComplexDoubleGenerator(RNG& rng) : rng(rng) {}
    complex<double> operator()() { return c(rng.randomDouble(),rng.randomDouble()); }
}; // }}}

struct HermitianMatrixGenerator { // {{{
    RNG& rng;
    HermitianMatrix m;
    unsigned int i, j;

    HermitianMatrixGenerator(RNG& rng,unsigned int const size) // {{{
      : rng(rng)
      , m(size,size)
      , i(0)
      , j(0)
    {
        resetMatrix();
    } // }}}

    void resetMatrix() { // {{{
        generate(m.data(),rng.randomComplexDouble);
        BOOST_FOREACH(size_t const k, irange((size_t)0,m.size1())) {
            m(k,k) = static_cast<complex<double> >(m(k,k)).real();
        }
    } // }}}

    complex<double> operator()() { // {{{
        if(j == m.size2()) {
            if(++i == m.size1()) {
                resetMatrix();
                i = 0;
            }
            j = 0;
        }
        return m(i,j++);
    } // }}}
}; // }}}

struct IntegerGenerator { // {{{
    uniform_smallint<unsigned int> smallint;

    variate_generator<taus88,uniform_smallint<unsigned int> > randomInteger;

    IntegerGenerator( // {{{
          RNG& rng
        , unsigned int const lo
        , unsigned int const hi
    ) : smallint(lo,hi)
      , randomInteger(rng.generator,smallint)
    {} // }}}

    uint32_t operator()() { return (uint32_t)randomInteger(); }
}; // }}}

struct IndexGenerator { // {{{
    bool left;

    IntegerGenerator left_index_generator, right_index_generator;

    IndexGenerator( // {{{
          RNG& rng
        , LeftDimension const left_index_bound
        , RightDimension const right_index_bound
    ) : left(false)
      , left_index_generator(rng,1,*left_index_bound)
      , right_index_generator(rng,1,*right_index_bound)
    {} // }}}

    uint32_t operator()() { // {{{
        left = !left;
        return left ? left_index_generator() : right_index_generator();
    } // }}}
}; // }}}

// function makeVariateGenerator {{{
template<typename Engine, typename Distribution>
variate_generator<Engine,Distribution>
makeVariateGenerator(
    Engine const& engine
  , Distribution const& distribution
)
{ return variate_generator<Engine,Distribution>(engine,distribution); }
// }}}

// }}}

// RNG methods {{{

RNG::RNG() // {{{
  : randomBoolean(makeVariateGenerator(generator,bernoulli_distribution<double>()))
  , randomDouble(makeVariateGenerator(generator,normal_distribution<double>(0,1)))
  , randomInteger(makeVariateGenerator(generator,uniform_smallint<unsigned int>(1,10)))
  , randomComplexDouble(ComplexDoubleGenerator(*this))
  , randomLowercaseLetter(makeVariateGenerator(generator,uniform_smallint<unsigned int>('a','z')))
{} // }}}

function<complex<double>()> RNG::generateRandomHermitianMatrices(unsigned int const size) { // {{{
    return HermitianMatrixGenerator(*this,size);
} // }}}

function<uint32_t()> RNG::generateRandomIndices( // {{{
      LeftDimension const left_index_bound
    , RightDimension const right_index_bound
) {
    return IndexGenerator(*this,left_index_bound,right_index_bound);
} // }}}

function<unsigned int()> RNG::generateRandomIntegers(unsigned int lo, unsigned int hi) { // {{{
    return IntegerGenerator(*this,lo,hi);
} // }}}

unsigned int RNG::operator()(unsigned int lo, unsigned int hi) { // {{{
    return generateRandomIntegers(lo,hi)();
} // }}}

InfiniteOperator RNG::randomInfiniteOperator( // {{{
      unsigned int const maximum_physical_dimension
    , unsigned int const maximum_bandwidth_dimension
) {
    PhysicalDimension const physical_dimension((*this)(1,maximum_physical_dimension));
    unsigned int const bandwidth_dimension = (*this)(1,maximum_bandwidth_dimension);
    LeftDimension const left_dimension(bandwidth_dimension);
    RightDimension const right_dimension(bandwidth_dimension);
    OperatorDimension const operator_dimension(bandwidth_dimension);

    return InfiniteOperator(
        OperatorBoundary<Left>(operator_dimension,fillWithGenerator(randomDouble)),
        randomOperatorSite(physical_dimension,left_dimension,right_dimension),
        OperatorBoundary<Right>(operator_dimension,fillWithGenerator(randomDouble))
    );
} // }}}

InfiniteChain RNG::randomInfiniteChain( // {{{
      unsigned int const maximum_physical_dimension
    , unsigned int const maximum_state_bandwidth_dimension
    , unsigned int const maximum_operator_bandwidth_dimension
) {
    InfiniteOperator op(randomInfiniteOperator(maximum_physical_dimension,maximum_operator_bandwidth_dimension));

    PhysicalDimension const physical_dimension(op.physicalDimension(as_dimension));
    OperatorDimension const operator_dimension(op.operatorDimension(as_dimension));

    unsigned int const state_bandwidth_dimension = (*this)(1,maximum_state_bandwidth_dimension);
    LeftDimension const left_dimension(state_bandwidth_dimension);
    RightDimension const right_dimension(state_bandwidth_dimension);
    StateDimension const state_dimension(state_bandwidth_dimension);

    InfiniteChain infinite_chain(
        boost::move(op),
        StateBoundary<Left>(state_dimension,fillWithGenerator(randomComplexDouble)),
        randomStateSiteMiddle(physical_dimension,left_dimension,right_dimension),
        StateBoundary<Right>(state_dimension,fillWithGenerator(randomComplexDouble))
    );

    return boost::move(infinite_chain);
} // }}}

MatrixPtr RNG::randomMatrix(unsigned int rows, unsigned int cols) { // {{{
    MatrixPtr const matrix(new Matrix(rows,cols));
    boost::generate(matrix->data(),randomComplexDouble);
    return matrix;
} // }}}

MatrixPtr RNG::randomHermitianMatrix(unsigned int dimension) { // {{{
    MatrixPtr const matrix(new Matrix(dimension,dimension));
    for(unsigned int i = 0; i < dimension; ++i) {
        (*matrix)(i,i) = randomDouble();
        for(unsigned int j = i+1; j < dimension; ++j) {
            (*matrix)(i,j) = randomComplexDouble();
            (*matrix)(j,i) = conj((*matrix)(i,j));
        }
    }
    return matrix;
} // }}}

Operator RNG::randomOperator( // {{{
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
} // }}}

OperatorSite RNG::randomOperatorSite( // {{{
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
} // }}}

OperatorSite RNG::randomOperatorSite() { // {{{
    return randomOperatorSite(
        PhysicalDimension((*this)(1,5)),
        LeftDimension((*this)(1,5)),
        RightDimension((*this)(1,5))
    );
} // }}}

MatrixPtr RNG::randomSquareMatrix(unsigned int dimension) { // {{{
    return randomMatrix(dimension,dimension);
} // }}}

State RNG::randomState() { // {{{
    return randomState((*this)(1,5));
} // }}}

State RNG::randomState(unsigned int const number_of_sites) { // {{{
    assert(number_of_sites > 0);
    return randomState(randomUnsignedIntegerVector(number_of_sites));
} // }}}

State RNG::randomState(vector<unsigned int> const& physical_dimensions) { // {{{
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
} // }}}

TemporaryFilepath RNG::randomTemporaryFilepath(string suffix) const { // {{{
    string random_data(5,0);
    generate(random_data,randomLowercaseLetter);
    return TemporaryFilepath(random_data + suffix);
} // }}}

vector<unsigned int> RNG::randomUnsignedIntegerVector(unsigned int n, unsigned int lo,unsigned int hi) { // {{{
    vector<unsigned int> physical_dimensions(n);
    generate(physical_dimensions,generateRandomIntegers(lo,hi));
    return boost::move(physical_dimensions);
} // }}}

VectorPtr RNG::randomVector() { // {{{
    return randomVector(*this);
} // }}}

VectorPtr RNG::randomVector(unsigned int size) { // {{{
    VectorPtr data = make_shared<Vector>(size);
    generate(*data,randomComplexDouble);
    return data;
} // }}}

// }}}

// TemporaryFilepath methods {{{

TemporaryFilepath::TemporaryFilepath() {}

TemporaryFilepath::TemporaryFilepath(path const& filepath) // {{{
  : filepath(filepath)
{ if(exists(filepath)) remove(filepath); }
// }}}

TemporaryFilepath::~TemporaryFilepath() { if(exists(filepath)) remove(filepath); }

path const& TemporaryFilepath::operator*() const { return filepath; }

path const* TemporaryFilepath::operator->() const { return &filepath; }

// }}}

void checkOperatorsEqual(Operator const& operator_1,Operator const& operator_2) { // {{{
    ASSERT_EQ(operator_1.size(),operator_2.size());
    BOOST_FOREACH(unsigned int const i, irange(0u,(unsigned int)operator_1.size())) {
        checkOperatorSitesEqual(*operator_1[i],*operator_2[i]);
    }
} // }}}

void checkOperatorsEquivalent( // {{{
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
        ASSERT_NEAR_REL(computeExpectationValue(state,operator_1),computeExpectationValue(state,operator_2),1e-10)
    }
} // }}}

void checkOperatorSitesEqual(OperatorSite const& operator_site_1,OperatorSite const& operator_site_2) { // {{{
    ASSERT_EQ(operator_site_1.physicalDimension(),operator_site_2.physicalDimension());
    ASSERT_EQ(operator_site_1.leftDimension(),operator_site_2.leftDimension());
    ASSERT_EQ(operator_site_1.rightDimension(),operator_site_2.rightDimension());
    ASSERT_EQ(operator_site_1.numberOfMatrices(),operator_site_2.numberOfMatrices());
    ASSERT_TRUE(equal(operator_site_1,operator_site_2));
    ASSERT_TRUE(equal((uint32_t const*)operator_site_1,((uint32_t const*)operator_site_1)+2*operator_site_1.numberOfMatrices(),(uint32_t const*)operator_site_2));
} // }}}

void checkSiteTensorsEqual(SiteBaseTensor const& site_1,SiteBaseTensor const& site_2) { // {{{
    ASSERT_EQ(site_1.physicalDimension(),site_2.physicalDimension());
    ASSERT_EQ(site_1.leftDimension(),site_2.leftDimension());
    ASSERT_EQ(site_1.rightDimension(),site_2.rightDimension());
    ASSERT_TRUE(equal(site_1,site_2));
} // }}}

void checkStatesEqual(State const& state_1,State const& state_2) { // {{{
    ASSERT_EQ(state_1.numberOfSites(),state_2.numberOfSites());
    for(State::iterator
            state_iter_1 = state_1.begin(),
            state_iter_2 = state_1.begin();
        state_iter_1 != state_1.end();
        ++state_iter_1, ++state_iter_2
    ) checkSiteTensorsEqual(*state_iter_1,*state_iter_2);
} // }}}
