// Includes {{{
#include <boost/filesystem.hpp>
#include <boost/function.hpp>
#include <boost/make_shared.hpp>
#include <boost/optional.hpp>
#include <boost/random.hpp>
#include <complex>

#include "nutcracker/infinite_chain.hpp"
#include "nutcracker/infinite_operators.hpp"
#include "nutcracker/operators.hpp"
#include "nutcracker/states.hpp"
// }}}

using namespace Nutcracker;

// Usings {{{
using boost::filesystem::path;
using boost::function;
using boost::make_shared;
using boost::none;
using boost::optional;
using boost::taus88;

using std::abs;
// }}}

class TemporaryFilepath { // {{{
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(TemporaryFilepath)
protected:
    path filepath;
public:
    TemporaryFilepath();
    TemporaryFilepath(path const& filepath);

    TemporaryFilepath(BOOST_RV_REF(TemporaryFilepath) other) {
        if(exists(filepath)) remove(filepath);
        filepath = other.filepath;
        other.filepath.clear();
    }

    ~TemporaryFilepath();

    path const& operator*() const;
    path const* operator->() const;
}; // }}}

class RNG { // {{{
    friend class ComplexDoubleGenerator;
    friend class IndexGenerator;
    friend class IntegerGenerator;
protected:
    taus88 generator;
public:
    function<bool()> const randomBoolean;
    function<double()> const randomDouble;
    function<unsigned int()> const randomInteger;
    function<complex<double>()> const randomComplexDouble;
    function<char()> const randomLowercaseLetter;

    RNG();

    unsigned int operator()(unsigned int lo, unsigned int hi);

    OperatorSite randomOperatorSite(
          PhysicalDimension const physical_dimension
        , LeftDimension const left_dimension
        , RightDimension const right_dimension
    );
    OperatorSite randomOperatorSite();

    Operator randomOperator(
          optional<unsigned int> maybe_number_of_sites=none
        , unsigned int const maximum_physical_dimension=10
        , unsigned int const maximum_bandwidth_dimension=10
    );

    InfiniteOperator randomInfiniteOperator(
          unsigned int const maximum_physical_dimension=10
        , unsigned int const maximum_state_bandwidth_dimension=10
    );

    InfiniteChain randomInfiniteChain( // {{{
          unsigned int const maximum_physical_dimension=10
        , unsigned int const maximum_state_bandwidth_dimension=10
        , unsigned int const maximum_operator_bandwidth_dimension=10
    );

    MatrixPtr randomMatrix(unsigned int rows, unsigned int cols);
    MatrixPtr randomSquareMatrix(unsigned int dimension);
    VectorPtr randomVector();
    VectorPtr randomVector(unsigned int size);
 
    vector<unsigned int> randomUnsignedIntegerVector(unsigned int n, unsigned int lo=1,unsigned int hi=10);

    State randomState();
    State randomState(unsigned int number_of_sites);
    State randomState(vector<unsigned int> const& physical_dimensions);

    TemporaryFilepath randomTemporaryFilepath(string suffix) const;

    function<complex<double>()> generateRandomHermitianMatrices(unsigned int const size);
    function<unsigned int()> generateRandomIntegers(unsigned int lo, unsigned int hi);
    function<uint32_t()> generateRandomIndices(
          LeftDimension const left_index_bound
        , RightDimension const right_index_bound
    );

    operator unsigned int() { return randomInteger(); }
    operator complex<double>() { return randomComplexDouble(); }
}; // }}}

// Functions {{{
void checkOperatorsEqual(Operator const& operator_1,Operator const& operator_2);

void checkOperatorsEquivalent(Operator const& operator_1,Operator const& operator_2,RNG& random,unsigned int number_of_samples=20);

void checkOperatorSitesEqual(OperatorSite const& operator_site_1,OperatorSite const& operator_site_2);

void checkSiteTensorsEqual(SiteBaseTensor const& site_1,SiteBaseTensor const& site_2);

void checkStatesEqual(State const& state_1,State const& state_2);

template<typename Range> VectorPtr vectorFromRange(Range const& range) { // {{{
    VectorPtr vector = make_shared<Vector>(range.size());
    copy(range,vector->begin());
    return vector;
} // }}}
// }}}
