//@+leo-ver=5-thin
//@+node:gcross.20110202223558.1713: * @thin test_utils.hpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110202223558.1715: ** << Includes >>
#include <boost/function.hpp>
#include <boost/optional.hpp>
#include <boost/random.hpp>
#include <boost/random/normal_distribution.hpp>
#include <boost/random/uniform_smallint.hpp>
#include <complex>

#include "operators.hpp"
#include "states.hpp"

using namespace Nutcracker;

using boost::function;
using boost::taus88;
using boost::none;
using boost::normal_distribution;
using boost::optional;
using boost::uniform_smallint;
//@-<< Includes >>

//@+others
//@+node:gcross.20110202223558.1714: ** struct RNG
class RNG {
    friend class ComplexDoubleGenerator;
    friend class IndexGenerator;
    friend class IntegerGenerator;
protected:
    taus88 generator;
    normal_distribution<double> normal;
    uniform_smallint<unsigned int> smallint;
public:
    function<double()> const randomDouble;
    function<unsigned int()> const randomInteger;
    function<complex<double>()> const randomComplexDouble;

    RNG();

    unsigned int operator()(unsigned int lo, unsigned int hi);
    OperatorSite randomOperatorSite(
          PhysicalDimension const physical_dimension
        , LeftDimension const left_dimension
        , RightDimension const right_dimension
    );
    Operator randomOperator(optional<unsigned int> maybe_number_of_sites=none);

    vector<unsigned int> randomUnsignedIntegerVector(unsigned int n, unsigned int lo=1,unsigned int hi=10);

    State randomState();
    State randomState(unsigned int number_of_sites);
    State randomState(vector<unsigned int> const& physical_dimensions);

    function<complex<double>()> generateRandomHermitianMatrices(unsigned int const size);
    function<unsigned int()> generateRandomIntegers(unsigned int lo, unsigned int hi);
    function<uint32_t()> generateRandomIndices(
          LeftDimension const left_index_bound
        , RightDimension const right_index_bound
    );

    operator unsigned int() { return randomInteger(); }
    operator complex<double>() { return randomComplexDouble(); }
};
//@-others
//@-leo
