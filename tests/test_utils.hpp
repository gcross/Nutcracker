//@+leo-ver=5-thin
//@+node:gcross.20110202223558.1713: * @thin test_utils.hpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110202223558.1715: ** << Includes >>
#include <boost/function.hpp>
#include <boost/random.hpp>
#include <boost/random/normal_distribution.hpp>
#include <boost/random/uniform_smallint.hpp>
#include <complex>

#include "tensors.hpp"

using namespace boost;
using namespace Nutcracker;
using namespace std;
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
    OperatorSite randomOperator(
          PhysicalDimension const physical_dimension
        , LeftDimension const left_dimension
        , RightDimension const right_dimension
    );

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
