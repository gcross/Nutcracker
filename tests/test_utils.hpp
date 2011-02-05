//@+leo-ver=5-thin
//@+node:gcross.20110202223558.1713: * @thin test_utils.hpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110202223558.1715: ** << Includes >>
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
struct RNG {
    taus88 generator;

    normal_distribution<double> normal;
    variate_generator<taus88,normal_distribution<double> > randomDouble;

    uniform_smallint<unsigned int> smallint;
    variate_generator<taus88,uniform_smallint<unsigned int> > randomInteger;

    template<typename T> struct Generator {
        RNG& rng;
        Generator(RNG& rng) : rng(rng) {}
        T operator()() { return c(rng.randomDouble(),rng.randomDouble()); }
    };

    Generator<complex<double> > randomComplexDouble;

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

    IntegerGenerator operator()(unsigned int lo, unsigned int hi) {
        return IntegerGenerator(*this,lo,hi);
    }

    struct IndexGenerator {
        bool left;
        IntegerGenerator left_index_generator, right_index_generator;
        IndexGenerator(
              RNG& rng
            , unsigned int const left_index_bound
            , unsigned int const right_index_bound
        ) : left(false)
          , left_index_generator(rng,1,left_index_bound)
          , right_index_generator(rng,1,right_index_bound)
        {}
        uint32_t operator()() {
            left = !left;
            return left ? left_index_generator() : right_index_generator();
        }
    };

    RNG()
      : normal(0,1)
      , randomDouble(generator,normal)
      , smallint(1,10)
      , randomInteger(generator,smallint)
      , randomComplexDouble(*this)
    {}

    operator unsigned int() { return randomInteger(); }
    operator complex<double>() { return randomComplexDouble(); }

    OperatorSite randomOperator(
          PhysicalDimension const physical_dimension
        , LeftDimension const left_dimension
        , RightDimension const right_dimension
    ) {
        IndexGenerator index_generator(*this,(*left_dimension),(*right_dimension));
        return OperatorSite(
                 (*this)(1,2*(*left_dimension)*(*right_dimension))()
                ,physical_dimension
                ,left_dimension
                ,right_dimension
                ,fillWithGenerator(index_generator)
                ,fillWithGenerator(randomComplexDouble)
        );
    }
};
//@-others
//@-leo
