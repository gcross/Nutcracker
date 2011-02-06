//@+leo-ver=5-thin
//@+node:gcross.20110206092738.1736: * @thin test_utils.cpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110206092738.1737: ** << Includes >>
#include "test_utils.hpp"
//@-<< Includes >>

//@+others
//@+node:gcross.20110206092738.1738: ** struct RNG
//@+node:gcross.20110206092738.1740: *3* (generators)
//@+node:gcross.20110206092738.1741: *4* ComplexDoubleGenerator
struct ComplexDoubleGenerator {
    RNG& rng;
    ComplexDoubleGenerator(RNG& rng) : rng(rng) {}
    complex<double> operator()() { return c(rng.randomDouble(),rng.randomDouble()); }
};
//@+node:gcross.20110206092738.1742: *4* IntegerGenerator
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
//@+node:gcross.20110206092738.1744: *4* IndexGeneraor
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
//@+node:gcross.20110206092738.1746: *3* randomOperator
OperatorSite RNG::randomOperator(
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
            ,fillWithGenerator(randomComplexDouble)
    );
}
//@-others
//@-leo
