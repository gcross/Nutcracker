//@+leo-ver=5-thin
//@+node:gcross.20110127123226.2514: * @thin tensors.cpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110127123226.2515: ** << Includes >>
#include "tensors.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110127123226.2516: ** << Usings >>
//@-<< Usings >>

//@+others
//@+node:gcross.20110127123226.2517: ** Values
OperatorSite const OperatorSite::trivial
    (LeftDimension(1)
    ,RightDimension(1)
    ,fillWithRange(list_of(1)(1))
    ,fillWithRange(list_of(1))
    );
//@-others

}
//@-leo
