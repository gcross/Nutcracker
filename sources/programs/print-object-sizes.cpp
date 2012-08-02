//@+leo-ver=5-thin
//@+node:gcross.20110511141322.2238: * @file print-object-sizes.cpp
//@@language cplusplus
//@+<< Includes >>
//@+node:gcross.20110511141322.2240: ** << Includes >>
#include <iostream>

#include "nutcracker/tensors.hpp"

using namespace Nutcracker;
using namespace std;
//@-<< Includes >>

//@+others
//@+node:gcross.20110511141322.2243: ** main
int main(int argc, char** argv) {
    cout << "BaseTensor: " << sizeof(BaseTensor) << endl;
    cout << "ExpectationBoundary: " << sizeof(ExpectationBoundary<Left>) << endl;
    cout << "OverlapBoundary: " << sizeof(OverlapBoundary<Left>) << endl;
    cout << "SiteBaseTensor: " << sizeof(SiteBaseTensor) << endl;
    cout << "OperatorSite: " << sizeof(OperatorSite) << endl;
    cout << "StateSiteAny: " << sizeof(StateSiteAny) << endl;
    cout << "StateSite: " << sizeof(StateSite<Left>) << endl;
    cout << "OverlapSiteAny: " << sizeof(OverlapSiteAny) << endl;
    cout << "OverlapSite: " << sizeof(OverlapSite<Left>) << endl;
}
//@-others
//@-leo
