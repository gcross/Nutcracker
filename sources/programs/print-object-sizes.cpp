#include <iostream>

#include "nutcracker/tensors.hpp"

using namespace Nutcracker;
using namespace std;

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
