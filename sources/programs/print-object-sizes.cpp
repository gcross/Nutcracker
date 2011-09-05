//@+leo-ver=5-thin
//@+node:gcross.20110511141322.2238: * @file print-object-sizes.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110511141322.2239: ** << License >>
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
