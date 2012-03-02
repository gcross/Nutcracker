# Imports {{{
from .._1d.tensors import *
from ..data import *

from . import *
from paycheck import *
# }}} Imports

# Tests {{{

class TestExpectation(TestCase): # {{{
    @with_checker # test_trivial {{{
    def test_trivial(self,absorptions=[bool]):
        l = LeftExpectationBoundary.newTrivial(NDArrayData)
        r = RightExpectationBoundary.newTrivial(NDArrayData)
        s = StateSite.newTrivial(NDArrayData)
        o = OperatorSite.newTrivial(NDArrayData)
        for absorb_left in absorptions:
            if absorb_left:
                l = l.absorb(s,o)
            else:
                r = r.absorb(s,o)
        self.assertEqual(1,l.contractWithRightBoundary(r))
        self.assertEqual(1,r.contractWithLeftBoundary(l))
    # }}}
# }}}

# }}} Tests
