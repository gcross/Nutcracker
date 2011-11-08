#@+leo-ver=5-thin
#@+node:gcross.20111108100704.1389: * @file test_utils.py
#@+<< Imports >>
#@+node:gcross.20111108100704.1390: ** << Imports >>
from paycheck import *

from nutcracker.utils import normalize

from nutcracker.tests import *
#@-<< Imports >>

#@+others
#@+node:gcross.20111108100704.1391: ** Tests
#@+node:gcross.20111108100704.1392: *3* normalize
class TestNormalize(TestCase):
    @with_checker
    def test_correctness(self,number_of_dimensions = irange(2,5)):
        tensor, index = randomNormalizableTensorAndIndex(number_of_dimensions)
        self.assertNormalized(normalize(tensor,index),index)
#@-others
#@-leo
