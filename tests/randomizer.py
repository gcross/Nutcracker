#@+leo-ver=4-thin
#@+node:gcross.20091110205054.1923:@thin randomizer.py
#@@language python
#@@tabwidth -4

import unittest
from paycheck import *

from numpy import *

import vmps

#@+others
#@+node:gcross.20091110205054.1924:rand_norm_state_site_tensor
class rand_norm_state_site_tensor(unittest.TestCase):
    #@    @+others
    #@-others

    @with_checker
    def testCorrectness(self,bl=irange(2,4),br=irange(2,4)):
        d = 2
        info, normalized_tensor = vmps.randomizer.rand_norm_state_site_tensor(bl,br,d)
        self.assertEqual(0,info)
        should_be_identity = tensordot(normalized_tensor.conj(),normalized_tensor,((0,2,),)*2)
        self.assertTrue(allclose(identity(bl),should_be_identity))
#@-node:gcross.20091110205054.1924:rand_norm_state_site_tensor
#@-others

tests = [
    rand_norm_state_site_tensor,
    ]

if __name__ == "__main__":
    unittest.main()
#@-node:gcross.20091110205054.1923:@thin randomizer.py
#@-leo
