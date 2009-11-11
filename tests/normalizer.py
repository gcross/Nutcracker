#@+leo-ver=4-thin
#@+node:gcross.20091110205054.1932:@thin normalizer.py
#@@language python
#@@tabwidth -4

import unittest
from paycheck import *

from numpy import *
from utils import crand

import vmps

#@+others
#@+node:gcross.20091110205054.1933:norm_denorm_going_left
class norm_denorm_going_left(unittest.TestCase):
    #@    @+others
    #@-others

    @with_checker
    def testCorrectness(self,bll=irange(2,4),bl=irange(2,4),br=irange(2,4)):
        dl = d = 2
        tensor_to_denormalize = crand(bl,bll,d)
        tensor_to_normalize = crand(br,bl,d)
        info, denormalized_tensor, normalized_tensor = vmps.normalizer.norm_denorm_going_left(tensor_to_denormalize,tensor_to_normalize)
        self.assertEqual(0,info)
        should_be_identity = tensordot(normalized_tensor.conj(),normalized_tensor,((0,2,),)*2)
        self.assertTrue(allclose(identity(bl),should_be_identity))
        self.assertTrue(allclose(
            tensordot(tensor_to_denormalize,tensor_to_normalize,(0,1)),
            tensordot(denormalized_tensor,normalized_tensor,(0,1)),
        ))
#@-node:gcross.20091110205054.1933:norm_denorm_going_left
#@+node:gcross.20091110205054.1937:norm_denorm_going_right
class norm_denorm_going_right(unittest.TestCase):
    #@    @+others
    #@-others

    @with_checker
    def testCorrectness(self,brr=irange(2,4),br=irange(2,4),bl=irange(2,4)):
        dr = d = 2
        tensor_to_normalize = crand(br,bl,d)
        tensor_to_denormalize = crand(brr,br,dr)
        info, normalized_tensor, denormalized_tensor = vmps.normalizer.norm_denorm_going_right(tensor_to_normalize,tensor_to_denormalize)
        self.assertEqual(0,info)
        should_be_identity = tensordot(normalized_tensor.conj(),normalized_tensor,((1,2,),)*2)
        self.assertTrue(allclose(identity(br),should_be_identity))
        self.assertTrue(allclose(
            tensordot(tensor_to_normalize,tensor_to_denormalize,(0,1)),
            tensordot(normalized_tensor,denormalized_tensor,(0,1)),
        ))
#@-node:gcross.20091110205054.1937:norm_denorm_going_right
#@-others

tests = [
    norm_denorm_going_left,
    norm_denorm_going_right,
    ]

if __name__ == "__main__":
    unittest.main()
#@-node:gcross.20091110205054.1932:@thin normalizer.py
#@-leo
