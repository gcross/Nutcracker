#@+leo-ver=4-thin
#@+node:gcross.20090930124443.1284:@thin chain_contractors.py
#@@language Python

#@<< Import needed modules >>
#@+node:gcross.20090930134608.1306:<< Import needed modules >>
from utils import make_contractor_from_implicit_joins
from numpy import inner
#@-node:gcross.20090930134608.1306:<< Import needed modules >>
#@nl

#@+others
#@+node:gcross.20090930124443.1294:Functions
#@+node:gcross.20090930124443.1285:Contractors
#@+node:gcross.20090930124443.1286:Absorb site expectation into left boundary
#@+at
# 
# L = left boundary
# S = state site tensor
# S* = state site tensor (conjugated)
# O = operator site tensor
# 
# /-1--S*-21
# |    |
# |    32
# |    |
# L-2--O--22
# |    |
# |    42
# |    |
# \-3--S--23
# 
#@-at
#@@c

contract_site_expectation_into_left_boundary = make_contractor_from_implicit_joins([
    [1,2,3],       # left environment
    [32,1,21],     # state site tensor (conjugated)
    [32,42,2,22],  # operator site tensor
    [42,3,23],     # state site tensor
],[
    21,
    22,
    23,
])
#@-node:gcross.20090930124443.1286:Absorb site expectation into left boundary
#@+node:gcross.20090930124443.1290:Absorb site contraction into left boundary
#@+at
# 
# L = left boundary
# A* = state A site tensor (conjugated)
# B = state B site tensor
# 
# /-1--A*-21
# |    |
# L    2
# |    |
# \-3--B--23
# 
#@-at
#@@c

contract_site_overlap_into_left_boundary = make_contractor_from_implicit_joins([
    [1,3],     # left boundary
    [2,1,21],  # state A site tensor (conjugated)
    [2,3,23],  # state B site tensor
],[
    21,
    23,
])
#@-node:gcross.20090930124443.1290:Absorb site contraction into left boundary
#@+node:gcross.20090930124443.1288:Absorb site expectation into right boundary
#@+at
# 
# R = right boundary
# S = state site tensor
# S* = state site tensor (conjugated)
# O = operator site tensor
# 
# 1--S*-21-\
#    |     |
#    32    |
#    |     |
# 2--O--22-R
#    |     |
#    42    |
#    |     |
# 3--S--23-/
# 
#@-at
#@@c

contract_site_expectation_into_right_boundary = make_contractor_from_implicit_joins([
    [21,22,23],    # right boundary
    [32,1,21],     # state site tensor (conjugated)
    [32,42,2,22],  # operator site tensor
    [42,3,23],     # state site tensor
],[
    1,
    2,
    3,
])
#@-node:gcross.20090930124443.1288:Absorb site expectation into right boundary
#@+node:gcross.20090930124443.1292:Absorb site contraction into right boundary
#@+at
# 
# R = left boundary
# A* = state A site tensor (conjugated)
# B = state B site tensor
# 
# 1--A*-21-\
#    |     |
#    2     R
#    |     |
# 3--B--23-/
# 
#@-at
#@@c

contract_site_overlap_into_right_boundary = make_contractor_from_implicit_joins([
    [21,23],   # right boundary
    [2,1,21],  # state A site tensor (conjugated)
    [2,3,23],  # state B site tensor
],[
    1,
    3,
])
#@-node:gcross.20090930124443.1292:Absorb site contraction into right boundary
#@+node:gcross.20090930134608.1334:Partial contraction for expectation
#@+at
# 
# L = left boundary
# R = left boundary
# S = state site tensor
# O = operator site tensor
# 
# /-1-   -21-\
# |    |     |
# |    32    |
# |    |     |
# L-2--O--22-R
# |    |     |
# |    42    |
# |    |     |
# \-3--S--23-/
# 
#@-at
#@@c

partially_contract_expectation = make_contractor_from_implicit_joins([
    [42,3,23],     # state site tensor
    [32,42,2,22],  # operator site tensor
    [1,2,3],       # left boundary tensor
    [21,22,23],    # right boundary tensor
],[
    32,
    1,
    21,
])
#@-node:gcross.20090930134608.1334:Partial contraction for expectation
#@+node:gcross.20090930134608.1336:Partial contraction for overlap
#@+at
# 
# L = left boundary
# R = left boundary
# S = state site tensor
# O = operator site tensor
# 
# /-1-   -21-\
# |    |     |
# |    42    |
# |    |     |
# \-3--S--23-/
# 
#@-at
#@@c

partially_contract_overlap = make_contractor_from_implicit_joins([
    [42,3,23],     # state site tensor
    [1,3],       # left boundary tensor
    [21,23],    # right boundary tensor
],[
    42,
    1,
    21,
])
#@-node:gcross.20090930134608.1336:Partial contraction for overlap
#@-node:gcross.20090930124443.1285:Contractors
#@-node:gcross.20090930124443.1294:Functions
#@+node:gcross.20090930124443.1293:Classes
#@+others
#@+node:gcross.20090930124443.1295:ChainContractorForExpectations
class ChainContractorForExpectations(object):
    #@    @+others
    #@+node:gcross.20090930134608.1289:__slots__
    __slots__ = [
        "left_boundary","left_boundary_stack","left_operator_stack",
        "right_boundary","right_boundary_stack","right_operator_stack",
        "operator_site_tensor",
        "number_of_sites","current_site_number"
    ]
    #@-node:gcross.20090930134608.1289:__slots__
    #@+node:gcross.20090930134608.1273:__init__
    def __init__(self,initial_left_boundary,initial_right_boundary,initial_state_site_tensors,operator_site_tensors):
        self.initialize(initial_left_boundary,initial_right_boundary,len(initial_state_site_tensors))
        assert(self.number_of_sites == len(operator_site_tensors))
        for i in xrange(self.number_of_sites-1,0,-1):
            self.operator_site_tensor = operator_site_tensors[i]
            self.push_right_boundary()
            self.right_boundary = self.contract_into_right_boundary(initial_state_site_tensors[i].conj(),initial_state_site_tensors[i])
        self.operator_site_tensor = operator_site_tensors[0]
    #@-node:gcross.20090930134608.1273:__init__
    #@+node:gcross.20090930134608.1323:initialize
    def initialize(self,initial_left_boundary,initial_right_boundary,number_of_sites):
        self.left_boundary = initial_left_boundary
        self.right_boundary = initial_right_boundary
        self.number_of_sites = number_of_sites
        self.current_site_number = 0
        self.left_boundary_stack = []
        self.left_operator_stack = []
        self.right_boundary_stack = []
        self.right_operator_stack = []
    #@-node:gcross.20090930134608.1323:initialize
    #@+node:gcross.20090930134608.1263:push_XXX_boundary
    def push_left_boundary(self):
        self.left_boundary_stack.append(self.left_boundary)
        self.left_operator_stack.append(self.operator_site_tensor)

    def push_right_boundary(self):
        self.right_boundary_stack.append(self.right_boundary)
        self.right_operator_stack.append(self.operator_site_tensor)
    #@-node:gcross.20090930134608.1263:push_XXX_boundary
    #@+node:gcross.20090930134608.1267:pop_XXX_boundary
    def pop_left_boundary(self):
        self.left_boundary = self.left_boundary_stack.pop()
        self.operator_site_tensor = self.left_operator_stack.pop()

    def pop_right_boundary(self):
        self.right_boundary = self.right_boundary_stack.pop()
        self.operator_site_tensor = self.right_operator_stack.pop()
    #@-node:gcross.20090930134608.1267:pop_XXX_boundary
    #@+node:gcross.20090930134608.1270:contract_and_move_XXX
    def contract_and_move_left(self,state_site_tensor_conjugated,state_site_tensor):
        self.current_site_number -= 1
        self.push_right_boundary()
        self.right_boundary = self.contract_into_right_boundary(state_site_tensor_conjugated,state_site_tensor)
        self.pop_left_boundary()

    def contract_and_move_right(self,state_site_tensor_conjugated,state_site_tensor):
        self.current_site_number += 1
        self.push_left_boundary()
        self.left_boundary = self.contract_into_left_boundary(state_site_tensor_conjugated,state_site_tensor)
        self.pop_right_boundary()
    #@-node:gcross.20090930134608.1270:contract_and_move_XXX
    #@+node:gcross.20090930134608.1274:contract_into_XXX_boundary
    def contract_into_left_boundary(self,state_site_tensor_conjugated,state_site_tensor):
        return \
            contract_site_expectation_into_left_boundary(
                self.left_boundary,
                state_site_tensor_conjugated,
                self.operator_site_tensor,
                state_site_tensor
            )

    def contract_into_right_boundary(self,state_site_tensor_conjugated,state_site_tensor):
        return \
            contract_site_expectation_into_right_boundary(
                self.right_boundary,
                state_site_tensor_conjugated,
                self.operator_site_tensor,
                state_site_tensor
            )
    #@-node:gcross.20090930134608.1274:contract_into_XXX_boundary
    #@+node:gcross.20090930134608.1331:fully_contract_with_state_site_tensor
    def fully_contract_with_state_site_tensor(self,state_site_tensor_conjugated,state_site_tensor):
        return inner(self.partially_contract_with_state_site_tensor(state_site_tensor).ravel(),state_site_tensor_conjugated.ravel())
    #@-node:gcross.20090930134608.1331:fully_contract_with_state_site_tensor
    #@+node:gcross.20090930134608.1338:partially_contract_with_state_site_tensor
    def partially_contract_with_state_site_tensor(self,state_site_tensor):
        return \
            partially_contract_expectation(
                state_site_tensor,
                self.operator_site_tensor,
                self.left_boundary,
                self.right_boundary
            )

    #@-node:gcross.20090930134608.1338:partially_contract_with_state_site_tensor
    #@-others
#@-node:gcross.20090930124443.1295:ChainContractorForExpectations
#@+node:gcross.20090930134608.1292:ChainContractorForOverlaps
class ChainContractorForOverlaps(ChainContractorForExpectations):
    #@    @+others
    #@+node:gcross.20090930134608.1294:__init__
    def __init__(self,initial_left_boundary,initial_right_boundary,initial_state_A_site_tensors,initial_state_B_site_tensors):
        self.initialize(initial_left_boundary,initial_right_boundary,len(initial_state_A_site_tensors))
        self.operator_site_tensor = None
        self.right_operator_stack = [None]*(self.number_of_sites-1)
        for i in xrange(self.number_of_sites-1,0,-1):
            self.push_right_boundary()
            self.right_boundary = self.contract_into_right_boundary(initial_state_A_site_tensors[i].conj(),initial_state_B_site_tensors[i])
    #@-node:gcross.20090930134608.1294:__init__
    #@+node:gcross.20090930134608.1291:contract_into_XXX_boundary
    def contract_into_left_boundary(self,state_A_site_tensor_conjugated,state_B_site_tensor):
        return \
            contract_site_overlap_into_left_boundary(
                self.left_boundary,
                state_A_site_tensor_conjugated,
                state_B_site_tensor
            )

    def contract_into_right_boundary(self,state_A_site_tensor_conjugated,state_B_site_tensor):
        return \
            contract_site_overlap_into_right_boundary(
                self.right_boundary,
                state_A_site_tensor_conjugated,
                state_B_site_tensor
            )
    #@-node:gcross.20090930134608.1291:contract_into_XXX_boundary
    #@+node:gcross.20090930134608.1340:partially_contract_with_state_site_tensor
    def partially_contract_with_state_site_tensor(self,state_site_tensor):
        return \
            partially_contract_expectation(
                state_site_tensor,
                self.left_boundary,
                self.right_boundary
            )
    #@-node:gcross.20090930134608.1340:partially_contract_with_state_site_tensor
    #@-others
#@-node:gcross.20090930134608.1292:ChainContractorForOverlaps
#@-others
#@-node:gcross.20090930124443.1293:Classes
#@+node:gcross.20090930134608.1315:Unit Tests
if __name__ == '__main__':
    import unittest
    from utils import crand, normalize
    from paulis import *
    from numpy import identity, allclose, array, zeros
    from paycheck import *
    #@    @+others
    #@+node:gcross.20090930134608.1326:ChainContractorForExpectationsTests
    class ChainContractorForExpectationsTests(unittest.TestCase):
        #@    @+others
        #@+node:gcross.20090930134608.1327:testOnNormalizedState
        @with_checker
        def testOnNormalizedState(self,physical_dimension=irange(2,4),bandwidth_dimension=irange(2,4),number_of_sites=irange(2,6)):
            initial_left_boundary = initial_right_boundary = identity(bandwidth_dimension).reshape(bandwidth_dimension,1,bandwidth_dimension)
            initial_site_tensors = [normalize(crand(physical_dimension,bandwidth_dimension,bandwidth_dimension),1) for _ in xrange(number_of_sites)]
            operator_site_tensors = [identity(physical_dimension).reshape(physical_dimension,physical_dimension,1,1)]*number_of_sites
            chain = ChainContractorForExpectations(initial_left_boundary,initial_right_boundary,initial_site_tensors,operator_site_tensors)
            self.assertTrue(allclose(identity(bandwidth_dimension),chain.left_boundary.squeeze()))
            self.assertTrue(allclose(identity(bandwidth_dimension),chain.right_boundary.squeeze()))

            for initial_site_tensor in initial_site_tensors[:-1]:
                initial_site_tensor = normalize(initial_site_tensor,2)
                chain.contract_and_move_right(initial_site_tensor.conj(),initial_site_tensor)
                self.assertTrue(allclose(identity(bandwidth_dimension),chain.left_boundary.squeeze()))
                self.assertTrue(allclose(identity(bandwidth_dimension),chain.right_boundary.squeeze()))

            for initial_site_tensor in initial_site_tensors[-1:0:-1]:
                chain.contract_and_move_left(initial_site_tensor.conj(),initial_site_tensor)
                self.assertTrue(allclose(identity(bandwidth_dimension),chain.left_boundary.squeeze()))
                self.assertTrue(allclose(identity(bandwidth_dimension),chain.right_boundary.squeeze()))
        #@-node:gcross.20090930134608.1327:testOnNormalizedState
        #@+node:gcross.20090930134608.1329:testOnSpinStateInMagneticXField
        @with_checker
        def testOnSpinStateInMagneticXField(self,number_of_sites=irange(2,6)):
            initial_left_boundary = array([1,0]).reshape(1,2,1)
            initial_right_boundary = array([0,1]).reshape(1,2,1)
            initial_site_tensor = array([1,0]).reshape(2,1,1)
            initial_site_tensors = [initial_site_tensor]*number_of_sites
            operator_site_tensor = zeros((2,2,2,2),dtype=int)
            operator_site_tensor[...,0,0] = I
            operator_site_tensor[...,0,1] = X
            operator_site_tensor[...,1,1] = I
            operator_site_tensors = [operator_site_tensor]*number_of_sites
            chain = ChainContractorForExpectations(initial_left_boundary,initial_right_boundary,initial_site_tensors,operator_site_tensors) 
            self.assertEqual(0,chain.fully_contract_with_state_site_tensor(initial_site_tensor,initial_site_tensor))

            for i in xrange(number_of_sites-1):
                chain.contract_and_move_right(initial_site_tensor,initial_site_tensor)
                self.assertEqual(0,chain.fully_contract_with_state_site_tensor(initial_site_tensor,initial_site_tensor))

            for i in xrange(number_of_sites-1):
                chain.contract_and_move_left(initial_site_tensor,initial_site_tensor)
                self.assertEqual(0,chain.fully_contract_with_state_site_tensor(initial_site_tensor,initial_site_tensor))
        #@-node:gcross.20090930134608.1329:testOnSpinStateInMagneticXField
        #@+node:gcross.20090930134608.1333:testOnSpinStateInMagneticZField
        @with_checker
        def testOnSpinStateInMagneticZField(self,number_of_sites=irange(2,6)):
            initial_left_boundary = array([1,0]).reshape(1,2,1)
            initial_right_boundary = array([0,1]).reshape(1,2,1)
            initial_site_tensor = array([1,0]).reshape(2,1,1)
            initial_site_tensors = [initial_site_tensor]*number_of_sites
            operator_site_tensor = zeros((2,2,2,2),dtype=int)
            operator_site_tensor[...,0,0] = I
            operator_site_tensor[...,0,1] = Z
            operator_site_tensor[...,1,1] = I
            operator_site_tensors = [operator_site_tensor]*number_of_sites
            chain = ChainContractorForExpectations(initial_left_boundary,initial_right_boundary,initial_site_tensors,operator_site_tensors) 
            self.assertEqual(number_of_sites,chain.fully_contract_with_state_site_tensor(initial_site_tensor,initial_site_tensor))

            for i in xrange(number_of_sites-1):
                chain.contract_and_move_right(initial_site_tensor,initial_site_tensor)
                self.assertEqual(number_of_sites,chain.fully_contract_with_state_site_tensor(initial_site_tensor,initial_site_tensor))

            for i in xrange(number_of_sites-1):
                chain.contract_and_move_left(initial_site_tensor,initial_site_tensor)
                self.assertEqual(number_of_sites,chain.fully_contract_with_state_site_tensor(initial_site_tensor,initial_site_tensor))
        #@-node:gcross.20090930134608.1333:testOnSpinStateInMagneticZField
        #@-others
    #@nonl
    #@-node:gcross.20090930134608.1326:ChainContractorForExpectationsTests
    #@+node:gcross.20090930134608.1316:ChainContractorForOverlapsTests
    class ChainContractorForOverlapsTests(unittest.TestCase):
        #@    @+others
        #@+node:gcross.20090930134608.1317:testOnNormalizedState
        @with_checker
        def testOnNormalizedState(self,physical_dimension=irange(2,4),bandwidth_dimension=irange(2,4),number_of_sites=irange(2,6)):
            initial_left_boundary = initial_right_boundary = identity(bandwidth_dimension)
            initial_site_tensors = [normalize(crand(physical_dimension,bandwidth_dimension,bandwidth_dimension),1) for _ in xrange(number_of_sites)]
            chain = ChainContractorForOverlaps(initial_left_boundary,initial_right_boundary,initial_site_tensors,initial_site_tensors)
            self.assertTrue(allclose(identity(bandwidth_dimension),chain.left_boundary))
            self.assertTrue(allclose(identity(bandwidth_dimension),chain.right_boundary))

            for initial_site_tensor in initial_site_tensors[:-1]:
                initial_site_tensor = normalize(initial_site_tensor,2)
                chain.contract_and_move_right(initial_site_tensor.conj(),initial_site_tensor)
                self.assertTrue(allclose(identity(bandwidth_dimension),chain.left_boundary))
                self.assertTrue(allclose(identity(bandwidth_dimension),chain.right_boundary))

            for initial_site_tensor in initial_site_tensors[-1:0:-1]:
                chain.contract_and_move_left(initial_site_tensor.conj(),initial_site_tensor)
                self.assertTrue(allclose(identity(bandwidth_dimension),chain.left_boundary))
                self.assertTrue(allclose(identity(bandwidth_dimension),chain.right_boundary))
        #@-node:gcross.20090930134608.1317:testOnNormalizedState
        #@-others
    #@nonl
    #@-node:gcross.20090930134608.1316:ChainContractorForOverlapsTests
    #@-others
    unittest.main()
#@-node:gcross.20090930134608.1315:Unit Tests
#@-others
#@-node:gcross.20090930124443.1284:@thin chain_contractors.py
#@-leo
