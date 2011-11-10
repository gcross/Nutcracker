#@+leo-ver=5-thin
#@+node:gcross.20111109104457.1804: * @file test_1d_infinite_chain.py
#@+<< Imports >>
#@+node:gcross.20111109104457.1805: ** << Imports >>
from . import *
from .._1d.enumerations import *
from .._1d.infinite.chain import *
from .._1d.tensors import *
#@-<< Imports >>

#@+others
#@+node:gcross.20111109104457.1811: ** Functions
#@+node:gcross.20111109104457.1812: *3* randomChain
def randomChain(cls=Chain):
    return cls.random(
        randint(2,4),
        randint(2,4),
        randint(2,4),
    )
#@+node:gcross.20111109104457.1806: ** Tests
#@+node:gcross.20111109104457.1807: *3* Chain
class TestChain(TestCase):
    #@+others
    #@+node:gcross.20111109104457.1808: *4* test_normalizeAndContract
    @prependContractor(
        ['L','R','S1','S1*','S2','S2*','O1','O2'],
        [(('L',LeftExpectationBoundary.state_index),('S1',StateSite.left_index))
        ,(('S1',StateSite.right_index),('S2',StateSite.left_index))
        ,(('S2',StateSite.right_index),('R',RightExpectationBoundary.state_index))
        ,(('L',LeftExpectationBoundary.state_conjugate_index),('S1*',StateSite.left_index))
        ,(('S1*',StateSite.right_index),('S2*',StateSite.left_index))
        ,(('S2*',StateSite.right_index),('R',RightExpectationBoundary.state_conjugate_index))
        ,(('L',LeftExpectationBoundary.operator_index),('O1',OperatorSite.left_index))
        ,(('O1',OperatorSite.right_index),('O2',OperatorSite.left_index))
        ,(('O2',OperatorSite.right_index),('R',RightExpectationBoundary.operator_index))
        ,(('O1',OperatorSite.physical_index),('S1',StateSite.physical_index))
        ,(('O2',OperatorSite.physical_index),('S2',StateSite.physical_index))
        ,(('O1',OperatorSite.physical_conjugate_index),('S1*',StateSite.physical_index))
        ,(('O2',OperatorSite.physical_conjugate_index),('S2*',StateSite.physical_index))
        ],
        []
    )
    @with_checker(number_of_calls=10)
    def test_normalizeAndContract(contract,self,direction=irange(0,1)):
        direction = Direction.values()[direction]
        chain = randomChain()
        operator_site_data = chain.operator_site.formDenseTensor().data
        correct_expectation = contract(
            chain.left_environment.data,
            chain.right_environment.data,
            chain.state_site.data,
            chain.state_site.data.conj(),
            chain.state_site.data,
            chain.state_site.data.conj(),
            operator_site_data,
            operator_site_data,
        )
        chain.normalizeAndContract(direction)
        self.assertAlmostEqual(chain.computeExpectation(),correct_expectation)
    #@-others
#@-others
#@-leo
