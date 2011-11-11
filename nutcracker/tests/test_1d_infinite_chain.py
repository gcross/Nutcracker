#@+leo-ver=5-thin
#@+node:gcross.20111109104457.1804: * @file test_1d_infinite_chain.py
#@+<< Imports >>
#@+node:gcross.20111109104457.1805: ** << Imports >>
from . import *
from .._1d.enumerations import *
from .._1d.infinite.chains import *
from .._1d.infinite.environment import *
from .._1d.tensors import *
from ..qubit import *
#@-<< Imports >>

#@+others
#@+node:gcross.20111109104457.1811: ** Functions
#@+node:gcross.20111109104457.1812: *3* randomChain
def randomChain(cls=InfiniteEnvironment):
    return cls.random(
        randint(2,4),
        randint(2,4),
        randint(2,4),
    )
#@+node:gcross.20111109104457.1806: ** Tests
#@+node:gcross.20111110144828.1736: *3* ExpectationBehaviour
class TestExpectationBehaviour(TestCase):
    #@+others
    #@+node:gcross.20111110150346.1736: *4* test_qubit_magnetic_field_on_random_product_state
    @with_checker
    def test_qubit_magnetic_field_on_random_product_state(self,
        contractions = [(Qubit,Direction)],
    ):
        environment = InfiniteEnvironment(OperatorChain.buildMagneticField(Pauli.Z))
        self.assertEqual(environment.computeExpectation(),1)
        correct_expectation = 1
        for qubit, direction in contractions:
            environment.normalizeAndContract(direction)
            environment.state_site = StateSite.simple(qubit)
            if qubit is Qubit.up:
                correct_expectation += 1
            else:
                correct_expectation -= 1
        self.assertEqual(environment.computeExpectation(),correct_expectation)
    #@+node:gcross.20111110233742.1823: *4* test_qubit_magnetic_field_on_W_state
    @with_checker
    def test_qubit_magnetic_field_on_W_state(self,
        contractions = [Direction],
    ):
        environment = InfiniteEnvironment(OperatorChain.buildMagneticField(Pauli.Z))
        for direction in contractions:
            environment.normalizeAndContract(direction)
        self.assertEqual(environment.computeExpectation(),len(contractions)+1)
    #@+node:gcross.20111110233742.1816: *4* test_qubit_spin_coupling_field_on_all_up_state
    @with_checker
    def test_qubit_spin_coupling_field_on_all_up_state(self,
        contractions = [Direction],
    ):
        environment = InfiniteEnvironment(
            operator_chain = OperatorChain.buildNearestNeighborSpinCouplingField(Pauli.Z),
            state_chain = StateChain.simple(Qubit.up),
        )
        for direction in contractions:
            environment.normalizeAndContract(direction)
        self.assertEqual(environment.computeExpectation(),-len(contractions))
    #@+node:gcross.20111110233742.1818: *4* test_qubit_spin_coupling_field_on_W_state
    @with_checker
    def test_qubit_spin_coupling_field_on_W_state(self,
        contractions = [Direction],
    ):
        environment = InfiniteEnvironment(
            operator_chain = OperatorChain.buildNearestNeighborSpinCouplingField(Pauli.Z),
            state_chain = StateChain.buildWState(),
        )
        for direction in contractions:
            environment.dangerouslyContractWithoutNormalizing(direction)
        self.assertEqual(environment.computeExpectation(),-(len(contractions)-1)*(len(contractions)-2)+2)
    #@+node:gcross.20111110233742.1821: *4* test_W_state_normalization
    @with_checker
    def test_W_state_normalization(self,
        contractions = [Direction],
    ):
        environment = InfiniteEnvironment(
            operator_chain = OperatorChain.simple(Pauli.I),
            state_chain = StateChain.buildWState(),
        )
        for direction in contractions:
            environment.dangerouslyContractWithoutNormalizing(direction)
        self.assertEqual(environment.computeExpectation(),len(contractions)+1)
    #@-others
#@+node:gcross.20111109104457.1807: *3* InfiniteEnvironment
class TestInfiniteEnvironment(TestCase):
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
    def test_normalizeAndContract(contract,self,direction=Direction):
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
