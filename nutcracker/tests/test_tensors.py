#@+leo-ver=5-thin
#@+node:gcross.20111107131531.1333: * @file test_tensors.py
#@+<< Imports >>
#@+node:gcross.20111107131531.1334: ** << Imports >>
from paycheck import *

from nutcracker.tensors import *
from nutcracker.utils import *

from nutcracker.tests import *
#@-<< Imports >>

#@+others
#@+node:gcross.20111107131531.1347: ** Tests
#@+node:gcross.20111107131531.1348: *3* TestLeftExpectationBoundary
class TestLeftExpectationBoundary(TestCase):
    #@+others
    #@+node:gcross.20111107131531.1349: *4* absorb
    @with_checker
    def test_absorb(self,
        physical_dimension=irange(1,4),
        left_state_dimension=irange(1,4),
        right_state_dimension=irange(1,4),
        left_operator_dimension=irange(1,4),
        right_operator_dimension=irange(1,4),
    ):
        L = LeftExpectationBoundary(
            state_dimension = left_state_dimension,
            operator_dimension = left_operator_dimension,
            randomize = True
        )
        O = OperatorSite.random(
            physical_dimension = physical_dimension,
            left_dimension = left_operator_dimension,
            right_dimension = right_operator_dimension,
        )
        S = StateSite(
            physical_dimension = physical_dimension,
            left_dimension = left_state_dimension,
            right_dimension = right_state_dimension,
            randomize = True
        )
        self.assertAllClose(
            L.absorb(O,S).data,
            self.test_absorb.contract(L.data,O.formDenseTensor(),S.data,S.data.conj())
        )

    test_absorb.contract = formContractor(
        ['L','O','S','S*'],
        [(('L',LeftExpectationBoundary.operator_index),('O',OperatorSite.left_index))
        ,(('L',LeftExpectationBoundary.state_index),('S',StateSite.left_index))
        ,(('L',LeftExpectationBoundary.state_conjugate_index),('S*',StateSite.left_index))
        ,(('O',OperatorSite.physical_index),('S',StateSite.physical_index))
        ,(('O',OperatorSite.physical_conjugate_index),('S*',StateSite.physical_index))
        ],
        [{'state':[('S',StateSite.right_index)]
         ,'state_conjugate':[('S*',StateSite.right_index)]
         ,'operator':[('O',OperatorSite.right_index)]
         }[name] for name in LeftExpectationBoundary._dimensions
        ]
    )
    #@-others
#@-others
#@-leo
